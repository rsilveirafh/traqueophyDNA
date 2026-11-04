# NCBI SRA: dados brutos de sequenciamento das espécies sem DNA --------------------
#
# O foco do painel são sequências de DNA depositadas (qualquer locus, no BOLD ou no
# GenBank). O SRA guarda leituras brutas (UCE, RAD, RNA-Seq, genomas) que ninguém
# montou: não muda a situação da espécie, só ganha a marca "só dados brutos".
#
# Consulta só as espécies sem sequência sob o nome da lista ou outra combinação do
# mesmo epíteto, e busca no campo Organism só esses nomes: sinônimos com outro
# epíteto ficam de fora, para não contar dados de uma espécie-mãe desmembrada.
#   esearch.fcgi?db=sra&term=("Gênero epíteto"[Organism] OR ...)&rettype=count
# Cache incremental em data/sra.csv (validade de 28 dias).

atualiza_sra <- function(especies, destino = "data/sra.csv", dias_validade = 28, max_minutos = Inf) {
  alvo <- especies |>
    mutate(termo = map2_chr(especie, sinonimos, function(e, s) {
      termo_ncbi(unique(c(e, mesmo_epiteto(e, if (is.na(s)) character() else strsplit(s, ";")[[1]]))))
    })) |>
    select(especie, termo)
  cache <- ler_csv(destino, col_types = cols(n_sra = col_integer(), .default = col_character()))
  if (is.null(cache)) cache <- tibble(especie = character(), termo = character(), n_sra = integer(),
                                       consultado_em = character())
  validos <- cache |>
    filter(!is.na(n_sra), as.numeric(as.Date(hoje()) - as.Date(consultado_em)) < dias_validade) |>
    semi_join(alvo, by = c("especie", "termo"))
  falta <- anti_join(alvo, validos, by = "especie")
  msg("SRA (dados brutos): ", nrow(falta), " consultas")
  inicio <- Sys.time()
  novos <- vector("list", nrow(falta))
  for (i in seq_len(nrow(falta))) {
    novos[[i]] <- tibble(especie = falta$especie[i], termo = falta$termo[i],
                         n_sra = ncbi_conta(falta$termo[i], db = "sra"), consultado_em = hoje())
    if (i %% 200 == 0) msg("  ", i, "/", nrow(falta))
    if (as.numeric(difftime(Sys.time(), inicio, units = "mins")) > max_minutos) break
  }
  out <- bind_rows(validos, bind_rows(novos)) |> arrange(especie)
  gravar_csv(out, destino)
  out
}
