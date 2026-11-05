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

# `nomes_ncbi` (especie, nome): só os nomes que existem na taxonomia do NCBI entram
# (o SRA usa a mesma taxonomia); a espécie sem nenhum fica com 0 sem consulta.
atualiza_sra <- function(especies, nomes_ncbi, destino = "data/sra.csv", dias_validade = 28, max_minutos = Inf) {
  conhecidos <- split(nomes_ncbi$nome, nomes_ncbi$especie)
  alvo <- especies |>
    mutate(termo = map2_chr(especie, sinonimos, function(e, s) {
      n <- intersect(unique(c(e, mesmo_epiteto(e, if (is.na(s)) character() else strsplit(s, ";")[[1]]))),
                     conhecidos[[e]])
      if (length(n)) termo_ncbi(n) else NA_character_
    })) |>
    select(especie, termo)
  fora <- filter(alvo, is.na(termo)) |>
    mutate(termo = "(nenhum nome na taxonomia do NCBI)", n_sra = 0L, consultado_em = hoje())
  alvo <- filter(alvo, !is.na(termo))
  cache <- ler_csv(destino, col_types = cols(n_sra = col_integer(), .default = col_character()))
  if (is.null(cache)) cache <- tibble(especie = character(), termo = character(), n_sra = integer(),
                                       consultado_em = character())
  validos <- cache |>
    filter(!is.na(n_sra), as.numeric(as.Date(hoje()) - as.Date(consultado_em)) < dias_validade) |>
    semi_join(alvo, by = c("especie", "termo"))
  falta <- anti_join(alvo, validos, by = "especie")
  msg("SRA (dados brutos): ", nrow(falta), " consultas")
  inicio <- Sys.time()
  novos <- list()
  blocos <- split(seq_len(nrow(falta)), ceiling(seq_len(nrow(falta)) / 500))
  for (b in seq_along(blocos)) {
    f <- falta[blocos[[b]], ]
    novos[[b]] <- tibble(especie = f$especie, termo = f$termo,
                         n_sra = ncbi_conta_lote(f$termo, db = "sra"), consultado_em = hoje())
    msg("  ", max(blocos[[b]]), "/", nrow(falta))
    if (as.numeric(difftime(Sys.time(), inicio, units = "mins")) > max_minutos) break
  }
  novos <- bind_rows(novos)
  if (nrow(novos)) novos <- filter(novos, !is.na(n_sra))   # falha volta na próxima
  out <- bind_rows(validos, novos, fora) |> arrange(especie)
  gravar_csv(out, destino)
  out
}
