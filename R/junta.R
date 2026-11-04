# Junta lista + CTFB + BOLD + GenBank (+ SRA) em data/cobertura.csv -------------

# Gênero com pelo menos este número de registros no GenBank torna "a verificar"
# uma espécie que aparece sem DNA (o grupo é bem amostrado; vale conferir nome).
LIMIAR_GENERO <- 500

faixa_gaplist <- function(n) {
  # Mesmas faixas do iBOL Europe Gap List (prioridade por nº de registros no BOLD).
  cut(n, c(-Inf, 0, 2, 9, Inf), labels = c("0", "1-2", "3-9", "10+")) |> as.character()
}

monta_cobertura <- function(especies, bold_cob, genbank, generos, sra = NULL) {
  x <- especies |>
    left_join(bold_cob, by = c("classe", "especie")) |>
    left_join(select(genbank, especie, n_genbank, n_genbank_nome, n_genbank_comb,
                     any_of("n_genbank_texto"), genbank_consultado_em = consultado_em),
              by = "especie") |>
    left_join(select(generos, genero, n_genbank_genero), by = "genero")
  x <- if (is.null(sra)) mutate(x, n_sra = NA_integer_) else left_join(x, select(sra, especie, n_sra), by = "especie")
  if (!"gbif_situacao" %in% names(x)) x$gbif_situacao <- NA_character_
  if (!"n_genbank_texto" %in% names(x)) x$n_genbank_texto <- NA_integer_

  x |>
    mutate(
      # com DNA = sob o nome da lista ou sob outra combinação do mesmo epíteto
      dna_nome = n_bold_comb > 0 | coalesce(n_genbank_comb, 0L) > 0,
      so_comb = dna_nome & !(n_bold_nome > 0 | coalesce(n_genbank_nome, 0L) > 0),
      dna_qualquer = n_bold > 0 | coalesce(n_genbank, 0L) > 0,
      motivos = pmap_chr(
        list(dna_nome, dna_qualquer, ctfb_situacao, sinonimos, n_genbank_genero,
             bold_br_genero_sem_id, bold_sinonimos_com_dado, n_genbank, gbif_situacao,
             n_genbank_texto),
        function(dn, dq, cs, sn, gg, gs, bs, ng, gb, nt) {
          if (isTRUE(dn)) return(NA_character_)
          m <- character()
          if (isTRUE(nt > 0)) m <- c(m, paste0("o nome aparece em ", nt, " registro(s) do GenBank fora do campo Organism"))
          if (identical(gb, "nao_reconhecido")) m <- c(m, "nome não reconhecido no GBIF (sinônimos não conferidos)")
          if (isTRUE(dq)) m <- c(m, paste0("há sequências só sob nome alternativo",
                                           if (!is.na(bs)) paste0(" (BOLD: ", gsub(";", ", ", bs), ")")))
          if (is.na(ng)) m <- c(m, "GenBank ainda não consultado")
          if (cs == "ausente") m <- c(m, "nome não encontrado no CTFB (sinônimos não conferidos)")
          if (cs == "sinonimo") m <- c(m, "o nome da lista é sinônimo no CTFB")
          # Ter sinônimos não é motivo: eles já foram buscados (ver nota_sinonimos).
          if (isTRUE(gg >= LIMIAR_GENERO)) m <- c(m, paste0("o gênero tem ", format(gg, big.mark = ".", decimal.mark = ","), " registros no GenBank"))
          if (length(m)) paste(m, collapse = "; ") else NA_character_
        }),
      # Notas que não mudam a situação, mas ajudam quem for conferir.
      nota_sinonimos = ifelse(!dna_qualquer & !is.na(sinonimos),
                              paste0("sinônimos buscados: ", gsub(";", ", ", sinonimos)),
                              NA_character_),
      # Leituras brutas no SRA: marca a espécie sem sequência montada; não muda a situação.
      dados_brutos = !dna_nome & coalesce(n_sra, 0L) > 0,
      nota_sra = ifelse(dados_brutos,
                        paste0("só dados brutos: ", n_sra, " registro(s) de leituras no SRA, nenhuma sequência montada no BOLD ou no GenBank"),
                        NA_character_),
      nota_bold = ifelse(!dna_nome & bold_br_genero_sem_id > 0,
                         paste0(bold_br_genero_sem_id, " registro(s) do gênero coletados no Brasil sem identificação até espécie no BOLD"),
                         NA_character_),
      situacao = case_when(
        dna_nome ~ "com DNA",
        !is.na(motivos) ~ "a verificar",
        TRUE ~ "sem DNA"
      ),
      observacao = case_when(
        situacao == "sem DNA" ~ "nenhuma sequência depositada no BOLD ou no GenBank sob este nome e os sinônimos conhecidos",
        so_comb ~ paste0("DNA só sob outra combinação do mesmo epíteto (",
                         map2_chr(especie, sinonimos, ~ paste(mesmo_epiteto(.x, strsplit(coalesce(.y, ""), ";")[[1]]), collapse = ", ")),
                         ")"),
        TRUE ~ motivos
      ),
      observacao = ifelse(is.na(nota_sinonimos), observacao,
                          ifelse(is.na(observacao), nota_sinonimos, paste0(observacao, "; ", nota_sinonimos))),
      observacao = ifelse(is.na(nota_sra), observacao,
                          ifelse(is.na(observacao), nota_sra, paste0(observacao, "; ", nota_sra))),
      observacao = ifelse(is.na(nota_bold), observacao,
                          ifelse(is.na(observacao), nota_bold, paste0(observacao, "; ", nota_bold))),
      faixa_bold = faixa_gaplist(n_bold),
      atualizado_em = hoje()
    ) |>
    select(grupo, classe, ordem, familia, genero, especie, autoria, ano_descricao,
           endemica_br, categoria_ameaca, fonte_ameaca, ufs, uf_fonte, biomas,
           situacao, dados_brutos, observacao, n_bold, n_bold_br, n_bold_nome, n_genbank,
           n_genbank_nome, n_genbank_texto, n_sra, faixa_bold, n_genbank_genero, bold_br_genero_sem_id,
           sinonimos, ctfb_situacao, gbif_situacao, id_ctfb, nota_lista, versao_lista,
           genbank_consultado_em, atualizado_em) |>
    arrange(grupo, familia, especie)
}

# Uma linha por grupo (e o total) por data de execução.
atualiza_historico <- function(cob, destino = "data/historico.csv") {
  resumo <- function(d) summarise(d,
    n_especies = n(), com_dna = sum(situacao == "com DNA"),
    sem_dna = sum(situacao == "sem DNA"), a_verificar = sum(situacao == "a verificar"),
    so_dados_brutos = sum(coalesce(as.logical(dados_brutos), FALSE)),
    com_bold = sum(n_bold > 0), com_genbank = sum(coalesce(n_genbank, 0L) > 0),
    registros_bold = sum(n_bold), registros_genbank = sum(coalesce(n_genbank, 0L)))
  novo <- bind_rows(
    cob |> group_by(grupo) |> resumo(),
    cob |> resumo() |> mutate(grupo = "total")
  ) |> mutate(data = hoje(), .before = 1)
  ant <- ler_csv(destino, col_types = cols(data = col_character(), grupo = col_character()))
  out <- bind_rows(if (!is.null(ant)) filter(ant, data != hoje()), novo) |> arrange(data, grupo)
  gravar_csv(out, destino)
}

# Registra espécies que SAÍRAM da lista sem DNA (o "gol" do painel).
atualiza_eventos <- function(cob_nova, cob_antiga, destino = "data/eventos.csv") {
  ant <- ler_csv(destino, col_types = cols(.default = col_character()))
  if (is.null(cob_antiga)) {
    if (is.null(ant)) gravar_csv(tibble(data = character(), grupo = character(),
                                        especie = character(), de = character(),
                                        para = character(), n_bold = integer(),
                                        n_genbank = integer()), destino)
    return(invisible(NULL))
  }
  ev <- cob_nova |>
    select(grupo, especie, para = situacao, n_bold, n_genbank) |>
    inner_join(select(cob_antiga, especie, de = situacao), by = "especie") |>
    filter(de != "com DNA", para == "com DNA") |>
    mutate(data = hoje(), .before = 1) |>
    select(data, grupo, especie, de, para, n_bold, n_genbank)
  if (nrow(ev)) msg("Eventos: ", nrow(ev), " espécie(s) ganharam DNA")
  out <- bind_rows(if (!is.null(ant)) mutate(ant, across(c(n_bold, n_genbank), as.integer)), ev) |>
    distinct()
  gravar_csv(out, destino)
}
