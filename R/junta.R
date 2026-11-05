# Junta lista + WCVP + BOLD + GenBank (+ SRA) em data/cobertura.csv -------------

# Gênero com pelo menos este número de registros no GenBank: vira nota na
# observação ("vale conferir o nome"), mas não muda a situação. No tetrapoDNA era
# motivo de "a verificar"; a validação mostrou que quase todas eram sem DNA mesmo.
LIMIAR_GENERO <- 500

# Até quantos nomes alternativos a observação lista um a um.
MAX_NOMES_OBS <- 8

faixa_gaplist <- function(n) {
  # Mesmas faixas do iBOL Europe Gap List (prioridade por nº de registros no BOLD).
  cut(n, c(-Inf, 0, 2, 9, Inf), labels = c("0", "1-2", "3-9", "10+")) |> as.character()
}

junta_obs <- function(a, b) ifelse(is.na(b), a, ifelse(is.na(a), b, paste0(a, "; ", b)))

monta_cobertura <- function(especies, bold_cob, genbank, generos, sra = NULL, nomes_ncbi = NULL) {
  x <- especies |>
    left_join(bold_cob, by = "especie") |>
    left_join(select(genbank, especie, n_genbank, n_genbank_nome, n_genbank_comb,
                     any_of(c("n_genbank_texto", "n_genbank_ffb")), genbank_consultado_em = consultado_em),
              by = "especie") |>
    left_join(select(generos, genero, n_genbank_genero), by = "genero")
  x <- if (is.null(sra)) mutate(x, n_sra = NA_integer_) else left_join(x, select(sra, especie, n_sra), by = "especie")
  if (!"n_genbank_texto" %in% names(x)) x$n_genbank_texto <- NA_integer_
  if (!"n_genbank_ffb" %in% names(x)) x$n_genbank_ffb <- x$n_genbank_comb
  if (!"n_bold_ffb" %in% names(x)) x$n_bold_ffb <- x$n_bold_comb
  if (!"wcvp_junta_com" %in% names(x)) x$wcvp_junta_com <- NA_character_
  if (!"sinonimos_ffb" %in% names(x)) x$sinonimos_ffb <- NA_character_
  x$no_ncbi <- if (is.null(nomes_ncbi)) NA else x$especie %in% nomes_ncbi$especie
  x$n_sinonimos <- ifelse(is.na(x$sinonimos), 0L, lengths(strsplit(x$sinonimos, ";")))

  x |>
    mutate(
      # com DNA = sob o nome da lista, outra combinação do mesmo epíteto (inclusive
      # os infraespecíficos) ou um sinônimo da Flora e Funga (validação, 2026-11-05)
      dna_comb = coalesce(n_bold_comb, 0L) > 0 | coalesce(n_genbank_comb, 0L) > 0,
      dna_nome = dna_comb | coalesce(n_bold_ffb, 0L) > 0 | coalesce(n_genbank_ffb, 0L) > 0,
      so_comb = dna_comb & !(coalesce(n_bold_nome, 0L) > 0 | coalesce(n_genbank_nome, 0L) > 0),
      so_ffb = dna_nome & !dna_comb,
      dna_qualquer = coalesce(n_bold, 0L) > 0 | coalesce(n_genbank, 0L) > 0,
      motivos = pmap_chr(
        list(dna_nome, dna_qualquer, wcvp_situacao, bold_sinonimos_com_dado, n_genbank),
        function(dn, dq, ws, bs, ng) {
          if (isTRUE(dn)) return(NA_character_)
          m <- character()
          # nomes só do WCVP: sinônimo global, às vezes um conceito mais amplo
          if (isTRUE(dq)) m <- c(m, paste0("há sequências só sob sinônimo do WCVP (pode ser um conceito mais amplo da espécie)",
                                           if (!is.na(bs)) paste0(" (BOLD: ", gsub(";", ", ", bs), ")")))
          if (is.na(ng)) m <- c(m, "GenBank ainda não consultado")
          # Ter sinônimos, ou ser sinônimo no WCVP, não é motivo: todos os nomes
          # (da Flora e Funga e do WCVP, inclusive o aceito no WCVP) foram buscados.
          # Nome ausente do WCVP também não: sem nenhuma sequência, é nota (na
          # validação, as 17 assim eram todas sem DNA).
          if (length(m)) paste(m, collapse = "; ") else NA_character_
        }),
      # Notas que não mudam a situação, mas ajudam quem for conferir.
      nota_sinonimos = case_when(
        dna_qualquer | n_sinonimos == 0 ~ NA_character_,
        n_sinonimos <= MAX_NOMES_OBS ~ paste0("nomes alternativos buscados: ", gsub(";", ", ", sinonimos)),
        TRUE ~ paste0(n_sinonimos, " nomes alternativos buscados (Flora e Funga e WCVP)")
      ),
      nota_wcvp = case_when(
        dna_nome ~ NA_character_,
        !is.na(wcvp_junta_com) ~ paste0("o WCVP trata como sinônimo de ", wcvp_junta_com,
                                        ", aceita na Flora e Funga: os nomes do WCVP não foram buscados"),
        wcvp_situacao %in% "sinonimo" ~ "o nome da lista é sinônimo no WCVP (o nome aceito lá também foi buscado)",
        wcvp_situacao %in% "ausente" ~ "nome não encontrado no WCVP (só os sinônimos da Flora e Funga foram buscados)",
        TRUE ~ NA_character_
      ),
      # citado fora do campo Organism: em geral como hospedeiro (fungo, bactéria, vírus)
      nota_texto = ifelse(!dna_nome & coalesce(n_genbank_texto, 0L) > 0,
                          paste0("o nome aparece em ", n_genbank_texto,
                                 " registro(s) do GenBank fora do campo Organism (em geral como hospedeiro)"),
                          NA_character_),
      nota_ncbi = ifelse(!dna_nome & no_ncbi %in% FALSE,
                         "nenhum dos nomes está na taxonomia do NCBI", NA_character_),
      nota_genero = ifelse(!dna_nome & coalesce(n_genbank_genero, 0L) >= LIMIAR_GENERO,
                           paste0("o gênero tem ", format(n_genbank_genero, big.mark = ".", decimal.mark = ","),
                                  " registros no GenBank"), NA_character_),
      # Leituras brutas no SRA: marca a espécie sem sequência montada; não muda a situação.
      dados_brutos = !dna_nome & coalesce(n_sra, 0L) > 0,
      nota_sra = ifelse(dados_brutos,
                        paste0("só dados brutos: ", n_sra, " registro(s) de leituras no SRA, nenhuma sequência montada no BOLD ou no GenBank"),
                        NA_character_),
      nota_bold = ifelse(!dna_nome & coalesce(bold_br_genero_sem_id, 0L) > 0,
                         paste0(bold_br_genero_sem_id, " registro(s) do gênero coletados no Brasil sem identificação até espécie no BOLD"),
                         NA_character_),
      situacao = case_when(
        dna_nome ~ "com DNA",
        !is.na(motivos) ~ "a verificar",
        TRUE ~ "sem DNA"
      ),
      observacao = case_when(
        situacao == "sem DNA" ~ "nenhuma sequência depositada no BOLD ou no GenBank sob este nome e os sinônimos conhecidos",
        so_ffb ~ paste0("DNA só sob sinônimo da Flora e Funga (",
                        map_chr(sinonimos_ffb, ~ paste(head(strsplit(coalesce(.x, ""), ";")[[1]], MAX_NOMES_OBS), collapse = ", ")),
                        ")"),
        so_comb ~ paste0("DNA só sob outra combinação do mesmo epíteto ou nome infraespecífico (",
                         map2_chr(especie, sinonimos, ~ paste(head(mesmo_epiteto(.x, strsplit(coalesce(.y, ""), ";")[[1]]), MAX_NOMES_OBS), collapse = ", ")),
                         ")"),
        TRUE ~ motivos
      ),
      observacao = junta_obs(observacao, nota_sinonimos),
      observacao = junta_obs(observacao, nota_wcvp),
      observacao = junta_obs(observacao, nota_ncbi),
      observacao = junta_obs(observacao, nota_texto),
      observacao = junta_obs(observacao, nota_genero),
      observacao = junta_obs(observacao, nota_sra),
      observacao = junta_obs(observacao, nota_bold),
      faixa_bold = faixa_gaplist(coalesce(n_bold, 0L)),
      atualizado_em = hoje()
    ) |>
    # `sinonimos` fica em data/especies.csv e data/wcvp.csv: aqui pesaria no site.
    select(grupo, classe, ordem, familia, genero, especie, autoria, ano_descricao,
           endemica_br, categoria_ameaca, fonte_ameaca, ufs, uf_fonte, biomas, forma_vida, substrato,
           situacao, dados_brutos, observacao, n_bold, n_bold_br, n_bold_nome, n_genbank,
           n_genbank_nome, n_genbank_texto, n_sra, faixa_bold, n_genbank_genero, bold_br_genero_sem_id,
           n_sinonimos, wcvp_situacao, wcvp_junta_com, no_ncbi, id_ffb, nota_lista, versao_lista,
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
  ant <- ler_csv(destino, col_types = cols(data = col_character(), grupo = col_character(),
                                           .default = col_integer()))
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
    # só quem já tinha sido consultado no GenBank: "a verificar" por falta de
    # consulta não é lacuna fechada
    inner_join(cob_antiga |> filter(!is.na(n_genbank)) |> select(especie, de = situacao),
               by = "especie") |>
    filter(de != "com DNA", para == "com DNA") |>
    mutate(data = hoje(), .before = 1) |>
    select(data, grupo, especie, de, para, n_bold, n_genbank)
  if (nrow(ev)) msg("Eventos: ", nrow(ev), " espécie(s) ganharam DNA")
  out <- bind_rows(if (!is.null(ant)) mutate(ant, across(c(n_bold, n_genbank), as.integer)), ev) |>
    distinct()
  gravar_csv(out, destino)
}

# Versão enxuta de cobertura.csv para o site (~1/3 do tamanho): só as colunas que
# as páginas usam, e a observação padrão das "sem DNA" fica implícita.
OBS_SEM_DNA <- "nenhuma sequência depositada no BOLD ou no GenBank sob este nome e os sinônimos conhecidos"

grava_site <- function(cob, destino = "data/site.csv") {
  cob |>
    transmute(grupo, familia, ordem, especie, autoria, ano_descricao, endemica_br,
              categoria_ameaca, ufs, biomas, forma_vida, substrato, situacao, dados_brutos,
              n_bold, n_genbank, n_sra, id_ffb,
              observacao = na_if(str_remove(observacao, fixed(OBS_SEM_DNA)) |> str_remove("^; "), "")) |>
    gravar_csv(destino)
}
