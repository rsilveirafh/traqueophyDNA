# BOLD Systems (portal v5) -------------------------------------------------------
#
# Uma chamada por classe devolve o número de registros públicos por nome de
# espécie, sem chave:
#   GET https://portal.boldsystems.org/api/summary?query=tax:class:Amphibia&fields=species&reduce=false
# Com `;geo:country/ocean:Brazil` na query, só material coletado no Brasil.
# A API está na versão "0.1.0": a chamada fica isolada aqui e, se falhar ou
# mudar de formato, o pipeline reaproveita o último resultado bom.

BOLD_URL <- "https://portal.boldsystems.org/api/summary"

bold_resumo <- function(classe, brasil = FALSE) {
  q <- paste0("tax:class:", classe, if (brasil) ";geo:country/ocean:Brazil")
  resp <- req_base(BOLD_URL) |>
    req_url_query(query = q, fields = "species", reduce = "false") |>
    req_perform()
  sp <- resp_body_json(resp)$species
  if (is.null(sp)) stop("formato inesperado na resposta do BOLD (sem campo 'species')")
  tibble(nome = names(sp), n = as.integer(unlist(sp)))
}

# Baixa global + Brasil para cada classe; grava data/cache/bold_<classe>.csv.
# Devolve tabela longa: classe, nome, n_bold, n_bold_br.
atualiza_bold <- function(classes, pasta = "data/cache") {
  purrr::map(classes, function(cl) {
    arq <- file.path(pasta, paste0("bold_", tolower(cl), ".csv"))
    msg("BOLD: ", cl)
    res <- tryCatch({
      g <- bold_resumo(cl)
      Sys.sleep(1)
      b <- bold_resumo(cl, brasil = TRUE)
      Sys.sleep(1)
      out <- full_join(rename(g, n_bold = n), rename(b, n_bold_br = n), by = "nome") |>
        mutate(classe = cl, n_bold = coalesce(n_bold, 0L), n_bold_br = coalesce(n_bold_br, 0L)) |>
        select(classe, nome, n_bold, n_bold_br) |>
        arrange(nome)
      if (nrow(out) < 10) stop("resposta suspeita (", nrow(out), " nomes)")
      gravar_csv(out, arq)
      out
    }, error = function(e) {
      msg("  aviso: BOLD falhou para ", cl, ": ", conditionMessage(e))
      ant <- ler_csv(arq)
      if (is.null(ant)) stop("BOLD indisponível e sem cache para ", cl)
      attr(ant, "aviso") <- TRUE
      ant
    })
    res
  }) |> bind_rows()
}

# Soma os registros do BOLD de cada espécie sob o nome da lista e sob os
# nomes alternativos (sinônimos/nome aceito no CTFB). Também conta registros
# do gênero coletados no Brasil sem identificação ("Gênero sp.", "cf.", "aff.").
cobre_bold <- function(especies, bold) {
  idx <- bold |> select(classe, nome, n_bold, n_bold_br)
  nomes <- especies |>
    select(classe, especie, sinonimos) |>
    mutate(nome = map2(especie, sinonimos, function(e, s) unique(c(e, if (!is.na(s)) strsplit(s, ";")[[1]])))) |>
    unnest(nome) |>
    left_join(idx, by = c("classe", "nome")) |>
    mutate(across(c(n_bold, n_bold_br), ~ coalesce(.x, 0L)))
  por_sp <- nomes |>
    group_by(classe, especie) |>
    summarise(
      n_bold_nome = sum(n_bold[nome == especie]),
      n_bold_comb = sum(n_bold[nome == especie | nome %in% mesmo_epiteto(first(especie), nome)]),
      n_bold = sum(n_bold), n_bold_br = sum(n_bold_br),
      bold_sinonimos_com_dado = paste(nome[nome != especie & n_bold > 0], collapse = ";"),
      .groups = "drop"
    ) |>
    mutate(bold_sinonimos_com_dado = na_if(bold_sinonimos_com_dado, ""))

  sem_id <- bold |>
    filter(n_bold_br > 0, str_detect(nome, " (sp\\.|cf\\.|aff\\.|nr\\.)")) |>
    mutate(genero = word(nome, 1)) |>
    group_by(classe, genero) |>
    summarise(bold_br_genero_sem_id = sum(n_bold_br), .groups = "drop")

  especies |>
    select(classe, especie, genero) |>
    left_join(por_sp, by = c("classe", "especie")) |>
    left_join(sem_id, by = c("classe", "genero")) |>
    mutate(bold_br_genero_sem_id = coalesce(bold_br_genero_sem_id, 0L)) |>
    select(-genero)
}
