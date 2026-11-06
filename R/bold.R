# BOLD Systems (portal v5) -------------------------------------------------------
#
# Uma chamada para o filo inteiro devolve o número de registros públicos por nome
# de espécie, sem chave (~115 mil nomes, poucos segundos):
#   GET https://portal.boldsystems.org/api/summary?query=tax:phylum:Tracheophyta&fields=species&reduce=false
# Com `;geo:country/ocean:Brazil` na query, só material coletado no Brasil.
# O BOLD não segue as classes da Flora e Funga (Gnetales ficam em Pinopsida):
# o casamento é só pelo nome.
# A API está na versão "0.1.0": a chamada fica isolada aqui e, se falhar ou
# mudar de formato, o pipeline reaproveita o último resultado bom.
# Limite do BOLD (suporte, 2026-11-06): abaixo de 20 consultas por minuto, senão o
# Cloudflare bloqueia o IP. Aqui: no máximo 1 a cada 4 s e sem novas tentativas
# (um 429 repetido piora o bloqueio); se falhar, vale o último resultado bom.

BOLD_URL <- "https://portal.boldsystems.org/api/summary"
BOLD_TAXON <- "tax:phylum:Tracheophyta"

bold_resumo <- function(consulta, brasil = FALSE) {
  q <- paste0(consulta, if (brasil) ";geo:country/ocean:Brazil")
  resp <- req_base(BOLD_URL, tentativas = 1) |>
    req_throttle(rate = 1 / 4, realm = "bold") |>
    req_url_query(query = q, fields = "species", reduce = "false") |>
    req_perform()
  sp <- resp_body_json(resp)$species
  if (is.null(sp)) stop("formato inesperado na resposta do BOLD (sem campo 'species')")
  tibble(nome = names(sp), n = as.integer(unlist(sp)))
}

# Baixa global + Brasil; grava data/cache/bold_tracheophyta.csv.
# Devolve: nome, n_bold, n_bold_br.
atualiza_bold <- function(consulta = BOLD_TAXON, pasta = "data/cache") {
  arq <- file.path(pasta, "bold_tracheophyta.csv")
  msg("BOLD: ", consulta)
  tryCatch({
    g <- bold_resumo(consulta)
    Sys.sleep(1)
    b <- bold_resumo(consulta, brasil = TRUE)
    out <- full_join(rename(g, n_bold = n), rename(b, n_bold_br = n), by = "nome") |>
      mutate(n_bold = coalesce(n_bold, 0L), n_bold_br = coalesce(n_bold_br, 0L)) |>
      arrange(nome)
    if (nrow(out) < 1000) stop("resposta suspeita (", nrow(out), " nomes)")
    msg("  ", nrow(out), " nomes; ", sum(out$n_bold), " registros (", sum(out$n_bold_br), " do Brasil)")
    gravar_csv(out, arq)
    out
  }, error = function(e) {
    msg("  aviso: BOLD falhou: ", conditionMessage(e))
    ant <- ler_csv(arq)
    if (is.null(ant)) stop("BOLD indisponível e sem cache")
    attr(ant, "aviso") <- TRUE
    ant
  })
}

# Soma os registros do BOLD de cada espécie sob o nome da lista e sob os nomes
# alternativos. Também conta registros do gênero coletados no Brasil sem
# identificação ("Gênero sp.", "cf.", "aff.").
cobre_bold <- function(especies, bold) {
  idx <- bold |> select(nome, n_bold, n_bold_br)
  nomes <- especies |>
    select(especie, sinonimos, sinonimos_ffb) |>
    mutate(nome = map2(especie, sinonimos, function(e, s) unique(c(e, if (!is.na(s)) strsplit(s, ";")[[1]]))),
           ffb = map(sinonimos_ffb, function(s) if (is.na(s)) character() else strsplit(s, ";")[[1]])) |>
    unnest(nome) |>
    mutate(da_ffb = map2_lgl(nome, ffb, `%in%`)) |>
    select(-ffb) |>
    left_join(idx, by = "nome") |>
    mutate(across(c(n_bold, n_bold_br), ~ coalesce(.x, 0L)))
  por_sp <- nomes |>
    group_by(especie) |>
    summarise(
      n_bold_nome = sum(n_bold[nome == especie]),
      n_bold_comb = sum(n_bold[nome == especie | nome %in% mesmo_epiteto(first(especie), nome)]),
      # sob o nome, o mesmo epíteto ou um sinônimo da Flora e Funga: conta como "com DNA"
      n_bold_ffb = sum(n_bold[nome == especie | da_ffb | nome %in% mesmo_epiteto(first(especie), nome)]),
      n_bold = sum(n_bold), n_bold_br = sum(n_bold_br),
      bold_sinonimos_com_dado = paste(nome[nome != especie & n_bold > 0], collapse = ";"),
      .groups = "drop"
    ) |>
    mutate(bold_sinonimos_com_dado = na_if(bold_sinonimos_com_dado, ""))

  sem_id <- bold |>
    filter(n_bold_br > 0, str_detect(nome, " (sp\\.|cf\\.|aff\\.|nr\\.)")) |>
    mutate(genero = word(nome, 1)) |>
    group_by(genero) |>
    summarise(bold_br_genero_sem_id = sum(n_bold_br), .groups = "drop")

  especies |>
    select(especie, genero) |>
    left_join(por_sp, by = "especie") |>
    left_join(sem_id, by = "genero") |>
    mutate(bold_br_genero_sem_id = coalesce(bold_br_genero_sem_id, 0L)) |>
    select(-genero)
}
