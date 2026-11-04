# Catálogo Taxonômico da Fauna do Brasil (CTFB/JBRJ) ---------------------------
#
# API REST aberta (PostgREST): https://fauna.jbrj.gov.br/rest/
# Visão `mv_planilha_extendida`: 1 linha por táxon, com `taxonomicStatus`,
# `acceptedNameUsageID` (liga sinônimo -> nome aceito) e `locationID`
# ("BR-AC;BR-AM;...", ocorrência por UF já agregada).
#
# Usos aqui: (a) sinônimos para buscar no BOLD/GenBank (contra a "falsa órfã");
# (b) UF, endemismo e ano de descrição quando a lista do grupo não traz.
# O pacote `faunabr` (CRAN) baixa o DwC-A completo do CTFB; a API REST é mais
# leve para o que precisamos (uma consulta por classe).

CTFB_URL <- "https://fauna.jbrj.gov.br/rest/mv_planilha_extendida"
CTFB_COLS <- c("taxonID", "genus", "specificEpithet", "scientificNameAuthorship",
               "namePublishedInYear", "taxonomicStatus", "acceptedNameUsageID",
               "locationID", "endemicBrazil", "class")

baixa_ctfb_classe <- function(classe, passo = 1000) {
  pedacos <- list()
  offset <- 0
  repeat {
    resp <- req_base(CTFB_URL) |>
      req_url_query(class = paste0("eq.", classe), taxonRank = "eq.species",
                    select = paste(CTFB_COLS, collapse = ","),
                    order = "taxonID", limit = passo, offset = offset) |>
      req_perform()
    lote <- resp_body_json(resp, simplifyVector = TRUE)
    if (length(lote) == 0 || NROW(lote) == 0) break
    pedacos[[length(pedacos) + 1]] <- as_tibble(lote)
    if (NROW(lote) < passo) break
    offset <- offset + passo
  }
  bind_rows(pedacos)
}

# Atualiza data/ctfb.csv; se a API falhar, mantém o arquivo anterior.
atualiza_ctfb <- function(classes, destino = "data/ctfb.csv") {
  anterior <- ler_csv(destino, col_types = cols(.default = col_character()))
  novos <- purrr::map(classes, function(cl) {
    msg("CTFB: ", cl)
    tryCatch(baixa_ctfb_classe(cl), error = function(e) {
      msg("  aviso: CTFB falhou para ", cl, ": ", conditionMessage(e))
      if (!is.null(anterior)) filter(anterior, class == cl) else NULL
    })
  }) |> bind_rows()
  if (NROW(novos) == 0) {
    if (is.null(anterior)) stop("CTFB indisponível e sem cópia local.")
    return(anterior)
  }
  novos <- novos |>
    mutate(across(everything(), as.character),
           nome = binomio(genus, specificEpithet)) |>
    arrange(class, nome, taxonID)
  gravar_csv(novos, destino)
  novos
}

# Junta CTFB à lista: nome aceito, sinônimos e preenchimento de lacunas.
enriquece_ctfb <- function(especies, ctfb) {
  ctfb <- ctfb |> mutate(nome = binomio(genus, specificEpithet))
  aceitos <- filter(ctfb, taxonomicStatus == "accepted")
  sinon <- filter(ctfb, taxonomicStatus != "accepted", !is.na(acceptedNameUsageID),
                  acceptedNameUsageID != "")

  # 1) localizar o táxon aceito no CTFB para cada nome da lista
  por_nome_aceito <- aceitos |> distinct(classe = class, nome, .keep_all = TRUE) |>
    select(classe, nome, id_aceito = taxonID)
  por_nome_sinon <- sinon |> distinct(classe = class, nome, .keep_all = TRUE) |>
    select(classe, nome, id_via_sinonimo = acceptedNameUsageID)

  x <- especies |>
    left_join(por_nome_aceito, by = c("classe", "especie" = "nome")) |>
    left_join(por_nome_sinon, by = c("classe", "especie" = "nome")) |>
    mutate(
      ctfb_situacao = case_when(
        !is.na(id_aceito) ~ "aceito",
        !is.na(id_via_sinonimo) ~ "sinonimo",
        TRUE ~ "ausente"
      ),
      id_ctfb = coalesce(id_aceito, id_via_sinonimo)
    ) |>
    select(-id_aceito, -id_via_sinonimo)

  info <- aceitos |>
    group_by(taxonID) |>
    summarise(
      nome_aceito_ctfb = first(nome),
      ufs_ctfb = junta_ufs(str_remove(unlist(strsplit(first(locationID), ";")), "^BR-")),
      endemica_ctfb = case_when(first(sem_acento(endemicBrazil)) == "sim" ~ TRUE,
                                first(sem_acento(endemicBrazil)) == "nao" ~ FALSE),
      ano_ctfb = suppressWarnings(as.integer(first(namePublishedInYear))),
      autoria_ctfb = first(scientificNameAuthorship),
      .groups = "drop"
    )
  sin_lista <- sinon |>
    group_by(acceptedNameUsageID) |>
    summarise(sinonimos_ctfb = paste(sort(unique(nome)), collapse = ";"), .groups = "drop")

  x |>
    left_join(info, by = c("id_ctfb" = "taxonID")) |>
    left_join(sin_lista, by = c("id_ctfb" = "acceptedNameUsageID")) |>
    mutate(
      # nomes alternativos = os da própria lista + nome aceito no CTFB (se
      # diferente) + sinônimos do CTFB
      sinonimos = pmap_chr(list(especie, nome_aceito_ctfb, sinonimos_ctfb, sinonimos_lista), function(e, a, s, l) {
        n <- unique(c(unlist(strsplit(coalesce(l, ""), ";")), a, unlist(strsplit(coalesce(s, ""), ";"))))
        n <- setdiff(n[!is.na(n) & nzchar(n)], e)
        n <- n[str_detect(n, "^[A-Z][a-z-]+ [a-z-]+$")]
        if (length(n)) paste(n, collapse = ";") else NA_character_
      }),
      uf_fonte = case_when(is.na(ufs) & !is.na(ufs_ctfb) ~ "CTFB", TRUE ~ uf_fonte),
      ufs = coalesce(ufs, ufs_ctfb),
      endemica_br = coalesce(endemica_br, endemica_ctfb),
      ano_descricao = coalesce(ano_descricao, ano_ctfb),
      autoria = coalesce(autoria, autoria_ctfb)
    ) |>
    select(-ufs_ctfb, -endemica_ctfb, -ano_ctfb, -autoria_ctfb, -sinonimos_ctfb)
}
