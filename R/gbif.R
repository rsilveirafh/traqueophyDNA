# GBIF Backbone: sinônimos para as candidatas a órfã -----------------------------
#
# O CTFB tem poucos sinônimos para aves e répteis. Para as espécies sem DNA
# sob o nome da lista, consulta o GBIF:
#   1) /v1/species/match (nome da lista) -> nome aceito no GBIF + sinônimos dele;
#   2) se o nome não casa no nível de espécie, nada é inventado: a espécie fica
#      "a verificar" ("nome não reconhecido no GBIF").
# Cache em data/gbif_sinonimos.csv (validade de 90 dias). Sem chave.

GBIF_API <- "https://api.gbif.org/v1"

eh_binomio <- function(x) !is.na(x) & str_detect(x, "^[A-Z][a-z-]+ [a-z-]+$")

gbif_json <- function(caminho, ...) {
  req_base(paste0(GBIF_API, caminho)) |> req_url_query(...) |> req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

gbif_nomes <- function(especie, classe, familia) {
  m <- gbif_json("/species/match", name = especie, class = classe, kingdom = "Animalia")
  if (!(isTRUE(m$matchType %in% c("EXACT", "FUZZY")) && isTRUE(m$rank == "SPECIES"))) {
    # Nome não reconhecido no nível de espécie. Não chutamos combinações
    # (o mesmo epíteto na mesma família pode ser outra espécie): a espécie
    # vira "a verificar" se continuar sem DNA.
    return(list(nomes = character(), situacao = "nao_reconhecido"))
  }
  nomes <- character()
  chave <- if (!is.null(m$acceptedUsageKey)) m$acceptedUsageKey else m$usageKey
  if (!is.null(m$species)) nomes <- c(nomes, m$species)
  s <- tryCatch(gbif_json(paste0("/species/", chave, "/synonyms"), limit = 100)$results,
                error = function(e) NULL)
  if (NROW(s)) nomes <- c(nomes, s$canonicalName)
  nomes <- unique(nomes[eh_binomio(nomes)])
  list(nomes = setdiff(nomes, especie),
       situacao = if (identical(m$status, "SYNONYM")) "sinonimo" else "aceito")
}

atualiza_gbif <- function(alvo, destino = "data/gbif_sinonimos.csv", dias_validade = 90) {
  cache <- ler_csv(destino, col_types = cols(.default = col_character()))
  if (is.null(cache) || !"gbif_situacao" %in% names(cache)) {
    cache <- tibble(especie = character(), sinonimos_gbif = character(),
                    gbif_situacao = character(), consultado_em = character())
  }
  validos <- cache |>
    filter(as.numeric(as.Date(hoje()) - as.Date(consultado_em)) < dias_validade) |>
    pull(especie)
  falta <- filter(alvo, !especie %in% validos)
  msg("GBIF (sinônimos das candidatas a órfã): ", nrow(falta), " consultas")
  if (nrow(falta) == 0) return(cache)
  novos <- vector("list", nrow(falta))
  for (i in seq_len(nrow(falta))) {
    r <- tryCatch(gbif_nomes(falta$especie[i], falta$classe[i], falta$familia[i]),
                  error = function(e) NULL)
    if (is.null(r)) next
    novos[[i]] <- tibble(especie = falta$especie[i],
                         sinonimos_gbif = if (length(r$nomes)) paste(r$nomes, collapse = ";") else NA_character_,
                         gbif_situacao = r$situacao, consultado_em = hoje())
    if (i %% 200 == 0) msg("  ", i, "/", nrow(falta))
  }
  novos <- bind_rows(novos)
  out <- cache |> filter(!especie %in% novos$especie) |> bind_rows(novos) |> arrange(especie)
  gravar_csv(out, destino)
  out
}

# Junta os sinônimos do GBIF aos do CTFB (coluna `sinonimos`).
soma_sinonimos_gbif <- function(especies, gbif) {
  especies |>
    left_join(select(gbif, especie, sinonimos_gbif, gbif_situacao), by = "especie") |>
    mutate(sinonimos = map2_chr(sinonimos, sinonimos_gbif, function(a, b) {
      n <- unique(c(unlist(strsplit(coalesce(a, ""), ";")), unlist(strsplit(coalesce(b, ""), ";"))))
      n <- n[nzchar(n)]
      if (length(n)) paste(n, collapse = ";") else NA_character_
    })) |>
    select(-sinonimos_gbif)
}

# Mesmo epíteto (com tolerância à concordância de gênero: -us/-a/-um) =
# mesma espécie em outra combinação (transferência de gênero).
mesmo_epiteto <- function(nome, outros) {
  raiz <- function(e) substr(e, 1, pmax(4, nchar(e) - 2))
  e <- word(nome, 2)
  outros[raiz(word(outros, 2)) == raiz(e)]
}
