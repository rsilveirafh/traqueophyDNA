# iNaturalist: uma foto por espécie sem DNA (para os cartões do site) -----------
#
# /v1/taxa?q=<nome>&rank=species: usa o táxon cujo nome é exatamente o da lista
# e a foto padrão dele, só se tiver licença Creative Commons (CC0, CC-BY...).
# Fotos "todos os direitos reservados" ficam de fora. Sem chave; o iNaturalist
# pede no máximo ~1 consulta por segundo.
# Cache em data/fotos.csv (validade de 180 dias, inclusive para "sem foto").

INAT_API <- "https://api.inaturalist.org/v1"

inat_foto <- function(especie) {
  j <- req_base(paste0(INAT_API, "/taxa")) |>
    req_url_query(q = especie, rank = "species", is_active = "true", per_page = 10) |>
    req_perform() |>
    resp_body_json()
  for (t in j$results) {
    if (!identical(tolower(t$name), tolower(especie))) next
    p <- t$default_photo
    ok <- !is.null(p) && !is.null(p$license_code) && nzchar(p$license_code)
    return(tibble(
      especie = especie, inat_id = as.character(t$id),
      foto_url = if (ok) p$medium_url else NA_character_,
      foto_credito = if (ok) p$attribution else NA_character_,
      foto_licenca = if (ok) p$license_code else NA_character_
    ))
  }
  tibble(especie = especie, inat_id = NA_character_, foto_url = NA_character_,
         foto_credito = NA_character_, foto_licenca = NA_character_)
}

atualiza_fotos <- function(alvo, destino = "data/fotos.csv", dias_validade = 180,
                           max_minutos = Inf) {
  cache <- ler_csv(destino, col_types = cols(.default = col_character()))
  if (is.null(cache)) {
    cache <- tibble(especie = character(), inat_id = character(), foto_url = character(),
                    foto_credito = character(), foto_licenca = character(),
                    consultado_em = character())
  }
  validos <- cache |>
    filter(as.numeric(as.Date(hoje()) - as.Date(consultado_em)) < dias_validade) |>
    pull(especie)
  falta <- setdiff(unique(alvo), validos)
  msg("iNaturalist (fotos): ", length(falta), " consultas")
  if (length(falta) == 0) return(invisible(cache))
  inicio <- Sys.time()
  novos <- vector("list", length(falta))
  for (i in seq_along(falta)) {
    novos[[i]] <- tryCatch(mutate(inat_foto(falta[i]), consultado_em = hoje()),
                           error = function(e) NULL)
    Sys.sleep(1)
    if (i %% 100 == 0) msg("  ", i, "/", length(falta))
    if (as.numeric(difftime(Sys.time(), inicio, units = "mins")) > max_minutos) break
  }
  novos <- bind_rows(novos)
  out <- cache |> filter(!especie %in% novos$especie) |> bind_rows(novos) |>
    filter(especie %in% alvo) |> arrange(especie)
  gravar_csv(out, destino)
  invisible(out)
}
