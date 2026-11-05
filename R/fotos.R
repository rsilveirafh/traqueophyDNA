# iNaturalist: uma foto por espécie sem DNA (para os cartões do site) -----------
#
# 1) Nome -> id do iNaturalist pela taxonomia completa que o iNat publica todo mês
#    (Darwin Core, ~80 MB; taxa.csv, só Tracheophyta e nível de espécie):
#      https://www.inaturalist.org/taxa/inaturalist-taxonomy.dwca.zip
#    O mapa fica em data/cache/inat_ids.csv (inclusive "ausente no iNat"), refeito a
#    cada 90 dias ou quando aparece nome novo.
# 2) Foto padrão de até 30 táxons por chamada: /v1/taxa/<id1>,<id2>,...
#    Só com licença Creative Commons (CC0, CC-BY...); "todos os direitos
#    reservados" fica de fora. Sem chave; ~1 chamada por segundo.
# Com 16 mil espécies, são ~540 chamadas (~10 min) em vez de 16 mil buscas por nome.
# Cache em data/fotos.csv (validade de 180 dias, inclusive para "sem foto").

INAT_API <- "https://api.inaturalist.org/v1"
INAT_DWCA <- "https://www.inaturalist.org/taxa/inaturalist-taxonomy.dwca.zip"
INAT_LOTE <- 30   # máximo de ids por chamada aceito pela API

inat_ids <- function(nomes, destino = "data/cache/inat_ids.csv", dias_validade = 90) {
  mapa <- ler_csv(destino, col_types = cols(.default = col_character()))
  if (!is.null(mapa) && nrow(mapa) && all(nomes %in% mapa$especie) &&
      as.numeric(as.Date(hoje()) - as.Date(mapa$consultado_em[1])) < dias_validade) {
    return(mapa)
  }
  pasta <- file.path(tempdir(), "inat")
  dir.create(pasta, showWarnings = FALSE)
  zip <- file.path(pasta, "inat.zip")
  msg("iNaturalist: baixando a taxonomia")
  req_base(INAT_DWCA) |> req_timeout(1800) |> req_perform(path = zip)
  utils::unzip(zip, files = "taxa.csv", exdir = pasta)
  tx <- readr::read_csv(file.path(pasta, "taxa.csv"), progress = FALSE,
                        col_types = cols(.default = col_character()),
                        col_select = c(id, phylum, scientificName, taxonRank)) |>
    filter(phylum == "Tracheophyta", taxonRank == "species") |>
    arrange(as.integer(id)) |>
    distinct(scientificName, .keep_all = TRUE)   # homônimo raro: fica o id mais antigo
  unlink(pasta, recursive = TRUE)
  todos <- sort(unique(c(nomes, if (!is.null(mapa)) mapa$especie)))
  out <- tibble(especie = todos) |>
    left_join(select(tx, especie = scientificName, inat_id = id), by = "especie") |>
    mutate(consultado_em = hoje())
  msg("  ", sum(!is.na(out$inat_id)), " de ", nrow(out), " nomes estão no iNaturalist")
  gravar_csv(out, destino)
  out
}

inat_fotos_lote <- function(ids) {
  j <- req_base(paste0(INAT_API, "/taxa/", paste(ids, collapse = ","))) |>
    req_throttle(rate = 1, realm = "inaturalist") |>
    req_perform() |>
    resp_body_json()
  map(j$results, function(t) {
    p <- t$default_photo
    ok <- !is.null(p) && !is.null(p$license_code) && nzchar(p$license_code)
    tibble(inat_id = as.character(t$id),
           foto_url = if (ok) p$medium_url else NA_character_,
           foto_credito = if (ok) p$attribution else NA_character_,
           foto_licenca = if (ok) p$license_code else NA_character_)
  }) |> bind_rows()
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
  if (length(falta) == 0) {
    msg("iNaturalist (fotos): nada a consultar")
    return(invisible(cache))
  }
  mapa <- inat_ids(falta) |> filter(especie %in% falta)
  com_id <- filter(mapa, !is.na(inat_id))
  lotes <- split(com_id$inat_id, ceiling(seq_len(nrow(com_id)) / INAT_LOTE))
  msg("iNaturalist (fotos): ", length(falta), " espécies, ", nrow(com_id), " no iNat, ",
      length(lotes), " chamadas")
  inicio <- Sys.time()
  fotos <- list()
  for (b in seq_along(lotes)) {
    fotos[[b]] <- tryCatch(inat_fotos_lote(lotes[[b]]), error = function(e) {
      msg("  aviso iNaturalist: ", conditionMessage(e))
      NULL
    })
    if (b %% 100 == 0) msg("  ", b, "/", length(lotes))
    if (as.numeric(difftime(Sys.time(), inicio, units = "mins")) > max_minutos) break
  }
  fotos <- bind_rows(fotos)
  if (!nrow(fotos)) fotos <- tibble(inat_id = character(), foto_url = character(),
                                    foto_credito = character(), foto_licenca = character())
  # consultadas = as que não estão no iNat + as dos lotes que responderam
  novos <- mapa |>
    filter(is.na(inat_id) | inat_id %in% fotos$inat_id) |>
    select(especie, inat_id) |>
    left_join(distinct(fotos, inat_id, .keep_all = TRUE), by = "inat_id") |>
    mutate(consultado_em = hoje())
  msg("  ", sum(!is.na(novos$foto_url)), " foto(s) com licença CC de ", nrow(novos), " espécies")
  out <- cache |> filter(!especie %in% novos$especie) |> bind_rows(novos) |>
    filter(especie %in% alvo) |> arrange(especie)
  gravar_csv(out, destino)
  invisible(out)
}
