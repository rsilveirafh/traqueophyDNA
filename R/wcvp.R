# WCVP (World Checklist of Vascular Plants, Kew): sinônimos globais -----------------
#
# A sinonímia da Flora e Funga cobre bem os nomes usados no Brasil; o WCVP traz
# os nomes usados no resto do mundo (que também batizam sequências no GenBank e
# no BOLD) e o ano de publicação que falta na Flora e Funga.
#
#   https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip (~90 MB; wcvp_names.csv)
#
# Para cada espécie da lista:
#   1) acha o nome no WCVP (nível de espécie; preferência para "Accepted" e para
#      a mesma família);
#   2) se for sinônimo, segue para o nome aceito;
#   3) junta o nome aceito, os sinônimos dele (espécie e infraespecíficos) e os
#      infraespecíficos aceitos. Nomes mal aplicados ficam fora.
# `wcvp_situacao`: aceito / sinonimo (o nome da lista é sinônimo no WCVP) / ausente.
# Resultado em data/wcvp.csv, refeito quando passa de `dias_validade` ou quando a
# lista ganha espécies. Usado no lugar do GBIF do tetrapoDNA (roteiro, item 3).

WCVP_URL <- "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip"
STATUS_SINONIMO_WCVP <- c("Synonym", "Orthographic", "Illegitimate", "Invalid")

ler_wcvp <- function(zip) {
  cols <- c("plant_name_id", "taxon_rank", "taxon_status", "family", "genus_hybrid",
            "species_hybrid", "taxon_name", "first_published",
            "accepted_plant_name_id", "parent_plant_name_id")
  readr::read_delim(unz(zip, "wcvp_names.csv"), delim = "|", quote = "", na = c("", "NA"),
                    col_types = cols(.default = col_character()), col_select = all_of(cols),
                    progress = FALSE)
}

atualiza_wcvp <- function(especies, destino = "data/wcvp.csv", dias_validade = 90) {
  ant <- ler_csv(destino, col_types = cols(.default = col_character()))
  if (!is.null(ant) && nrow(ant) &&
      as.numeric(as.Date(hoje()) - as.Date(ant$consultado_em[1])) < dias_validade &&
      all(especies$especie %in% ant$especie)) {
    msg("WCVP: usando ", destino, " de ", ant$consultado_em[1])
    return(ant)
  }
  zip <- Sys.getenv("WCVP_ZIP", file.path(tempdir(), "wcvp.zip"))
  msg("WCVP: baixando")
  if (!file.exists(zip)) req_base(WCVP_URL) |> req_timeout(1800) |> req_perform(path = zip)
  w <- ler_wcvp(zip) |> filter(is.na(genus_hybrid), is.na(species_hybrid))
  if (!nzchar(Sys.getenv("WCVP_ZIP"))) unlink(zip)

  sp <- filter(w, taxon_rank == "Species")
  # 1) e 2): nome da lista -> id aceito no WCVP
  casa <- especies |>
    select(especie, familia) |>
    inner_join(select(sp, especie = taxon_name, plant_name_id, taxon_status, family,
                      accepted_plant_name_id, first_published),
               by = "especie", relationship = "many-to-many") |>
    filter(taxon_status == "Accepted" | taxon_status %in% STATUS_SINONIMO_WCVP) |>
    arrange(especie, taxon_status != "Accepted", family != familia) |>
    group_by(especie) |>
    slice(1) |>
    ungroup() |>
    transmute(especie,
              wcvp_situacao = if_else(taxon_status == "Accepted", "aceito", "sinonimo"),
              id_aceito = coalesce(accepted_plant_name_id, plant_name_id),
              ano_wcvp = extrai_ano(first_published))

  aceitos <- w |> filter(taxon_status == "Accepted") |>
    select(id_aceito = plant_name_id, nome_aceito_wcvp = taxon_name, rank_aceito = taxon_rank,
           ano_aceito = first_published)
  # 3) nomes alternativos: sinônimos do aceito; infraespecíficos aceitos e os
  #    sinônimos deles
  infra <- w |> filter(taxon_status == "Accepted", taxon_rank != "Species",
                       parent_plant_name_id %in% casa$id_aceito) |>
    select(id_infra = plant_name_id, id_aceito = parent_plant_name_id, nome = taxon_name)
  alvo <- bind_rows(tibble(id = unique(casa$id_aceito), id_aceito = id),
                    transmute(infra, id = id_infra, id_aceito))
  sinon <- w |>
    filter(taxon_status %in% STATUS_SINONIMO_WCVP) |>
    inner_join(alvo, by = c("accepted_plant_name_id" = "id"), relationship = "many-to-many") |>
    select(id_aceito, nome = taxon_name)
  nomes <- bind_rows(sinon, select(infra, id_aceito, nome),
                     select(aceitos, id_aceito, nome = nome_aceito_wcvp) |> filter(id_aceito %in% casa$id_aceito)) |>
    distinct()

  outras <- especies$especie
  sin_sp <- casa |>
    select(especie, id_aceito) |>
    inner_join(nomes, by = "id_aceito", relationship = "many-to-many") |>
    # fora: o próprio nome e os nomes de outras espécies da lista
    filter(nome != especie, !nome %in% outras) |>
    group_by(especie) |>
    summarise(sinonimos_wcvp = paste(sort(unique(nome)), collapse = ";"), .groups = "drop")
  out <- especies |>
    select(especie) |>
    left_join(casa, by = "especie") |>
    left_join(select(aceitos, id_aceito, nome_aceito_wcvp), by = "id_aceito") |>
    left_join(sin_sp, by = "especie") |>
    select(-id_aceito) |>
    mutate(wcvp_situacao = coalesce(wcvp_situacao, "ausente"),
           nome_aceito_wcvp = if_else(nome_aceito_wcvp == especie, NA_character_, nome_aceito_wcvp),
           consultado_em = hoje()) |>
    arrange(especie)
  msg("  WCVP: ", sum(out$wcvp_situacao == "aceito"), " aceitas, ",
      sum(out$wcvp_situacao == "sinonimo"), " sinônimos, ",
      sum(out$wcvp_situacao == "ausente"), " ausentes")
  gravar_csv(out, destino)
  out
}

# Soma os sinônimos do WCVP aos da lista (coluna `sinonimos`) e completa o ano.
soma_wcvp <- function(especies, wcvp) {
  junta <- function(a, b) {
    x <- unique(c(if (!is.na(a)) strsplit(a, ";")[[1]], if (!is.na(b)) strsplit(b, ";")[[1]]))
    if (length(x)) paste(sort(x), collapse = ";") else NA_character_
  }
  especies |>
    left_join(select(wcvp, especie, wcvp_situacao, sinonimos_wcvp, ano_wcvp), by = "especie") |>
    mutate(sinonimos = map2_chr(sinonimos_lista, sinonimos_wcvp, junta),
           ano_descricao = coalesce(ano_descricao, as.integer(ano_wcvp))) |>
    select(-sinonimos_wcvp, -ano_wcvp)
}
