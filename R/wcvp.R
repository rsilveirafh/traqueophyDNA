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
      all(especies$especie %in% ant$especie) && "ambiguos" %in% names(ant)) {
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

  # Homônimos: binômio que, no WCVP, leva a mais de uma espécie aceita (o
  # infraespecífico conta como a espécie-mãe). Sequências sob ele podem ser de
  # qualquer uma: o nome sai da busca (validação, 2026-11-05: Terminalia sericea,
  # Eupatorium paniculatum, Cymbidium grandiflorum...).
  sp_de <- w |> filter(taxon_status == "Accepted") |>
    transmute(id = plant_name_id, sp = if_else(taxon_rank == "Species", plant_name_id, parent_plant_name_id))
  ambiguos <- w |>
    filter(taxon_status == "Accepted" | taxon_status %in% STATUS_SINONIMO_WCVP) |>
    transmute(taxon_name, id = if_else(taxon_status == "Accepted", plant_name_id, accepted_plant_name_id)) |>
    inner_join(sp_de, by = "id") |>
    distinct(taxon_name, sp) |>
    count(taxon_name) |>
    filter(n > 1) |>
    pull(taxon_name)
  rm(sp_de)

  outras <- especies$especie
  sin_sp <- casa |>
    select(especie, id_aceito) |>
    inner_join(nomes, by = "id_aceito", relationship = "many-to-many") |>
    # fora: o próprio nome e os nomes de outras espécies da lista
    filter(nome != especie, !nome %in% outras) |>
    group_by(especie) |>
    summarise(sinonimos_wcvp = paste(sort(unique(nome)), collapse = ";"), .groups = "drop")
  # nomes ambíguos entre os nomes buscados de cada espécie (da lista e do WCVP)
  amb <- especies |>
    select(especie, sinonimos_lista) |>
    left_join(sin_sp, by = "especie") |>
    mutate(ambiguos = map2_chr(sinonimos_lista, sinonimos_wcvp, function(a, b) {
      x <- intersect(unique(c(if (!is.na(a)) strsplit(a, ";")[[1]], if (!is.na(b)) strsplit(b, ";")[[1]])), ambiguos)
      if (length(x)) paste(sort(x), collapse = ";") else NA_character_
    })) |>
    select(especie, ambiguos)
  out <- especies |>
    select(especie) |>
    left_join(casa, by = "especie") |>
    left_join(select(aceitos, id_aceito, nome_aceito_wcvp), by = "id_aceito") |>
    left_join(sin_sp, by = "especie") |>
    left_join(amb, by = "especie") |>
    select(-id_aceito) |>
    mutate(wcvp_situacao = coalesce(wcvp_situacao, "ausente"),
           nome_aceito_wcvp = if_else(nome_aceito_wcvp == especie, NA_character_, nome_aceito_wcvp),
           # o WCVP junta a espécie com outra que a Flora e Funga aceita: vale a
           # Flora e Funga, e os nomes do WCVP (que são da outra espécie) não entram
           wcvp_junta_com = if_else(nome_aceito_wcvp %in% outras, nome_aceito_wcvp, NA_character_),
           sinonimos_wcvp = if_else(is.na(wcvp_junta_com), sinonimos_wcvp, NA_character_),
           consultado_em = hoje()) |>
    arrange(especie)
  msg("  WCVP: ", sum(out$wcvp_situacao == "aceito"), " aceitas, ",
      sum(out$wcvp_situacao == "sinonimo"), " sinônimos, ",
      sum(out$wcvp_situacao == "ausente"), " ausentes; ",
      sum(!is.na(out$wcvp_junta_com)), " juntadas a outra espécie da lista; ",
      sum(!is.na(out$ambiguos)), " com algum nome ambíguo (homônimo)")
  gravar_csv(out, destino)
  out
}

# Soma os sinônimos do WCVP aos da lista e completa o ano. Saem dos nomes buscados:
#   - os ambíguos (homônimos no WCVP);
#   - os que a Flora e Funga atribui a outra espécie (nome aceito ou sinônimo dela):
#     vale a sinonímia da Flora e Funga (validação, 2026-11-05).
# `sinonimos_ffb`: só os da Flora e Funga (sequência sob eles conta como "com DNA");
# `sinonimos`: os da Flora e Funga e os do WCVP (sob nome só do WCVP: "a verificar").
soma_wcvp <- function(especies, wcvp) {
  wc <- select(wcvp, especie, any_of(c("wcvp_situacao", "wcvp_junta_com", "sinonimos_wcvp", "ambiguos", "ano_wcvp")))
  for (col in c("wcvp_junta_com", "ambiguos", "sinonimos_wcvp")) if (!col %in% names(wc)) wc[[col]] <- NA_character_
  x <- especies |> left_join(wc, by = "especie")
  longo <- function(col, fonte) {
    tibble(especie = x$especie, nome = strsplit(coalesce(x[[col]], ""), ";")) |>
      unnest(nome) |> filter(nzchar(nome)) |> mutate(fonte = fonte)
  }
  ffb <- longo("sinonimos_lista", "ffb")
  # quem a Flora e Funga diz que é dono de cada nome (a espécie aceita ou a dona do sinônimo)
  dono <- bind_rows(tibble(nome = x$especie, dono = x$especie),
                    transmute(ffb, nome, dono = especie)) |> distinct()
  nomes <- bind_rows(ffb, longo("sinonimos_wcvp", "wcvp")) |>
    filter(nome != especie) |>
    anti_join(longo("ambiguos", "amb"), by = c("especie", "nome")) |>
    distinct(especie, nome, fonte)
  de_outra <- nomes |>
    inner_join(dono, by = "nome", relationship = "many-to-many") |>
    filter(dono != especie) |>
    distinct(especie, nome)
  nomes <- anti_join(nomes, de_outra, by = c("especie", "nome"))
  junta <- function(d) d |> group_by(especie) |>
    summarise(s = paste(sort(unique(nome)), collapse = ";"), .groups = "drop")
  x |>
    left_join(junta(filter(nomes, fonte == "ffb")) |> rename(sinonimos_ffb = s), by = "especie") |>
    left_join(junta(nomes) |> rename(sinonimos = s), by = "especie") |>
    mutate(ano_descricao = coalesce(ano_descricao, as.integer(ano_wcvp))) |>
    select(-any_of(c("sinonimos_wcvp", "ano_wcvp", "ambiguos")))
}
