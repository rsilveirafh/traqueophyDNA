# Categoria nacional de ameaça: Portaria GM/MMA nº 1.783/2026 --------------------
#
# Lista Nacional Oficial das Espécies da Flora Ameaçadas de Extinção (substitui o
# anexo da Portaria MMA 443/2014 e revoga a 148/2022). O MMA publica a lista em
# CSV no Portal de Dados Abertos (separador ";", latin1):
#   colunas Nº; #; Família; Espécie ou Subespécie/Variedade; Categoria de Risco
#   ("#" = "*" quando a espécie entrou nesta revisão).
#
# Regras (as mesmas do tetrapoDNA):
# - espécie listada -> categoria da portaria;
# - só táxons infraespecíficos listados -> categoria mais grave + " (infra)";
# - nome da portaria que é sinônimo na Flora e Funga -> vai para o nome aceito;
# - não listada -> NA (a Flora e Funga não traz DD/NT/LC).

ORDEM_CAT <- c("EW", "CR (PE)", "CR", "EN", "VU")
FONTE_AMEACA <- "Portaria MMA 1.783/2026"

URL_MMA <- "https://dados.mma.gov.br/dataset/41a79b71-445f-4a6a-8c70-d46af991292a/resource/4e80c194-8ef8-4ef4-9731-48afb5c1aeef/download/lista-de-especies-da-flora-ameacadas-portaria1783_2026.csv"
ARQ_MMA <- "data-raw/mma_flora_portaria_1783_2026.csv"

ler_portaria <- function(arq = ARQ_MMA) {
  p <- readr::read_delim(arq, delim = ";", locale = locale(encoding = "latin1"),
                         col_types = cols(.default = col_character()), progress = FALSE)
  names(p) <- c("n", "nova", "familia", "taxon", "categoria")
  p |>
    mutate(across(everything(), str_squish),
           taxon = str_remove(taxon, "\\s*\\*$"),
           nova = nova %in% "*",
           especie = word(taxon, 1, 2),
           nivel = if_else(str_count(taxon, " ") >= 2, "infra", "sp"),
           grav = match(categoria, ORDEM_CAT))
}

aplica_ameaca <- function(especies, baixar = TRUE) {
  if (baixar) tryCatch(baixar_fonte(URL_MMA, ARQ_MMA), error = function(e) NULL)
  if (!file.exists(ARQ_MMA)) {
    msg("  aviso: ", ARQ_MMA, " ausente; sem categoria de ameaça")
    return(mutate(especies, categoria_ameaca = NA_character_, fonte_ameaca = NA_character_))
  }
  p <- ler_portaria()
  if (any(is.na(p$grav))) msg("  aviso: categorias desconhecidas na portaria: ",
                              paste(unique(p$categoria[is.na(p$grav)]), collapse = ", "))

  # nome (da lista ou alternativo) -> espécie aceita; nomes ambíguos ficam fora
  ponte <- especies |>
    select(especie, sinonimos_lista) |>
    mutate(nome = map2(especie, sinonimos_lista, ~ unique(c(.x, if (!is.na(.y)) strsplit(.y, ";")[[1]])))) |>
    select(aceita = especie, nome) |>
    unnest(nome) |>
    group_by(nome) |>
    filter(n_distinct(aceita) == 1) |>
    ungroup() |>
    distinct()
  p <- p |>
    left_join(rename(ponte, aceita_taxon = aceita), by = c("taxon" = "nome")) |>
    left_join(rename(ponte, aceita_sp = aceita), by = c("especie" = "nome")) |>
    mutate(
      aceita = coalesce(aceita_taxon, aceita_sp),
      # nome infra que é sinônimo de OUTRA espécie (variedade elevada a
      # espécie, por exemplo): a categoria vale para essa espécie inteira
      nivel = if_else(nivel == "infra" & !is.na(aceita_taxon) & aceita_taxon != coalesce(aceita_sp, ""),
                      "sp", nivel)
    )

  # Reserva: grafia do gênero ou do epíteto diferente (Melanoxylon/Melanoxylum,
  # gomesferreirae/gomes-ferreirae, gênero antigo). Mesmo epíteto, sem hífen,
  # dentro da mesma família, e só quando há uma espécie da lista com ele.
  chave <- function(e) str_remove_all(tolower(e), "-")
  por_epiteto <- especies |>
    transmute(familia, ep = chave(word(especie, 2)), aceita_ep = especie) |>
    group_by(familia, ep) |>
    filter(n() == 1) |>
    ungroup()
  p <- p |>
    mutate(ep = chave(word(especie, 2))) |>
    left_join(por_epiteto, by = c("familia", "ep")) |>
    mutate(por_ep = is.na(aceita) & !is.na(aceita_ep),
           aceita = coalesce(aceita, aceita_ep))
  if (any(p$por_ep)) msg("  pelo epíteto na família: ",
                         paste0(p$taxon[p$por_ep], " -> ", p$aceita[p$por_ep], collapse = "; "))
  fora <- filter(p, is.na(aceita))
  msg("  Portaria 1.783/2026: ", nrow(p), " táxons; ", nrow(p) - nrow(fora),
      " ligados a espécies da lista; ", nrow(fora), " sem par (exóticas, não vasculares ou nome ausente)")
  attr(especies, "ameaca_sem_par") <- fora

  por_sp <- p |>
    filter(!is.na(aceita)) |>
    group_by(aceita) |>
    summarise(
      cat_sp = categoria[nivel == "sp"][which.min(grav[nivel == "sp"])][1],
      cat_infra = categoria[nivel == "infra"][which.min(grav[nivel == "infra"])][1],
      nova_lista = any(nova),
      .groups = "drop"
    ) |>
    mutate(cat = coalesce(cat_sp, if_else(is.na(cat_infra), NA_character_, paste0(cat_infra, " (infra)"))))

  especies |>
    select(-any_of(c("categoria_ameaca", "fonte_ameaca"))) |>
    left_join(select(por_sp, especie = aceita, categoria_ameaca = cat), by = "especie") |>
    mutate(fonte_ameaca = if_else(is.na(categoria_ameaca), NA_character_, FONTE_AMEACA)) |>
    relocate(categoria_ameaca, fonte_ameaca, .after = endemica_br)
}
