# Categoria nacional de ameaça: Portaria MMA nº 1.704/2026 ----------------------
#
# Lista Nacional Oficial das Espécies da Fauna Ameaçadas de Extinção (Anexo I),
# extraída uma vez do PDF por data-raw/mma_portaria_1704_2026.R.
#
# Regras:
# - espécie listada -> categoria da portaria;
# - só subespécie(s) listada(s) -> categoria mais grave + " (ssp.)";
# - não listada, mas com categoria de ameaça (CR/EN/VU) na fonte antiga da lista
#   do grupo -> NA (saiu da lista ou não consta);
# - não listada e com DD/NT/LC na fonte antiga -> mantém (resultado de avaliação),
#   com a fonte antiga.

ORDEM_CAT <- c("EW", "CR (PE)", "CR", "EN", "VU")

aplica_ameaca_mma2026 <- function(especies, arq = "data-raw/mma_portaria_1704_2026.csv") {
  if (!file.exists(arq)) {
    msg("  aviso: ", arq, " ausente; mantendo categorias das listas")
    return(especies)
  }
  p <- readr::read_csv(arq, show_col_types = FALSE) |>
    mutate(nivel = if_else(str_count(taxon, " ") >= 2, "ssp", "sp"),
           grav = match(categoria, ORDEM_CAT))
  por_sp <- p |>
    group_by(classe, especie) |>
    summarise(
      cat_sp = categoria[nivel == "sp"][1],
      cat_ssp = categoria[nivel == "ssp"][which.min(grav[nivel == "ssp"])][1],
      .groups = "drop"
    ) |>
    mutate(cat_2026 = coalesce(cat_sp, if_else(is.na(cat_ssp), NA_character_, paste0(cat_ssp, " (ssp.)"))))

  x <- especies |>
    left_join(select(por_sp, classe, especie, cat_2026), by = c("classe", "especie"))
  antiga_ameacada <- !is.na(x$categoria_ameaca) & x$categoria_ameaca %in% c("CR", "EN", "VU", "EW", "EX", "RE")
  x |>
    mutate(
      fonte_ameaca = case_when(
        !is.na(cat_2026) ~ "Portaria MMA 1.704/2026",
        antiga_ameacada ~ NA_character_,
        !is.na(categoria_ameaca) ~ fonte_ameaca,
        TRUE ~ NA_character_
      ),
      categoria_ameaca = case_when(
        !is.na(cat_2026) ~ cat_2026,
        antiga_ameacada ~ NA_character_,
        TRUE ~ categoria_ameaca
      )
    ) |>
    select(-cat_2026)
}
