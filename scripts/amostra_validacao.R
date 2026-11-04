#!/usr/bin/env Rscript
# Sorteia a amostra para conferência manual da classificação (ver validacao/PROTOCOLO.md).
#
# Amostra estratificada por situação e, dentro dela, proporcional ao tamanho de cada
# grupo (método do maior resto). Semente fixa: rodar de novo sobre o mesmo
# data/cobertura.csv dá a mesma amostra.
#
# Uso: Rscript scripts/amostra_validacao.R
# Saída: validacao/amostra.csv (o sorteio) e validacao/conferencia.xlsx (para preencher).

source("R/utils.R", encoding = "UTF-8")

SEMENTE <- 20261030
TAMANHO <- c("sem DNA" = 50, "a verificar" = 50, "com DNA" = 20)

cob <- ler_csv("data/cobertura.csv", col_types = cols(.default = col_character()))
versao <- unique(cob$atualizado_em)

# Quantas de cada grupo em cada situação: proporcional, arredondando pelo maior resto.
reparte <- function(n_pop, n) {
  q <- n * n_pop / sum(n_pop)
  k <- floor(q)
  resto <- n - sum(k)
  k[order(q - k, decreasing = TRUE)[seq_len(resto)]] <- k[order(q - k, decreasing = TRUE)[seq_len(resto)]] + 1
  k
}

set.seed(SEMENTE)
amostra <- purrr::imap(TAMANHO, function(tam, sit) {
  pop <- filter(cob, situacao == sit)
  cotas <- count(pop, grupo) |> mutate(k = reparte(n, tam))
  purrr::map2(cotas$grupo, cotas$k, function(g, k) slice_sample(filter(pop, grupo == g), n = k)) |>
    bind_rows() |>
    mutate(peso = nrow(pop) / tam)
}) |>
  bind_rows() |>
  mutate(situacao = factor(situacao, names(TAMANHO))) |>
  arrange(situacao, grupo, especie) |>
  mutate(situacao = as.character(situacao),
         id = sprintf("V%03d", row_number()), .before = 1)

url <- function(base, termo) paste0(base, utils::URLencode(termo, reserved = TRUE))
saida <- amostra |>
  transmute(
    id, grupo, familia, especie, autoria, situacao_painel = situacao, observacao,
    sinonimos_buscados = gsub(";", ", ", coalesce(sinonimos, "")),
    n_bold, n_genbank,
    bold = url("https://portal.boldsystems.org/result?query=", paste0(especie, "[tax]")),
    genbank_organism = url("https://www.ncbi.nlm.nih.gov/nuccore/?term=", paste0('"', especie, '"[Organism]')),
    genbank_todos = url("https://www.ncbi.nlm.nih.gov/nuccore/?term=", paste0('"', especie, '"')),
    taxonomia_ncbi = url("https://www.ncbi.nlm.nih.gov/taxonomy/?term=", especie),
    gbif = url("https://www.gbif.org/species/search?q=", especie),
    peso,
    # A preencher (ver PROTOCOLO.md)
    dna_publico = NA_character_, base = NA_character_, nome_no_banco = NA_character_,
    n_sequencias = NA_character_, acesso_exemplo = NA_character_, situacao_correta = NA_character_,
    motivo = NA_character_, comentario = NA_character_, conferido_por = NA_character_,
    data_conferencia = NA_character_
  )

dir.create("validacao", showWarnings = FALSE)
gravar_csv(select(saida, id:peso), "validacao/amostra.csv")

# Planilha: links clicáveis, listas suspensas e as colunas a preencher em destaque.
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "conferencia")
openxlsx::writeData(wb, "conferencia", saida)
for (col in c("bold", "genbank_organism", "genbank_todos", "taxonomia_ncbi", "gbif")) {
  j <- match(col, names(saida))
  v <- saida[[col]]
  class(v) <- "hyperlink"
  openxlsx::writeData(wb, "conferencia", x = v, startCol = j, startRow = 2, colNames = FALSE)
}
n <- nrow(saida)
listas <- list(
  dna_publico = c("sim", "não", "incerto"),
  base = c("BOLD", "GenBank", "ambos", "nenhuma"),
  situacao_correta = c("com DNA", "sem DNA", "a verificar"),
  motivo = c("confere", "sinônimo não buscado", "grafia diferente", "só como sp./cf./aff.",
             "nome fora do Organism", "registro privado/sem dado público", "outro")
)
for (col in names(listas)) {
  openxlsx::dataValidation(wb, "conferencia", cols = match(col, names(saida)), rows = 2:(n + 1),
                           type = "list", value = paste0('"', paste(listas[[col]], collapse = ","), '"'))
}
a_preencher <- match("dna_publico", names(saida)):ncol(saida)
openxlsx::addStyle(wb, "conferencia", openxlsx::createStyle(fgFill = "#fff3d6"),
                   rows = 1:(n + 1), cols = a_preencher, gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, "conferencia", openxlsx::createStyle(textDecoration = "bold"),
                   rows = 1, cols = seq_along(saida), stack = TRUE)
openxlsx::freezePane(wb, "conferencia", firstActiveRow = 2, firstActiveCol = 5)
openxlsx::setColWidths(wb, "conferencia", cols = seq_along(saida), widths = "auto")
openxlsx::setColWidths(wb, "conferencia", cols = match("observacao", names(saida)), widths = 50)

openxlsx::addWorksheet(wb, "sobre")
openxlsx::writeData(wb, "sobre", data.frame(campo = c("dados", "semente", "tamanho", "protocolo"), valor = c(
  paste("data/cobertura.csv, atualizado em", paste(versao, collapse = ", ")),
  SEMENTE,
  paste(names(TAMANHO), TAMANHO, sep = ": ", collapse = "; "),
  "validacao/PROTOCOLO.md"
)))
openxlsx::saveWorkbook(wb, "validacao/conferencia.xlsx", overwrite = TRUE)

msg("Amostra: ", n, " espécies (dados de ", paste(versao, collapse = ", "), ")")
print(count(amostra, situacao, grupo) |> tidyr::pivot_wider(names_from = grupo, values_from = n))
