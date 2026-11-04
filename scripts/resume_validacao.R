#!/usr/bin/env Rscript
# Resume a validação: junta o sorteio (validacao/amostra.csv) com o julgamento de
# cada espécie (validacao/julgamento.csv), calcula proporções com intervalo de
# confiança de Wilson (95%), estima o efeito no painel inteiro e sorteia a
# subamostra para auditoria humana.
#
# Uso: Rscript scripts/resume_validacao.R
# Saída: validacao/resultado.csv e validacao/auditoria.csv.

source("R/utils.R", encoding = "UTF-8")

wilson <- function(x, n, z = 1.96) {
  p <- x / n
  centro <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  meia <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  tibble(p = p, li = pmax(0, centro - meia), ls = pmin(1, centro + meia))
}

amostra <- ler_csv("validacao/amostra.csv", col_types = cols(.default = col_character()))
julg <- ler_csv("validacao/julgamento.csv", col_types = cols(.default = col_character()))
cob <- ler_csv("data/cobertura.csv", col_types = cols(.default = col_character()))
x <- inner_join(select(amostra, id, grupo, especie, situacao_painel), select(julg, id, situacao_correta, motivo), by = "id")
stopifnot(nrow(x) == nrow(amostra))

pop <- count(cob, situacao_painel = situacao, name = "n_painel")
res <- x |>
  count(situacao_painel, situacao_correta, name = "k") |>
  group_by(situacao_painel) |> mutate(n = sum(k)) |> ungroup()
res <- bind_cols(res, wilson(res$k, res$n)) |>
  left_join(pop, by = "situacao_painel") |>
  mutate(estimativa = round(p * n_painel), est_li = round(li * n_painel), est_ls = round(ls * n_painel)) |>
  arrange(situacao_painel, desc(k))
gravar_csv(res, "validacao/resultado.csv")

cat("\nSituação no painel -> situação conferida (proporção e IC 95%; estimativa para o painel)\n")
for (i in seq_len(nrow(res))) with(res[i, ], cat(sprintf("  %-12s -> %-12s %2d/%2d  %3.0f%% (%2.0f–%2.0f%%)  ~%d de %s (%d–%d)\n",
  situacao_painel, situacao_correta, k, n, 100 * p, 100 * li, 100 * ls, estimativa, n_painel, est_li, est_ls)))
cat("\nMotivos:\n"); print(count(x, motivo, sort = TRUE))

# Auditoria humana: 20 espécies, por estrato, com peso maior nos casos que mudam o resultado.
set.seed(20261031)
COTAS <- tribble(
  ~situacao_painel, ~situacao_correta, ~k,
  "sem DNA",     "só SRA",      4,
  "sem DNA",     "sem DNA",     5,
  "a verificar", "com DNA",     3,
  "a verificar", "só SRA",      2,
  "a verificar", "a verificar", 1,
  "a verificar", "sem DNA",     3,
  "com DNA",     "com DNA",     2
)
aud <- purrr::pmap(COTAS, function(situacao_painel, situacao_correta, k) {
  d <- x[x$situacao_painel == situacao_painel & x$situacao_correta == situacao_correta, ]
  slice_sample(d, n = min(k, nrow(d)))
}) |> bind_rows() |> arrange(id)
gravar_csv(aud, "validacao/auditoria.csv")
msg("Auditoria: ", nrow(aud), " espécies em validacao/auditoria.csv")

# Planilha da auditoria: o julgamento da checagem assistida, os links e duas colunas a preencher.
links <- select(amostra, id, bold, genbank_organism, genbank_todos, taxonomia_ncbi)
planilha <- aud |>
  select(id, grupo, especie, situacao_painel) |>
  left_join(select(julg, id, situacao_correta, base, nome_no_banco, n_sequencias, acesso_exemplo, comentario), by = "id") |>
  left_join(links, by = "id") |>
  mutate(sra = paste0("https://www.ncbi.nlm.nih.gov/sra/?term=", utils::URLencode(paste0('"', especie, '"[Organism]'), reserved = TRUE)),
         concorda = NA_character_, situacao_auditor = NA_character_, comentario_auditor = NA_character_,
         auditor = NA_character_)
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "auditoria")
openxlsx::writeData(wb, "auditoria", planilha)
for (col in c("bold", "genbank_organism", "genbank_todos", "taxonomia_ncbi", "sra")) {
  v <- planilha[[col]]; class(v) <- "hyperlink"
  openxlsx::writeData(wb, "auditoria", x = v, startCol = match(col, names(planilha)), startRow = 2, colNames = FALSE)
}
n <- nrow(planilha)
openxlsx::dataValidation(wb, "auditoria", cols = match("concorda", names(planilha)), rows = 2:(n + 1),
                         type = "list", value = '"sim,não,em parte"')
openxlsx::dataValidation(wb, "auditoria", cols = match("situacao_auditor", names(planilha)), rows = 2:(n + 1),
                         type = "list", value = '"com DNA,só SRA,sem DNA,a verificar"')
openxlsx::addStyle(wb, "auditoria", openxlsx::createStyle(fgFill = "#fff3d6"), rows = 1:(n + 1),
                   cols = match("concorda", names(planilha)):ncol(planilha), gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, "auditoria", openxlsx::createStyle(textDecoration = "bold"), rows = 1,
                   cols = seq_along(planilha), stack = TRUE)
openxlsx::freezePane(wb, "auditoria", firstActiveRow = 2, firstActiveCol = 4)
openxlsx::setColWidths(wb, "auditoria", cols = seq_along(planilha), widths = "auto")
openxlsx::setColWidths(wb, "auditoria", cols = match("comentario", names(planilha)), widths = 50)
openxlsx::saveWorkbook(wb, "validacao/auditoria.xlsx", overwrite = TRUE)
