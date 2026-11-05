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
# Em plantas, as decisões que pedem olho de botânico são as das "a verificar"
# (sinônimo de outra espécie aceita, homônimo, conceito amplo do WCVP).
set.seed(20261105)
x <- x |> mutate(estrato = case_when(
  situacao_painel == "a verificar" & situacao_correta == "sem DNA" &
    motivo %in% c("sequências de outra espécie aceita na Flora e Funga") ~ "av_outra_especie",
  situacao_painel == "a verificar" & situacao_correta == "sem DNA" &
    motivo %in% c("homônimo", "conceito amplo, material de fora") ~ "av_homonimo_conceito",
  situacao_painel == "a verificar" ~ paste0("av_", situacao_correta),
  TRUE ~ situacao_painel
))
COTAS <- tribble(
  ~estrato,               ~k,
  "av_com DNA",            4,
  "av_outra_especie",      3,
  "av_homonimo_conceito",  3,
  "av_a verificar",        2,
  "av_sem DNA",            2,
  "sem DNA",               4,
  "com DNA",               2
)
aud <- purrr::pmap(COTAS, function(estrato, k) {
  d <- x[x$estrato == estrato, ]
  slice_sample(d, n = min(k, nrow(d)))
}) |> bind_rows() |> arrange(id)
gravar_csv(aud, "validacao/auditoria.csv")
msg("Auditoria: ", nrow(aud), " espécies em validacao/auditoria.csv")

# Planilha da auditoria: uma pergunta de sim/não por espécie, a resposta da
# checagem assistida ao lado e só os links que respondem a pergunta.
url_busca <- function(nome) paste0("https://www.ncbi.nlm.nih.gov/nuccore/?term=",
                                   utils::URLencode(paste0('"', nome, '"[Organism]'), reserved = TRUE))
planilha <- aud |>
  select(id, especie) |>
  left_join(select(amostra, id, familia, situacao_painel, sinonimos_buscados, genbank_organism, flora_funga), by = "id") |>
  left_join(select(julg, id, situacao_correta, motivo, nome_no_banco, acesso_exemplo, comentario), by = "id") |>
  mutate(
    nome1 = str_trim(str_remove(coalesce(nome_no_banco, ""), "[;(].*$")),
    tipo = case_when(
      situacao_painel == "com DNA" | nome1 == especie ~ "com",
      nzchar(nome1) & !motivo %in% c("só SRA", "nome só como hospedeiro") ~ "outro_nome",
      TRUE ~ "nada"
    ),
    pergunta = case_when(
      tipo == "com" ~ paste0("As sequências de ", especie, " no GenBank/BOLD são mesmo desta espécie?"),
      tipo == "outro_nome" ~ paste0("As sequências de ", str_replace_all(nome_no_banco, "; ", " e "), " são de ", especie, "?"),
      TRUE ~ paste0("Nenhuma sequência sob ", especie,
                    ifelse(is.na(sinonimos_buscados) | sinonimos_buscados == "", "",
                           paste0(" nem sob ", sinonimos_buscados)),
                    ". Você conhece outro nome desta espécie?")
    ),
    resposta_checagem = case_when(
      tipo == "com" & situacao_painel == "com DNA" ~ "sim",
      tipo == "com" ~ paste0("sim: ", comentario),
      tipo == "outro_nome" & situacao_correta == "com DNA" ~ paste0("sim: ", comentario),
      tipo == "outro_nome" & situacao_correta == "sem DNA" ~ paste0("não: ", comentario),
      tipo == "outro_nome" ~ paste0("não sei: ", comentario),
      TRUE ~ paste0("não conheço: ", comentario)
    ),
    ver_sequencias = case_when(tipo == "outro_nome" ~ url_busca(nome1),
                               tipo == "com" ~ url_busca(especie), TRUE ~ NA_character_),
    exemplo = ifelse(is.na(acesso_exemplo) | acesso_exemplo == "", NA_character_,
                     paste0("https://www.ncbi.nlm.nih.gov/nuccore/", acesso_exemplo)),
    concorda = NA_character_, comentario_auditor = NA_character_
  ) |>
  select(id, especie, familia, pergunta, resposta_checagem, ver_sequencias, exemplo, flora_funga,
         concorda, comentario_auditor)

como <- data.frame(`Como preencher` = c(
  "Cada linha tem uma pergunta e a resposta que a checagem deu.",
  "Na coluna 'concorda', marque sim, não ou não sei. Se marcar não, escreva o porquê em 'comentario_auditor' (ex.: 'é sinônimo pro parte', 'o nome certo é X').",
  "Links: 'ver_sequencias' abre as sequências no GenBank; 'exemplo' abre um registro (veja o voucher e o país); 'flora_funga' abre a espécie na Flora e Funga.",
  "Quando a pergunta é 'você conhece outro nome desta espécie?', basta responder se a lista de nomes está completa.",
  "Leva uns 5 minutos por espécie. Não precisa buscar nada além dos links."
), check.names = FALSE)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "como preencher")
openxlsx::writeData(wb, "como preencher", como)
openxlsx::setColWidths(wb, "como preencher", cols = 1, widths = 140)
openxlsx::addStyle(wb, "como preencher", openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                   rows = 1:6, cols = 1)
openxlsx::addWorksheet(wb, "auditoria")
openxlsx::writeData(wb, "auditoria", planilha)
for (col in c("ver_sequencias", "exemplo", "flora_funga")) {
  v <- planilha[[col]]
  rotulo <- c(ver_sequencias = "ver sequências", exemplo = "ver registro", flora_funga = "ver na Flora e Funga")[[col]]
  f <- ifelse(is.na(v), "", paste0('HYPERLINK("', v, '", "', rotulo, '")'))
  class(f) <- "formula"
  openxlsx::writeData(wb, "auditoria", x = f, startCol = match(col, names(planilha)), startRow = 2, colNames = FALSE)
}
n <- nrow(planilha)
openxlsx::dataValidation(wb, "auditoria", cols = match("concorda", names(planilha)), rows = 2:(n + 1),
                         type = "list", value = '"sim,não,não sei"')
quebra <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
openxlsx::addStyle(wb, "auditoria", quebra, rows = 2:(n + 1), cols = seq_along(planilha), gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, "auditoria", openxlsx::createStyle(fgFill = "#fff3d6"), rows = 1:(n + 1),
                   cols = match("concorda", names(planilha)):ncol(planilha), gridExpand = TRUE, stack = TRUE)
openxlsx::addStyle(wb, "auditoria", openxlsx::createStyle(textDecoration = "bold"), rows = 1,
                   cols = seq_along(planilha), stack = TRUE)
openxlsx::freezePane(wb, "auditoria", firstActiveRow = 2, firstActiveCol = 3)
openxlsx::setColWidths(wb, "auditoria", cols = seq_along(planilha),
                       widths = c(6, 24, 16, 50, 50, 16, 14, 20, 11, 40))
openxlsx::saveWorkbook(wb, "validacao/auditoria.xlsx", overwrite = TRUE)
