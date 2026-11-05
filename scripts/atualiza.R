#!/usr/bin/env Rscript
# Pipeline completo: lista (Flora e Funga) -> ameaça (MMA) -> WCVP -> taxonomia do
# NCBI -> BOLD -> GenBank -> SRA -> cobertura -> fotos.
#
# Uso (a partir da raiz do repositório):
#   Rscript scripts/atualiza.R
#   Rscript scripts/atualiza.R --so-lista            # só data/especies.csv
#   Rscript scripts/atualiza.R --max-genbank=500 --max-minutos=60 --sem-baixar
#
# Variáveis de ambiente: NCBI_API_KEY (opcional, 10 req/s), NCBI_EMAIL (opcional).

for (f in c("utils", "listas", "ameaca", "wcvp", "taxdump", "bold", "genbank", "sra", "junta", "fotos")) {
  source(file.path("R", paste0(f, ".R")), encoding = "UTF-8")
}

args <- commandArgs(trailingOnly = TRUE)
arg <- function(nome, padrao = NULL) {
  a <- grep(paste0("^--", nome, "(=|$)"), args, value = TRUE)
  if (!length(a)) return(padrao)
  v <- sub(paste0("^--", nome, "=?"), "", a[1])
  if (v == "") TRUE else v
}
max_genbank <- as.numeric(arg("max-genbank", Inf))
max_minutos <- as.numeric(arg("max-minutos", Inf))
# `--max-minutos` é o prazo da execução inteira: o GenBank fica com até 85% do que
# resta, o SRA com o que sobrar e as fotos com até 20 min.
prazo <- Sys.time() + max_minutos * 60
resta <- function() max(1, as.numeric(difftime(prazo, Sys.time(), units = "mins")))
so_lista <- isTRUE(arg("so-lista", FALSE))
grupos_gb <- arg("grupos-genbank", paste(GRUPOS$grupo, collapse = ","))
grupos_gb <- strsplit(grupos_gb, ",")[[1]]
baixar <- !isTRUE(arg("sem-baixar", FALSE))

avisos <- character()

# 1. Lista e ameaça (sempre: é barato) --------------------------------------------
especies <- monta_listas(baixar = baixar) |> aplica_ameaca(baixar = baixar)

# 2. WCVP: sinônimos globais e ano de descrição -----------------------------------
wcvp <- tryCatch(atualiza_wcvp(especies), error = function(e) {
  avisos <<- c(avisos, paste("WCVP indisponível:", conditionMessage(e)))
  ler_csv("data/wcvp.csv", col_types = cols(.default = col_character()))
})
if (!is.null(wcvp)) {
  especies <- soma_wcvp(especies, wcvp)
} else {
  especies <- mutate(especies, sinonimos = sinonimos_lista, sinonimos_ffb = sinonimos_lista,
                     wcvp_situacao = NA_character_, wcvp_junta_com = NA_character_)
}
gravar_csv(select(especies, -any_of(c("sinonimos", "sinonimos_ffb", "wcvp_situacao", "wcvp_junta_com"))),
           "data/especies.csv")
msg("Espécies: ", nrow(especies))
print(count(especies, grupo))
if (so_lista) quit(save = "no")

# 3. Taxonomia do NCBI: quais nomes podem ter registro no GenBank e no SRA ---------
nomes_ncbi <- tryCatch(atualiza_taxdump(especies), error = function(e) {
  avisos <<- c(avisos, paste("taxdump do NCBI indisponível:", conditionMessage(e)))
  ler_csv("data/ncbi_taxonomia.csv", col_types = cols(.default = col_character()))
})
if (is.null(nomes_ncbi)) stop("taxdump do NCBI indisponível e sem cópia local")
nomes_ncbi <- limpa_nomes_ncbi(nomes_ncbi, especies)

# 4. BOLD ------------------------------------------------------------------------
bold <- atualiza_bold()
if (isTRUE(attr(bold, "aviso"))) avisos <- c(avisos, "BOLD: usando o último resultado salvo")
bold_cob <- cobre_bold(especies, bold)

# 5. GenBank (incremental) --------------------------------------------------------
genbank <- atualiza_genbank(filter(especies, grupo %in% grupos_gb), nomes_ncbi,
                            max_consultas = max_genbank, max_minutos = 0.85 * resta())
genbank <- filter(genbank, especie %in% especies$especie)

# 6. Gêneros das possíveis órfãs (nota de falsa órfã) -----------------------------
provis <- especies |>
  left_join(select(bold_cob, especie, n_bold), by = "especie") |>
  left_join(select(genbank, especie, n_genbank), by = "especie") |>
  filter(n_bold == 0, !is.na(n_genbank), n_genbank == 0)
generos <- atualiza_generos(provis$genero)

# 6b. SRA: leituras brutas das que seguem sem sequência montada (só marca) ---------
sem_montada <- especies |>
  left_join(select(bold_cob, especie, n_bold_ffb), by = "especie") |>
  left_join(select(genbank, especie, n_genbank_ffb), by = "especie") |>
  # só quem o GenBank já consultou (as pendentes entram quando forem consultadas)
  filter(coalesce(n_bold_ffb, 0L) == 0, !is.na(n_genbank_ffb), n_genbank_ffb == 0)
sra <- tryCatch(atualiza_sra(sem_montada, nomes_ncbi, max_minutos = 0.7 * resta()), error = function(e) {
  avisos <<- c(avisos, paste("SRA indisponível:", conditionMessage(e)))
  ler_csv("data/sra.csv", col_types = cols(n_sra = col_integer(), .default = col_character()))
})

# 7. Junta, histórico e eventos -----------------------------------------------------
cob_antiga <- ler_csv("data/cobertura.csv", col_types = cols(.default = col_character()))
cob <- monta_cobertura(especies, bold_cob, genbank, generos, sra, nomes_ncbi)
atualiza_eventos(cob, cob_antiga)
gravar_csv(cob, "data/cobertura.csv")
grava_site(cob)
atualiza_historico(cob)

# 8. Fotos (iNaturalist) das espécies que não estão "com DNA" -----------------------
#    Só as já consultadas no GenBank; as sem DNA primeiro (~1 consulta/s no iNaturalist).
alvo_fotos <- cob |>
  filter(situacao != "com DNA", !is.na(n_genbank)) |>
  arrange(situacao != "sem DNA") |>
  pull(especie)
tryCatch(atualiza_fotos(alvo_fotos, max_minutos = min(20, resta())),
         error = function(e) avisos <<- c(avisos, paste("iNaturalist indisponível:", conditionMessage(e))))

pendentes <- sum(is.na(cob$n_genbank))
if (pendentes > 0) avisos <- c(avisos, paste0("GenBank: ", pendentes, " espécies ainda não consultadas"))
status <- list(
  atualizado_em = hoje(),
  especies = nrow(cob),
  lista = cob$versao_lista[1],
  genbank_pendentes = pendentes,
  ncbi_chave = nzchar(ncbi_chave()),
  avisos = as.list(avisos)
)
jsonlite::write_json(status, "data/status.json", auto_unbox = TRUE, pretty = TRUE)

print(count(cob, grupo, situacao) |> tidyr::pivot_wider(names_from = situacao, values_from = n))
msg("Pronto.")
