#!/usr/bin/env Rscript
# Pipeline completo: listas -> CTFB -> BOLD -> GenBank -> SRA -> cobertura -> fotos.
#
# Uso (a partir da raiz do repositório):
#   Rscript scripts/atualiza.R
#   Rscript scripts/atualiza.R --max-genbank=500 --max-minutos=60
#   Rscript scripts/atualiza.R --grupos-genbank=anfibios,repteis --sem-baixar
#
# Variáveis de ambiente: NCBI_API_KEY (opcional, 10 req/s), NCBI_EMAIL (opcional).

for (f in c("utils", "listas", "ameaca", "ctfb", "gbif", "bold", "genbank", "sra", "junta", "fotos")) {
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
grupos_gb <- arg("grupos-genbank", paste(GRUPOS$grupo, collapse = ","))
grupos_gb <- strsplit(grupos_gb, ",")[[1]]
baixar <- !isTRUE(arg("sem-baixar", FALSE))

avisos <- character()

# 1. Listas (todas, sempre: é barato) ------------------------------------------
especies <- monta_listas(GRUPOS$grupo, baixar = baixar) |> aplica_ameaca_mma2026()

# 2. CTFB: sinônimos, UF, endemismo, ano ----------------------------------------
ctfb <- tryCatch(atualiza_ctfb(GRUPOS$classe), error = function(e) {
  avisos <<- c(avisos, paste("CTFB indisponível:", conditionMessage(e))); NULL
})
if (!is.null(ctfb)) {
  especies <- enriquece_ctfb(especies, ctfb)
} else {
  especies <- mutate(especies, ctfb_situacao = "ausente", id_ctfb = NA_character_,
                     nome_aceito_ctfb = NA_character_, sinonimos = sinonimos_lista)
}

# 3. BOLD ------------------------------------------------------------------------
bold <- atualiza_bold(GRUPOS$classe)
if (isTRUE(attr(bold, "aviso"))) avisos <- c(avisos, "BOLD: usando o último resultado salvo")

# 3b. GBIF: sinônimos das candidatas a órfã (sem BOLD sob o nome e sem GenBank
#     sob o nome no último resultado salvo) -------------------------------------
gb_ant <- ler_csv("data/genbank.csv", col_types = cols(.default = col_character()))
sem_bold <- especies |>
  left_join(select(bold, classe, especie = nome, nb = n_bold), by = c("classe", "especie")) |>
  filter(coalesce(nb, 0L) == 0)
candidatas <- if (is.null(gb_ant)) sem_bold else
  filter(sem_bold, !especie %in% gb_ant$especie[as.integer(gb_ant$n_genbank_nome) > 0])
gbif <- tryCatch(atualiza_gbif(select(candidatas, especie, classe, familia)), error = function(e) {
  avisos <<- c(avisos, paste("GBIF indisponível:", conditionMessage(e)))
  ler_csv("data/gbif_sinonimos.csv", col_types = cols(.default = col_character()))
})
if (!is.null(gbif)) especies <- soma_sinonimos_gbif(especies, gbif)
gravar_csv(select(especies, -any_of("nome_aceito_ctfb")), "data/especies.csv")
msg("Espécies: ", nrow(especies))
bold_cob <- cobre_bold(especies, bold)

# 4. GenBank (incremental) --------------------------------------------------------
genbank <- atualiza_genbank(filter(especies, grupo %in% grupos_gb),
                            max_consultas = max_genbank, max_minutos = max_minutos)
genbank <- filter(genbank, especie %in% especies$especie)

# 5. Gêneros das possíveis órfãs (sinal de falsa órfã) -----------------------------
provis <- especies |>
  left_join(select(bold_cob, especie, n_bold), by = "especie") |>
  left_join(select(genbank, especie, n_genbank), by = "especie") |>
  filter(n_bold == 0, !is.na(n_genbank), n_genbank == 0)
generos <- atualiza_generos(provis$genero)

# 5b. SRA: leituras brutas das que seguem sem sequência montada (só marca) ---------
sem_montada <- especies |>
  left_join(select(bold_cob, especie, n_bold_comb), by = "especie") |>
  left_join(select(genbank, especie, n_genbank_comb), by = "especie") |>
  filter(coalesce(n_bold_comb, 0L) == 0, coalesce(n_genbank_comb, 0L) == 0)
sra <- tryCatch(atualiza_sra(sem_montada, max_minutos = max_minutos), error = function(e) {
  avisos <<- c(avisos, paste("SRA indisponível:", conditionMessage(e)))
  ler_csv("data/sra.csv", col_types = cols(n_sra = col_integer(), .default = col_character()))
})

# 6. Junta, histórico e eventos -----------------------------------------------------
cob_antiga <- ler_csv("data/cobertura.csv", col_types = cols(.default = col_character()))
cob <- monta_cobertura(especies, bold_cob, genbank, generos, sra)
atualiza_eventos(cob, cob_antiga)
gravar_csv(cob, "data/cobertura.csv")
atualiza_historico(cob)

# 7. Fotos (iNaturalist) das espécies que não estão "com DNA" -----------------------
tryCatch(atualiza_fotos(cob$especie[cob$situacao != "com DNA"], max_minutos = 20),
         error = function(e) avisos <<- c(avisos, paste("iNaturalist indisponível:", conditionMessage(e))))

pendentes <- sum(is.na(cob$n_genbank))
if (pendentes > 0) avisos <- c(avisos, paste0("GenBank: ", pendentes, " espécies ainda não consultadas"))
status <- list(
  atualizado_em = hoje(),
  especies = nrow(cob),
  genbank_pendentes = pendentes,
  ncbi_chave = nzchar(ncbi_chave()),
  avisos = as.list(avisos)
)
jsonlite::write_json(status, "data/status.json", auto_unbox = TRUE, pretty = TRUE)

print(count(cob, grupo, situacao) |> tidyr::pivot_wider(names_from = situacao, values_from = n))
msg("Pronto.")
