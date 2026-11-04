#!/usr/bin/env Rscript
# Checagem independente da amostra de validação (validacao/amostra.csv).
#
# Junta evidências por caminhos diferentes dos do pipeline, para a conferência
# não repetir os mesmos erros:
#   - Taxonomia do NCBI: nomes que o NCBI conhece (sinônimos, grafias) e o taxid;
#     contagem no nuccore por taxid (txid[Organism:exp]), no SRA e no BioSample.
#   - nuccore em todos os campos: o nome da lista, os sinônimos e os nomes do NCBI
#     como frase; e o epíteto em registros do gênero ou da família ("sp.", "cf.").
#   - BOLD: contagem por nome exato (lista, sinônimos, nomes do NCBI) e, no gênero
#     de cada nome, os nomes do BOLD com a mesma raiz do epíteto.
#   - Para as "com DNA": título e organismo dos primeiros registros do GenBank.
#
# Uso: Rscript scripts/confere_amostra.R
# Saída: validacao/checagem.csv (uma linha por espécie, evidências em texto).

for (f in c("utils", "genbank")) source(file.path("R", paste0(f, ".R")), encoding = "UTF-8")

EUTILS <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
BOLD <- "https://portal.boldsystems.org/api/summary"

eutil <- function(rota, ...) {
  espera <- ncbi_intervalo() - (as.numeric(Sys.time()) - .ncbi_ultimo$t)
  if (espera > 0) Sys.sleep(espera)
  q <- list(..., tool = projeto_slug())
  if (nzchar(ncbi_chave())) q$api_key <- ncbi_chave()
  resp <- req_base(paste0(EUTILS, rota)) |> req_url_query(!!!q) |> req_perform()
  .ncbi_ultimo$t <- as.numeric(Sys.time())
  resp
}

ids_taxonomia <- function(nome) {
  j <- eutil("esearch.fcgi", db = "taxonomy", term = paste0('"', nome, '"[All Names]'), retmode = "json") |>
    resp_body_json()
  unlist(j$esearchresult$idlist)
}

# Nome científico, posto e outros nomes (sinônimos, grafias erradas etc.) de cada taxid.
nomes_taxonomia <- function(ids) {
  if (!length(ids)) return(tibble(taxid = character(), nome = character(), posto = character(), outros = character()))
  x <- eutil("efetch.fcgi", db = "taxonomy", id = paste(ids, collapse = ","), retmode = "xml") |>
    resp_body_xml()
  taxa <- xml2::xml_find_all(x, "/TaxaSet/Taxon")
  tibble(
    taxid = xml2::xml_text(xml2::xml_find_first(taxa, "./TaxId")),
    nome = xml2::xml_text(xml2::xml_find_first(taxa, "./ScientificName")),
    posto = xml2::xml_text(xml2::xml_find_first(taxa, "./Rank")),
    outros = purrr::map_chr(taxa, ~ paste(unique(xml2::xml_text(xml2::xml_find_all(.x,
      "./OtherNames/Synonym | ./OtherNames/EquivalentName | ./OtherNames/Misspelling | ./OtherNames/GenbankSynonym | ./OtherNames/Includes | ./OtherNames/Name/DispName")
    )), collapse = ";"))
  )
}

titulos_nuccore <- function(termo, n = 5) {
  j <- eutil("esearch.fcgi", db = "nuccore", term = termo, retmax = n, retmode = "json") |> resp_body_json()
  ids <- unlist(j$esearchresult$idlist)
  if (!length(ids)) return(NA_character_)
  s <- eutil("esummary.fcgi", db = "nuccore", id = paste(ids, collapse = ","), retmode = "json") |> resp_body_json()
  paste(purrr::map_chr(ids, ~ paste0(s$result[[.x]]$accessionversion, ": ", s$result[[.x]]$title)), collapse = " || ")
}

bold_conta <- function(query) {
  Sys.sleep(0.4)
  j <- tryCatch(req_base(BOLD) |> req_url_query(query = query, fields = "species", reduce = "false") |>
                  req_perform() |> resp_body_json(), error = function(e) NULL)
  if (is.null(j) || is.null(j$species)) return(integer())
  unlist(j$species)
}

raiz <- function(epiteto) substr(epiteto, 1, pmax(4, nchar(epiteto) - 2))

confere <- function(especie, sinonimos, familia, situacao) {
  genero <- word(especie, 1); epiteto <- word(especie, 2)
  nomes_lista <- unique(c(especie, if (!is.na(sinonimos) && nzchar(sinonimos)) strsplit(sinonimos, ", ")[[1]]))

  # Taxonomia do NCBI
  ids <- unique(unlist(purrr::map(nomes_lista, ids_taxonomia)))
  tx <- nomes_taxonomia(ids)
  nomes_ncbi <- setdiff(unique(c(tx$nome, unlist(strsplit(tx$outros[nzchar(tx$outros)], ";")))), nomes_lista)
  nomes_ncbi <- setdiff(unique(na.omit(str_extract(nomes_ncbi, "^[A-Z][a-z-]+ [a-z-]+"))), nomes_lista)
  todos <- unique(c(nomes_lista, nomes_ncbi))

  # GenBank e afins
  gb_txid <- if (length(ids)) ncbi_conta(paste0("(", paste0("txid", ids, "[Organism:exp]", collapse = " OR "), ")")) else 0L
  sra <- if (length(ids)) ncbi_conta(paste0("(", paste0("txid", ids, "[Organism:exp]", collapse = " OR "), ")"), db = "sra") else 0L
  biosample <- if (length(ids)) ncbi_conta(paste0("(", paste0("txid", ids, "[Organism:exp]", collapse = " OR "), ")"), db = "biosample") else 0L
  gb_frase <- ncbi_conta(paste0("(", paste0('"', todos, '"', collapse = " OR "), ")"))
  termo_ep <- paste0(epiteto, "[All Fields] AND (", genero, "[Organism] OR ", familia, "[Organism]) NOT \"",
                     especie, "\"[Organism]")
  gb_epiteto <- ncbi_conta(termo_ep)
  titulos_ep <- if (isTRUE(gb_epiteto > 0) && situacao != "com DNA") titulos_nuccore(termo_ep) else NA_character_
  titulos_com <- if (situacao == "com DNA") titulos_nuccore(paste0('"', especie, '"[Organism]')) else NA_character_

  # BOLD: nome exato e parecidos no gênero
  n_exato <- purrr::map_int(todos, ~ sum(bold_conta(paste0("tax:species:", .x))))
  generos <- unique(word(todos, 1))
  no_genero <- unlist(purrr::map(generos, ~ bold_conta(paste0("tax:genus:", .x))))
  parecidos <- no_genero[str_detect(names(no_genero), fixed(raiz(epiteto))) & !names(no_genero) %in% todos]

  tibble(
    ncbi_taxid = paste(ids, collapse = ";"),
    ncbi_posto = paste(unique(tx$posto), collapse = ";"),
    ncbi_nomes_extra = paste(nomes_ncbi, collapse = "; "),
    gb_txid, gb_frase, gb_epiteto_no_genero = gb_epiteto, sra, biosample,
    bold_exato = paste(paste0(todos, "=", n_exato), collapse = "; "),
    bold_total = sum(n_exato),
    bold_parecidos = if (length(parecidos)) paste(paste0(names(parecidos), "=", parecidos), collapse = "; ") else NA_character_,
    gb_titulos_epiteto = titulos_ep,
    gb_titulos = titulos_com
  )
}

amostra <- ler_csv("validacao/amostra.csv", col_types = cols(.default = col_character()))
limite <- as.integer(commandArgs(TRUE)[1])
if (!is.na(limite)) amostra <- slice(amostra, c(1:limite, 101:(100 + limite)))
msg("Conferindo ", nrow(amostra), " espécies (chave NCBI: ", nzchar(ncbi_chave()), ")")
res <- purrr::pmap(select(amostra, especie, sinonimos_buscados, familia, situacao_painel), function(especie, sinonimos_buscados, familia, situacao_painel) {
  r <- tryCatch(confere(especie, sinonimos_buscados, familia, situacao_painel),
                error = function(e) tibble(erro = conditionMessage(e)))
  msg("  ", especie)
  r
}) |> bind_rows()
out <- bind_cols(select(amostra, id, grupo, familia, especie, situacao_painel, observacao, sinonimos_buscados, n_bold, n_genbank), res) |>
  mutate(conferido_em = hoje())
gravar_csv(out, "validacao/checagem.csv")
msg("Pronto: validacao/checagem.csv")
