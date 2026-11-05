# Pré-filtro pela taxonomia do NCBI (taxdump) -----------------------------------
#
# Todo registro do GenBank tem um táxon na taxonomia do NCBI. Um nome que não está
# em `names.dmp` (como nome científico, sinônimo, nome equivalente ou "includes")
# não tem registro sob ele no campo [Organism]. Com ~35 mil espécies e ~68 mil
# nomes alternativos, a busca por nome no GenBank passaria de 100 mil chamadas: só
# as espécies com algum nome na taxonomia do NCBI vão para a consulta de contagem.
#
#   https://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump.tar.gz (~80 MB)
#   names.dmp: tax_id | nome | nome único | classe |
#   nodes.dmp: tax_id | tax_id do pai | nível | ...
#
# Só a subárvore de Viridiplantae (33090), para não casar homônimos de animais.
# Resultado em data/ncbi_taxonomia.csv (1 linha por nome da lista presente no NCBI).

TAXDUMP_URL <- "https://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump.tar.gz"
VIRIDIPLANTAE <- 33090L
CLASSES_NCBI <- c("scientific name", "synonym", "equivalent name", "includes",
                  "genbank synonym", "misspelling")

ler_dmp <- function(arq, colunas, nomes) {
  # campos separados por "\t|\t"; lidos como TSV, as colunas úteis são as ímpares
  x <- readr::read_tsv(arq, col_names = FALSE, quote = "", na = character(),
                       col_types = cols(.default = col_character()),
                       col_select = all_of(colunas), progress = FALSE)
  names(x) <- nomes
  x
}

# tax_ids de Viridiplantae: sobe a árvore a partir de cada nó até achar 33090 ou a raiz.
subarvore <- function(nodes, raiz) {
  id <- as.integer(nodes$tax_id)
  pai <- as.integer(nodes$pai)
  idx_pai <- match(pai, id)
  dentro <- rep(NA, length(id))
  dentro[id == raiz] <- TRUE
  dentro[id == 1L] <- FALSE
  atual <- seq_along(id)
  for (k in 1:100) {
    pend <- which(is.na(dentro))
    if (!length(pend)) break
    estado <- dentro[idx_pai[atual[pend]]]
    resolvido <- !is.na(estado)
    dentro[pend[resolvido]] <- estado[resolvido]
    atual[pend[!resolvido]] <- idx_pai[atual[pend[!resolvido]]]
  }
  id[dentro %in% TRUE]
}

# Impressão digital do conjunto de nomes buscados: se a lista ou os sinônimos
# mudam, o cruzamento é refeito mesmo dentro da validade.
digital_nomes <- function(especies) {
  n <- sort(unique(c(especies$especie, unlist(strsplit(na.omit(especies$sinonimos), ";")))))
  tmp <- tempfile()
  writeLines(n, tmp, useBytes = TRUE)
  on.exit(unlink(tmp))
  unname(tools::md5sum(tmp))
}

atualiza_taxdump <- function(especies, destino = "data/ncbi_taxonomia.csv", dias_validade = 7,
                             arq_digital = "data/cache/ncbi_taxonomia.md5") {
  ant <- ler_csv(destino, col_types = cols(.default = col_character()))
  dig <- digital_nomes(especies)
  dig_ant <- if (file.exists(arq_digital)) readLines(arq_digital, warn = FALSE)[1] else ""
  if (!is.null(ant) && nrow(ant) && identical(dig, dig_ant) &&
      as.numeric(as.Date(hoje()) - as.Date(ant$consultado_em[1])) < dias_validade) {
    msg("NCBI taxonomia: usando ", destino, " de ", ant$consultado_em[1])
    return(ant)
  }
  pasta <- file.path(tempdir(), "taxdump")
  dir.create(pasta, showWarnings = FALSE)
  tgz <- file.path(pasta, "taxdump.tar.gz")
  msg("NCBI taxonomia: baixando o taxdump")
  req_base(TAXDUMP_URL) |> req_timeout(1200) |> req_perform(path = tgz)
  utils::untar(tgz, files = c("names.dmp", "nodes.dmp"), exdir = pasta)

  nodes <- ler_dmp(file.path(pasta, "nodes.dmp"), c(1, 3), c("tax_id", "pai"))
  plantas <- subarvore(nodes, VIRIDIPLANTAE)
  rm(nodes)
  nm <- ler_dmp(file.path(pasta, "names.dmp"), c(1, 3, 7), c("tax_id", "nome", "classe")) |>
    filter(classe %in% CLASSES_NCBI, as.integer(tax_id) %in% plantas)
  cient <- nm |> filter(classe == "scientific name") |> select(tax_id, nome_ncbi = nome)
  msg("  ", length(plantas), " táxons e ", nrow(nm), " nomes de Viridiplantae")

  nomes <- especies |>
    select(especie, sinonimos) |>
    mutate(nome = map2(especie, sinonimos, ~ unique(c(.x, if (!is.na(.y)) strsplit(.y, ";")[[1]])))) |>
    select(especie, nome) |>
    unnest(nome)
  out <- nomes |>
    inner_join(distinct(nm, nome, tax_id, classe), by = "nome", relationship = "many-to-many") |>
    left_join(cient, by = "tax_id") |>
    group_by(especie, nome) |>
    summarise(tax_id = paste(unique(tax_id), collapse = ";"),
              classe_ncbi = paste(unique(classe), collapse = ";"),
              nome_ncbi = paste(unique(nome_ncbi), collapse = ";"), .groups = "drop") |>
    mutate(consultado_em = hoje()) |>
    arrange(especie, nome)
  unlink(pasta, recursive = TRUE)
  msg("  ", n_distinct(out$especie), " de ", nrow(especies), " espécies com algum nome na taxonomia do NCBI")
  gravar_csv(out, destino)
  writeLines(dig, arq_digital)
  out
}

# Nome que, na taxonomia do NCBI, é de outra espécie aceita na Flora e Funga (o NCBI
# o trata como sinônimo dela) sai da busca: as sequências sob ele são da outra
# espécie (validação, 2026-11-05: Pleurothallis picta -> Specklinia picta). Também
# saem os nomes que deixaram de ser buscados (sinônimos filtrados em soma_wcvp).
limpa_nomes_ncbi <- function(nomes_ncbi, especies) {
  buscados <- especies |>
    transmute(especie, nome = map2(especie, sinonimos, ~ c(.x, if (!is.na(.y)) strsplit(.y, ";")[[1]]))) |>
    unnest(nome)
  x <- semi_join(nomes_ncbi, buscados, by = c("especie", "nome"))
  outra <- x |>
    mutate(sci = strsplit(coalesce(nome_ncbi, ""), ";")) |>
    unnest(sci) |>
    filter(nome != especie, sci != especie, sci %in% especies$especie) |>
    distinct(especie, nome)
  if (nrow(outra)) msg("NCBI taxonomia: ", nrow(outra), " nome(s) fora da busca (no NCBI são de outra espécie da lista)")
  anti_join(x, outra, by = c("especie", "nome"))
}
