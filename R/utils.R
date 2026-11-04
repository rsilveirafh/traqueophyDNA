# Utilidades comuns: caminhos, HTTP com retry, leitura/escrita de CSV.

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# Identificação nas APIs (NCBI pede `tool` e `email`). O slug vem do nome do
# repositório no GitHub Actions; localmente, do nome da pasta.
projeto_slug <- function() {
  repo <- Sys.getenv("GITHUB_REPOSITORY")
  if (nzchar(repo)) return(basename(repo))
  basename(normalizePath("."))
}

user_agent_txt <- function() {
  paste0(projeto_slug(), " (painel de lacunas de DNA; https://github.com/rsilveirafh)")
}

hoje <- function() format(Sys.time(), "%Y-%m-%d", tz = "America/Sao_Paulo")

msg <- function(...) {
  cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
  flush.console()
}

# Requisição base: user-agent, timeout e retry exponencial em erros transitórios.
req_base <- function(url, tentativas = 5) {
  request(url) |>
    req_user_agent(user_agent_txt()) |>
    req_timeout(120) |>
    req_retry(
      max_tries = tentativas,
      is_transient = function(resp) resp_status(resp) %in% c(408, 429, 500, 502, 503, 504),
      backoff = function(i) min(60, 2^i)
    )
}

ler_csv <- function(caminho, ...) {
  if (!file.exists(caminho)) return(NULL)
  readr::read_csv(caminho, show_col_types = FALSE, progress = FALSE,
                  guess_max = 100000, ...)
}

gravar_csv <- function(x, caminho) {
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(x, caminho, na = "")
  invisible(x)
}

# Baixa uma fonte para data-raw/. `url` pode ter alternativas, tentadas em
# ordem. Se todas falharem, mantém a cópia versionada.
baixar_fonte <- function(url, destino) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(destino)))
  ok <- FALSE
  for (u in url) {
    ok <- tryCatch({
      req_base(u) |> req_perform(path = tmp)
      file.size(tmp) > 0
    }, error = function(e) {
      msg("  aviso: falha ao baixar ", u, " (", conditionMessage(e), ")")
      FALSE
    })
    if (ok) break
  }
  if (ok) {
    file.copy(tmp, destino, overwrite = TRUE)
  } else if (!file.exists(destino)) {
    stop("Fonte indisponível e sem cópia local: ", destino)
  } else {
    msg("  usando a cópia local de ", destino)
  }
  destino
}

# Normaliza binômio: só gênero + epíteto, espaços simples.
binomio <- function(genero, epiteto) {
  str_squish(paste(str_trim(genero), str_trim(epiteto)))
}

sem_acento <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  tolower(str_squish(x))
}

extrai_ano <- function(x) {
  as.integer(str_extract(x, "(1[789]|20)\\d{2}"))
}

UFS <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT",
         "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO",
         "RR", "SC", "SP", "SE", "TO")

junta_ufs <- function(x) {
  x <- intersect(UFS, unique(x[!is.na(x)]))
  if (length(x) == 0) NA_character_ else paste(sort(x), collapse = ";")
}
