# NCBI GenBank (E-utilities) -----------------------------------------------------
#
# Contagem de registros no `nuccore` por espécie:
#   esearch.fcgi?db=nuccore&term="Boana faber"[Organism]&rettype=count&retmode=json
# `[Organism]` usa a taxonomia do NCBI, que já resolve os sinônimos que ela
# conhece. Os sinônimos do CTFB entram com OR na mesma consulta (contagem sem
# dupla contagem). Limite: 3 req/s sem chave; 10 req/s com `NCBI_API_KEY`.
#
# Cache incremental em data/genbank.csv: a cada execução consulta primeiro as
# espécies novas (ou cujo termo de busca mudou), depois as que estavam com 0,
# depois as mais antigas, até `max_consultas` ou `max_minutos`.

NCBI_URL <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

ncbi_chave <- function() Sys.getenv("NCBI_API_KEY")

ncbi_intervalo <- function() if (nzchar(ncbi_chave())) 1 / 8 else 1 / 2.5

.ncbi_ultimo <- new.env()
.ncbi_ultimo$t <- 0

termo_ncbi <- function(nomes) {
  paste0("(", paste0("\"", nomes, "\"[Organism]", collapse = " OR "), ")")
}

ncbi_conta <- function(termo, db = "nuccore") {
  espera <- ncbi_intervalo() - (as.numeric(Sys.time()) - .ncbi_ultimo$t)
  if (espera > 0) Sys.sleep(espera)
  q <- list(db = db, term = termo, rettype = "count", retmode = "json",
            tool = projeto_slug())
  if (nzchar(Sys.getenv("NCBI_EMAIL"))) q$email <- Sys.getenv("NCBI_EMAIL")
  if (nzchar(ncbi_chave())) q$api_key <- ncbi_chave()
  res <- tryCatch({
    resp <- req_base(NCBI_URL) |> req_url_query(!!!q) |> req_perform()
    j <- resp_body_json(resp)
    n <- j$esearchresult$count
    if (is.null(n)) NA_integer_ else as.integer(n)
  }, error = function(e) {
    msg("  aviso NCBI: ", conditionMessage(e))
    NA_integer_
  })
  .ncbi_ultimo$t <- as.numeric(Sys.time())
  res
}

atualiza_genbank <- function(especies, destino = "data/genbank.csv",
                             max_consultas = Inf, max_minutos = Inf,
                             dias_validade = 28, salvar_cada = 100) {
  alvo <- especies |>
    mutate(nomes = map2(especie, sinonimos, function(e, s) unique(c(e, if (!is.na(s)) strsplit(s, ";")[[1]]))),
           termo = map_chr(nomes, termo_ncbi)) |>
    select(especie, termo, nomes)
  cache <- ler_csv(destino, col_types = cols(n_genbank = col_integer(),
                                              n_genbank_nome = col_integer(),
                                              n_genbank_comb = col_integer(),
                                              n_genbank_texto = col_integer(),
                                              .default = col_character()))
  if (!is.null(cache) && !"n_genbank_comb" %in% names(cache)) {
    cache$n_genbank_comb <- ifelse(cache$n_genbank == cache$n_genbank_nome, cache$n_genbank_nome, NA)
  }
  if (is.null(cache)) {
    cache <- tibble(especie = character(), termo = character(), n_genbank = integer(),
                    n_genbank_nome = integer(), n_genbank_comb = integer(),
                    n_genbank_texto = integer(), consultado_em = character())
  }
  fila <- alvo |>
    left_join(select(cache, especie, termo_ant = termo, n_ant = n_genbank,
                     comb_ant = n_genbank_comb, data_ant = consultado_em), by = "especie") |>
    mutate(
      idade = as.numeric(as.Date(hoje()) - as.Date(data_ant)),
      prioridade = case_when(
        is.na(n_ant) | is.na(comb_ant) | is.na(termo_ant) | termo_ant != termo ~ 1,  # nova, falhou ou mudou
        n_ant == 0 ~ 2,                                          # órfã: rechecar sempre
        idade >= dias_validade ~ 3,                              # vencida
        TRUE ~ 9
      )
    ) |>
    filter(prioridade < 9) |>
    arrange(prioridade, desc(idade))
  fila <- head(fila, max_consultas)
  msg("GenBank: ", nrow(fila), " consultas na fila (",
      if (nzchar(ncbi_chave())) "com" else "sem", " chave NCBI)")
  if (nrow(fila) == 0) return(cache)

  inicio <- Sys.time()
  novos <- vector("list", nrow(fila))
  grava <- function(k) {
    feitos <- bind_rows(novos[seq_len(k)])
    feitos <- filter(feitos, !is.na(n_genbank))
    out <- cache |> filter(!especie %in% feitos$especie) |> bind_rows(feitos) |>
      filter(especie %in% alvo$especie) |> arrange(especie)
    gravar_csv(out, destino)
    out
  }
  for (i in seq_len(nrow(fila))) {
    nomes <- fila$nomes[[i]]
    n <- ncbi_conta(fila$termo[i])
    n_nome <- if (length(nomes) > 1 && isTRUE(n > 0)) ncbi_conta(termo_ncbi(nomes[1])) else n
    # mesma espécie em outra combinação (transferência de gênero)
    comb <- c(nomes[1], mesmo_epiteto(nomes[1], nomes[-1]))
    n_comb <- if (length(comb) > 1 && isTRUE(n > n_nome)) {
      if (length(comb) == length(nomes)) n else ncbi_conta(termo_ncbi(comb))
    } else n_nome
    # sem nada no campo Organism: o nome aparece em outro campo (nota,
    # subespécie, identificação antiga)? Sinal de possível falsa órfã.
    n_texto <- if (isTRUE(n == 0)) ncbi_conta(paste0("\"", nomes[1], "\"")) else NA_integer_
    novos[[i]] <- tibble(especie = fila$especie[i], termo = fila$termo[i],
                         n_genbank = n, n_genbank_nome = n_nome, n_genbank_comb = n_comb,
                         n_genbank_texto = n_texto, consultado_em = hoje())
    if (i %% salvar_cada == 0) {
      cache_parcial <- grava(i)
      msg("  ", i, "/", nrow(fila))
    }
    if (as.numeric(difftime(Sys.time(), inicio, units = "mins")) > max_minutos) {
      msg("  limite de tempo atingido; o resto fica para a próxima execução")
      return(grava(i))
    }
  }
  grava(nrow(fila))
}

# Para as órfãs: quantos registros o GÊNERO tem no GenBank (sinal de possível
# "falsa órfã"). Cache em data/genbank_generos.csv, validade de 90 dias.
atualiza_generos <- function(generos, destino = "data/genbank_generos.csv", dias_validade = 90) {
  cache <- ler_csv(destino, col_types = cols(n_genbank_genero = col_integer(),
                                              .default = col_character()))
  if (is.null(cache)) cache <- tibble(genero = character(), n_genbank_genero = integer(),
                                       consultado_em = character())
  generos <- unique(generos)
  vencidos <- cache |>
    filter(as.numeric(as.Date(hoje()) - as.Date(consultado_em)) >= dias_validade) |>
    pull(genero)
  falta <- union(setdiff(generos, cache$genero), intersect(generos, vencidos))
  msg("GenBank (gêneros das órfãs): ", length(falta), " consultas")
  if (length(falta)) {
    novos <- tibble(genero = falta,
                    n_genbank_genero = map_int(falta, ~ ncbi_conta(paste0("\"", .x, "\"[Organism]"))),
                    consultado_em = hoje()) |>
      filter(!is.na(n_genbank_genero))
    cache <- cache |> filter(!genero %in% novos$genero) |> bind_rows(novos) |> arrange(genero)
    gravar_csv(cache, destino)
  }
  cache
}
