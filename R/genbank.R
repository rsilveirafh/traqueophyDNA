# NCBI GenBank (E-utilities) -----------------------------------------------------
#
# Contagem de registros no `nuccore` por espécie:
#   esearch.fcgi?db=nuccore&term="Boana faber"[Organism]&rettype=count&retmode=json
# `[Organism]` usa a taxonomia do NCBI, que já resolve os sinônimos que ela
# conhece. Os nomes alternativos (Flora e Funga e WCVP) que existem na taxonomia
# do NCBI entram com OR na mesma consulta (contagem sem dupla contagem). Limite: 3 req/s sem chave; 10 req/s com `NCBI_API_KEY`.
#
# Cache incremental em data/genbank.csv: a cada execução consulta primeiro as
# espécies novas (ou cujo termo de busca mudou), depois as que estavam com 0,
# depois as mais antigas, até `max_consultas` ou `max_minutos`.

NCBI_URL <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"

ncbi_chave <- function() Sys.getenv("NCBI_API_KEY")

termo_ncbi <- function(nomes) {
  paste0("(", paste0("\"", nomes, "\"[Organism]", collapse = " OR "), ")")
}

# usados por scripts/confere_amostra.R (chamadas em série com pausa manual)
ncbi_intervalo <- function() if (nzchar(ncbi_chave())) 1 / 8 else 1 / 2.5
.ncbi_ultimo <- new.env()
.ncbi_ultimo$t <- 0

ncbi_req <- function(termo, db = "nuccore") {
  q <- list(db = db, term = termo, rettype = "count", retmode = "json",
            tool = projeto_slug())
  if (nzchar(Sys.getenv("NCBI_EMAIL"))) q$email <- Sys.getenv("NCBI_EMAIL")
  if (nzchar(ncbi_chave())) q$api_key <- ncbi_chave()
  # um pouco abaixo do limite (10 ou 3 req/s), compartilhado por todas as chamadas
  req <- req_base(NCBI_URL) |>
    req_throttle(rate = if (nzchar(ncbi_chave())) 9 else 2.5, realm = "eutils")
  # termos longos (muitos sinônimos) vão no corpo (POST), como pede o NCBI
  if (nchar(termo) > 1500) req_body_form(req, !!!q) else req_url_query(req, !!!q)
}

ncbi_le_conta <- function(resp) {
  n <- tryCatch(resp_body_json(resp)$esearchresult$count, error = function(e) NULL)
  if (is.null(n)) NA_integer_ else as.integer(n)
}

# Várias contagens de uma vez: requisições em paralelo, no ritmo permitido pelo
# NCBI (cada uma leva ~0,5 s; em série, o ritmo real fica em ~2 req/s).
# Falha vira NA (a espécie volta para a fila na próxima execução).
ncbi_conta_lote <- function(termos, db = "nuccore", max_ativas = 8) {
  if (!length(termos)) return(integer())
  resps <- req_perform_parallel(map(termos, ncbi_req, db = db), on_error = "continue",
                                progress = FALSE, max_active = max_ativas)
  falhas <- vapply(resps, inherits, logical(1), "error")
  if (any(falhas)) msg("  aviso NCBI: ", sum(falhas), " falha(s), por exemplo: ",
                       conditionMessage(resps[[which(falhas)[1]]]))
  vapply(resps, function(r) if (inherits(r, "error")) NA_integer_ else ncbi_le_conta(r), integer(1))
}

ncbi_conta <- function(termo, db = "nuccore") ncbi_conta_lote(termo, db = db, max_ativas = 1)

# `nomes_ncbi` (especie, nome): os nomes de cada espécie que existem na taxonomia
# do NCBI (R/taxdump.R). Só eles entram na consulta [Organism]; a espécie sem
# nenhum ganha 0 sem consulta e passa só pela busca do nome em texto livre.
atualiza_genbank <- function(especies, nomes_ncbi, destino = "data/genbank.csv",
                             max_consultas = Inf, max_minutos = Inf,
                             dias_validade = 28, dias_orfa = 6, salvar_cada = 500) {
  conhecidos <- split(nomes_ncbi$nome, nomes_ncbi$especie)
  alvo <- especies |>
    mutate(nomes = map2(especie, sinonimos, function(e, s) {
             todos <- unique(c(e, if (!is.na(s)) strsplit(s, ";")[[1]]))
             intersect(todos, conhecidos[[e]])
           }),
           termo = map2_chr(especie, nomes, function(e, n) {
             if (length(n)) termo_ncbi(n) else paste0("texto:\"", e, "\"")
           })) |>
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
        n_ant == 0 & idade >= dias_orfa ~ 2,                     # órfã: rechecar toda semana
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
  feitos <- list()
  grava <- function() {
    f <- filter(bind_rows(feitos), !is.na(n_genbank))
    out <- cache |> filter(!especie %in% f$especie) |> bind_rows(f) |>
      filter(especie %in% alvo$especie) |> arrange(especie)
    gravar_csv(out, destino)
    out
  }
  blocos <- split(seq_len(nrow(fila)), ceiling(seq_len(nrow(fila)) / salvar_cada))
  for (b in seq_along(blocos)) {
    feitos[[b]] <- conta_bloco_genbank(fila[blocos[[b]], ])
    grava()
    msg("  ", max(blocos[[b]]), "/", nrow(fila), " (",
        round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1), " min)")
    if (as.numeric(difftime(Sys.time(), inicio, units = "mins")) > max_minutos) {
      msg("  limite de tempo atingido; o resto fica para a próxima execução")
      break
    }
  }
  grava()
}

# Um bloco da fila em três ondas de chamadas paralelas:
#   1) todos os nomes conhecidos pelo NCBI (n_genbank);
#   2) só o nome da lista, quando há outros nomes e algum registro (n_genbank_nome),
#      e o nome em texto livre, quando não há nada no campo Organism (n_genbank_texto:
#      o nome aparece em nota, subespécie, identificação antiga? sinal de falsa órfã);
#   3) mesma espécie em outra combinação (transferência de gênero, variedade elevada
#      a espécie, infraespecíficos da própria espécie), quando sobra registro sob
#      outros nomes (n_genbank_comb).
conta_bloco_genbank <- function(fila) {
  e <- fila$especie
  nomes <- fila$nomes
  tem <- lengths(nomes) > 0
  proprio <- map2_lgl(e, nomes, `%in%`)

  n <- rep(0L, length(e))
  n[tem] <- ncbi_conta_lote(fila$termo[tem])

  q_nome <- proprio & lengths(nomes) > 1 & !is.na(n) & n > 0
  q_texto <- n %in% 0L
  r2 <- ncbi_conta_lote(c(termo_ncbi_vec(e[q_nome]), paste0("\"", e[q_texto], "\"")))
  n_nome <- ifelse(proprio, n, 0L)
  n_nome[q_nome] <- r2[seq_len(sum(q_nome))]
  n_texto <- rep(NA_integer_, length(e))
  n_texto[q_texto] <- r2[sum(q_nome) + seq_len(sum(q_texto))]

  comb <- map2(e, nomes, function(x, nm) intersect(c(x, mesmo_epiteto(x, setdiff(nm, x))), nm))
  n_comb <- ifelse(lengths(comb) == 0, 0L, n_nome)
  mais <- lengths(comb) > 0 & !is.na(n) & !is.na(n_nome) & n > n_nome
  igual_todos <- mais & lengths(comb) == lengths(nomes)
  n_comb[igual_todos] <- n[igual_todos]
  so_proprio <- map2_lgl(comb, e, identical)
  q_comb <- mais & !igual_todos & !so_proprio
  n_comb[q_comb] <- ncbi_conta_lote(map_chr(comb[q_comb], termo_ncbi))
  n_nome[is.na(n)] <- NA_integer_
  n_comb[is.na(n)] <- NA_integer_

  tibble(especie = e, termo = fila$termo, n_genbank = n, n_genbank_nome = n_nome,
         n_genbank_comb = n_comb, n_genbank_texto = n_texto, consultado_em = hoje())
}

termo_ncbi_vec <- function(nomes) paste0("(\"", nomes, "\"[Organism])")

# Para as órfãs: quantos registros o GÊNERO tem no GenBank (nota para quem for
# conferir; não muda a situação). Cache em data/genbank_generos.csv, validade de 90 dias.
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
                    n_genbank_genero = ncbi_conta_lote(paste0("\"", falta, "\"[Organism]")),
                    consultado_em = hoje()) |>
      filter(!is.na(n_genbank_genero))
    cache <- cache |> filter(!genero %in% novos$genero) |> bind_rows(novos) |> arrange(genero)
    gravar_csv(cache, destino)
  }
  cache
}
