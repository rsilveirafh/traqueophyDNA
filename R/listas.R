# Lista de espécies: Flora e Funga do Brasil (JBRJ) -------------------------------
#
# Uma lista só, no Darwin Core Archive publicado no IPT do JBRJ:
#   https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil
# (doi:10.15468/1mtkaw; CC-BY). Arquivos usados: taxon (nomes, status,
# classificação, sinônimos), distribution (UF, origem, endemismo, domínio
# fitogeográfico), speciesprofile (forma de vida, substrato) e reference (ano da
# descrição original quando o taxon não traz).
#
# Recorte (decisões D-P1, D-P6 e D-P7 do roteiro):
# - plantas vasculares: angiospermas, gimnospermas, samambaias e licófitas;
# - nome aceito, no nível de espécie;
# - nativa (establishmentMeans "Nativa"); exóticas (cultivadas e naturalizadas)
#   e as sem origem informada ficam fora;
# - variedades, subespécies e formas somam na espécie: os nomes infraespecíficos
#   (aceitos e sinônimos) entram em `sinonimos_lista` e, daí, na busca de DNA.
#
# A função devolve o esquema comum (`ESQUEMA`); o resto do pipeline lê só
# `data/especies.csv`.

ESQUEMA <- c("grupo", "classe", "ordem", "familia", "genero", "especie",
             "autoria", "ano_descricao", "endemica_br", "categoria_ameaca",
             "fonte_ameaca", "ufs", "uf_fonte", "biomas", "forma_vida", "substrato",
             "nota_lista", "sinonimos_lista", "id_ffb", "fonte_lista", "versao_lista")

# grupo = 2º nível de higherClassification na Flora e Funga.
GRUPOS <- tibble::tribble(
  ~grupo,         ~rotulo,                  ~ffb,
  "angiospermas", "Angiospermas",           "Angiospermas",
  "gimnospermas", "Gimnospermas",           "Gimnospermas",
  "samambaias",   "Samambaias e licófitas", "Samambaias e Licófitas"
)

FONTE_LISTA <- list(
  citacao = "Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Lista de espécies (Darwin Core Archive, IPT do JBRJ).",
  licenca = "CC-BY 4.0",
  url = "https://doi.org/10.15468/1mtkaw"
)

URL_FFB <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
ARQ_FFB <- "data-raw/flora_fungabr_dwca.zip"   # fora do git (20 MB); baixado a cada execução

RANK_INFRA <- c(VARIEDADE = "var.", SUB_ESPECIE = "subsp.", FORMA = "f.")

# Nome canônico (sem autoria): "Gênero epíteto [var.|subsp.|f. infra]".
nome_canonico <- function(genero, epiteto, rank, infra) {
  base <- binomio(genero, epiteto)
  abrev <- unname(RANK_INFRA[rank])
  ifelse(!is.na(infra) & !is.na(abrev), paste(base, abrev, str_trim(infra)), base)
}

ler_dwca <- function(zip, arquivo) {
  readr::read_tsv(unz(zip, arquivo), col_types = cols(.default = col_character()),
                  quote = "", na = c("", "NA"), progress = FALSE)
}

# Versão do recurso no IPT (packageId ".../v393.433") e data de publicação.
versao_ffb <- function(zip) {
  eml <- paste(readLines(unz(zip, "eml.xml"), warn = FALSE, encoding = "UTF-8"), collapse = " ")
  v <- str_match(eml, "packageId=\"[^\"]*/v([0-9.]+)\"")[, 2]
  paste0("Flora e Funga do Brasil v", v)
}

# Valores de um campo-lista no JSON de occurrenceRemarks/lifeForm
# ('{"lifeForm":["Árvore","Arbusto"],...}') -> "Árvore;Arbusto".
campo_json <- function(x, chave) {
  m <- str_match(x, paste0('"', chave, '":\\[([^\\]]*)\\]'))[, 2]
  m <- str_remove_all(m, '"')
  ifelse(is.na(m) | m == "", NA_character_, str_replace_all(m, ",", ";"))
}

junta_txt <- function(x) {
  x <- unique(unlist(strsplit(na.omit(x), ";")))
  if (length(x)) paste(sort(x), collapse = ";") else NA_character_
}

lista_flora <- function(baixar = TRUE) {
  if (baixar) baixar_fonte(URL_FFB, ARQ_FFB)
  if (!file.exists(ARQ_FFB)) stop("Flora e Funga indisponível e sem cópia local: ", ARQ_FFB)
  versao <- versao_ffb(ARQ_FFB)
  msg("  ", versao)

  tx <- ler_dwca(ARQ_FFB, "taxon.txt") |>
    mutate(grupo_ffb = word(higherClassification, 2, sep = fixed(";")),
           nome = nome_canonico(genus, specificEpithet, taxonRank, infraspecificEpithet))
  di <- ler_dwca(ARQ_FFB, "distribution.txt")
  pf <- ler_dwca(ARQ_FFB, "speciesprofile.txt")
  rf <- ler_dwca(ARQ_FFB, "reference.txt")

  vasc <- filter(tx, grupo_ffb %in% GRUPOS$ffb)
  aceitas <- filter(vasc, taxonomicStatus == "NOME_ACEITO", taxonRank == "ESPECIE")

  # Origem, UF, endemismo e domínio (uma linha por UF; o JSON se repete).
  dist <- di |>
    filter(id %in% aceitas$id) |>
    group_by(id) |>
    summarise(
      nativa = any(establishmentMeans == "Nativa", na.rm = TRUE),
      ufs = junta_ufs(str_remove(locationID, "^BR-")),
      biomas = junta_txt(campo_json(occurrenceRemarks, "phytogeographicDomain")),
      endemica_br = case_when(
        any(str_detect(occurrenceRemarks, '"endemism":"Endemica"'), na.rm = TRUE) ~ TRUE,
        any(str_detect(occurrenceRemarks, '"endemism":"N[^"]*o endemica"'), na.rm = TRUE) ~ FALSE
      ),
      .groups = "drop"
    )
  perfil <- pf |>
    filter(id %in% aceitas$id) |>
    transmute(id, forma_vida = campo_json(lifeForm, "lifeForm"),
              substrato = campo_json(lifeForm, "habitat"))
  ano_ref <- rf |>
    filter(id %in% aceitas$id, !is.na(date)) |>
    group_by(id) |>
    summarise(ano_ref = extrai_ano(first(date)), .groups = "drop")

  base <- aceitas |>
    left_join(dist, by = "id") |>
    filter(coalesce(nativa, FALSE))

  # Nomes alternativos de cada espécie (D-P7 e sinonímia da própria lista):
  # infraespecíficos aceitos, sinônimos da espécie e sinônimos dos
  # infraespecíficos. Fora: nomes mal aplicados (são de outra espécie) e nomes
  # iguais ao de outra espécie aceita (homônimos, sinônimos pro parte).
  infra <- filter(vasc, taxonomicStatus == "NOME_ACEITO", taxonRank %in% names(RANK_INFRA),
                  parentNameUsageID %in% base$id) |>
    select(id_infra = id, id_sp = parentNameUsageID, nome)
  alvo <- bind_rows(select(base, id_alvo = id) |> mutate(id_sp = id_alvo),
                    transmute(infra, id_alvo = id_infra, id_sp))
  sinon <- vasc |>
    filter(taxonomicStatus == "SINONIMO",
           is.na(nomenclaturalStatus) | nomenclaturalStatus != "NOME_MAL_APLICADO") |>
    inner_join(alvo, by = c("acceptedNameUsageID" = "id_alvo")) |>
    select(id_sp, nome)
  # (as espécies aceitas de qualquer reino: um sinônimo pode ser homônimo de alga)
  aceitas_sp <- unique(tx$nome[tx$taxonomicStatus %in% "NOME_ACEITO" & tx$taxonRank %in% "ESPECIE"])
  nomes_sp <- base |> transmute(id_sp = id, especie = nome)
  alternativos <- bind_rows(select(infra, id_sp, nome), sinon) |>
    inner_join(nomes_sp, by = "id_sp") |>
    filter(nome != especie, !nome %in% aceitas_sp) |>
    group_by(id_sp) |>
    summarise(sinonimos_lista = paste(sort(unique(nome)), collapse = ";"), .groups = "drop")

  out <- base |>
    left_join(perfil, by = "id") |>
    left_join(ano_ref, by = "id") |>
    left_join(alternativos, by = c("id" = "id_sp")) |>
    left_join(select(GRUPOS, grupo, grupo_ffb = ffb), by = "grupo_ffb") |>
    transmute(
      grupo, classe = class, ordem = order, familia = family, genero = genus,
      especie = nome, autoria = scientificNameAuthorship,
      ano_descricao = coalesce(extrai_ano(namePublishedInYear), ano_ref),
      endemica_br, ufs, uf_fonte = ifelse(is.na(ufs), NA, "Flora e Funga"),
      biomas, forma_vida, substrato,
      nota_lista = if_else(nomenclaturalStatus %in% "NOME_CORRETO_VIA_CONSERVACAO",
                           "nome conservado", NA_character_),
      sinonimos_lista, id_ffb = taxonID,
      fonte_lista = FONTE_LISTA$url, versao_lista = versao
    )
  padroniza(out)
}

padroniza <- function(x) {
  faltam <- setdiff(ESQUEMA, names(x))
  for (f in faltam) x[[f]] <- NA
  x$endemica_br <- as.logical(x$endemica_br)
  x$ano_descricao <- as.integer(x$ano_descricao)
  fora <- x |> filter(is.na(especie) | !str_detect(especie, "^[A-Z][a-z-]+ [a-z-]+$"))
  if (nrow(fora)) msg("  ", nrow(fora), " nome(s) fora do padrão binomial descartado(s): ",
                      paste(head(fora$especie, 5), collapse = ", "))
  x[ESQUEMA] |>
    mutate(across(where(is.character), str_squish)) |>
    filter(!is.na(especie), str_detect(especie, "^[A-Z][a-z-]+ [a-z-]+$")) |>
    distinct(especie, .keep_all = TRUE) |>
    arrange(grupo, familia, especie)
}

monta_listas <- function(baixar = TRUE) {
  msg("Lista: Flora e Funga do Brasil")
  lista_flora(baixar = baixar)
}
