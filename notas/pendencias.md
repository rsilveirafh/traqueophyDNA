# Pendências e registro de decisões

Começado em 2026-11-04. Riscar ou apagar o que for feito.

## F1: lista e ameaça (feito em 2026-11-04)

Fonte: Flora e Funga do Brasil, DwC-A do IPT do JBRJ, **v393.433** (publicada em 2026-11-01).
Arquivos: `taxon`, `distribution`, `speciesprofile`, `reference`, `resourcerelationship`
(este último não é usado: a sinonímia já vem em `acceptedNameUsageID`), `typesandspecimen`,
`vernacularname`.

| Etapa | Espécies |
|---|---|
| Aceitas, nível de espécie, vasculares | 42.659 |
| Nativas (`establishmentMeans` = Nativa) | 35.287 |
| Exóticas (cultivadas e naturalizadas, fora por D-P6) | 3.581 |
| Sem origem nem UF informadas (fora) | 3.791 |
| Híbridos (`×`), fora | 80 |
| **Na lista** (`data/especies.csv`) | **35.207** |

Por grupo: angiospermas 33.796 (19.715 endêmicas), samambaias e licófitas 1.386 (526),
gimnospermas 25 (5).

**Conferência com o número publicado.** O contador do portal
[Reflora](https://ferramentasreflora.jbrj.gov.br/) mostra Angiospermas 37.312, Samambaias e
Licófitas 1.432 e Gimnospermas 128. Esses números são as espécies aceitas da DwC-A **com origem
informada** (nativas + exóticas): 40.900 − 3.592 = 37.308 angiospermas; 1.598 − 166 = 1.432
samambaias e licófitas; 161 − 33 = 128 gimnospermas. A diferença de 4 angiospermas é de versão.
O portal não separa nativas.

Campos: UF falta em 584 espécies, domínio em 850, forma de vida em 532, endemismo em 413 e
**ano de descrição em 11.706** (nem `namePublishedInYear` nem `reference` trazem; completar
pelo WCVP na F2).

Nomes alternativos (`sinonimos_lista`): 67.573 nomes em 18.904 espécies. Entram os
infraespecíficos aceitos (D-P7), os sinônimos da espécie e os sinônimos dos infraespecíficos.
Ficam fora os nomes mal aplicados (são de outra espécie) e os binômios iguais ao de outra
espécie aceita (homônimos, sinônimos pro parte).

**Ameaça.** Portaria GM/MMA nº **1.783, de 26/10/2026** (DOU de 28/10/2026), CSV no
[Portal de Dados Abertos do MMA](https://dados.mma.gov.br/dataset/especies-ameacadas)
(não precisou extrair do PDF). 3.730 táxons; 3.706 ligados a espécies da lista (3.700
espécies). Dos 24 sem par, 21 são briófitas (fora do escopo); os vasculares são *Oocephalus
piranii* (sinônimo sem nome aceito na Flora e Funga), *Ouratea grandifolia* (nome de aplicação
incerta) e *Specklinia gomesferreirae* (ausente; há *Dryadella gomes-ferreirae* como sinônimo).
*Melanoxylon brauna* casou com *Melanoxylum brauna* pelo epíteto na família. A portaria não
traz DD/NT/LC.

## Decisões da F1 (confirmadas pelo Ricardo em 2026-11-05)

- [x] **Só nativas** (D-P6): cultivadas e naturalizadas ficam fora.
- [x] **Híbridos fora** (80 nototáxons, como *Dyckia ×elisabethae*). Dá para incluir: o GenBank
      grafa "Dyckia x elisabethae".
- [x] **Sem origem informada fora** (3.791 aceitas sem nenhuma linha em `distribution`). Na
      amostra há exóticas (*Eucalyptus*, *Gasteria*) e nativas mal preenchidas
      (*Pleurothallis*, *Rudgea*). O Reflora também não as conta.
- [ ] O DwC-A (20 MB) fica fora do git e é baixado a cada execução; o histórico da lista fica
      no diff de `data/especies.csv`.

## F2: DNA (em andamento, 2026-11-04)

- [x] **WCVP** (`R/wcvp.R`): baixado direto do SFTP da Kew (`wcvp.zip`, `wcvp_names.csv`), sem
      os pacotes `rWCVP`/`rWCVPdata`. 31.831 espécies aceitas no WCVP, 2.658 sinônimos (o nome
      aceito lá também é buscado), 718 ausentes. Ano de descrição completado para 34.489.
      Refeito a cada 90 dias (`data/wcvp.csv`).
- [x] **Pré-filtro pela taxonomia do NCBI** (`R/taxdump.R`): subárvore de Viridiplantae
      (442 mil táxons, 583 mil nomes). **32.405 de 35.207** espécies têm algum nome no NCBI;
      só esses nomes entram no `[Organism]`. As outras 2.802 ganham 0 sem consulta e passam só
      pela busca em texto livre. Refeito a cada 7 dias ou quando muda o conjunto de nomes.
- [x] **BOLD**: o portal v5 responde para o filo inteiro (`tax:phylum:Tracheophyta`) em ~5 s:
      115.356 nomes, 367.562 registros (só 5.188 coletados no Brasil). As classes do BOLD não
      seguem as da Flora e Funga (Gnetales em Pinopsida): casamento só pelo nome.
- [x] `mesmo_epiteto()` trata o epíteto infraespecífico (variedade elevada a espécie).
- [x] `id_ffb` e `wcvp_situacao` no lugar de `id_ctfb`/`ctfb_situacao`/`gbif_situacao`.
- [x] **Ritmo do NCBI.** Em série, cada chamada leva ~0,4 s (~2,5 req/s, abaixo do limite de
      10 req/s da chave). Com requisições paralelas (`httr2::req_perform_parallel`, 8 conexões,
      `req_throttle` a 9 req/s), o NCBI entrega ~4 req/s: o teto é do servidor (16 conexões ou
      sem limitador não melhoram). GenBank: ~2,5 espécies/s (~2,2 chamadas por espécie).
- [x] **1ª coleta completa** (local, 2026-11-04 21:11 a 2026-11-05 13:31, com o PC suspenso
      ~11 h no meio e uma queda de energia). Tempo efetivo: **GenBank ~3h50** para 32.507
      espécies; SRA ~55 min para 12.599; gêneros ~4 min. Cabe nas 5 h da Action, mas as
      semanais são menores: órfãs a cada 6+ dias (~16 mil, ~1h45) e com DNA a cada 28 dias
      (~4,5 mil por semana).

**Resultado da 1ª coleta** (35.207 espécies):

| Situação | Espécies | % |
|---|---|---|
| com DNA | 17.930 | 51 |
| sem DNA | 15.928 | 45 |
| a verificar | 1.349 | 4 |

- Por grupo: angiospermas 16.981 / 15.535 / 1.280; samambaias e licófitas 931 / 386 / 69;
  gimnospermas 18 / 7 / 0 (com / sem / a verificar).
- Sem DNA: endêmicas 54%, não endêmicas 32%; ameaçadas (Portaria 1.783) 52%, demais 45%.
- Famílias grandes: Fabaceae 27% sem DNA, Poaceae 36%, Melastomataceae 38%, Bromeliaceae 43%,
  Orchidaceae 44%, Asteraceae 53%, Euphorbiaceae 53%, Myrtaceae 54%, Malvaceae 57%,
  Rubiaceae 59%.
- 10.138 espécies com registro no BOLD; 1.286 só com dados brutos no SRA; 181 sem DNA cujo
  nome aparece em texto livre no GenBank (candidatas a falsa órfã).
- **A taxonomia do NCBI tem muitos nomes sem sequência**: 42% das espécies com nome no NCBI
  estão sem DNA. Conferido à mão (*Neocouma parviflora*, *Spigelia spruceana*, *Bulbostylis
  distichoides*, *Mollinedia argyrogyna*: tax_id existe, 0 no nuccore e 0 em proteína). O
  pré-filtro descarta menos que o esperado (só 2.802 espécies), mas não erra.
- "A verificar": a maioria é "há sequências só sob nome alternativo" e "nome ausente do WCVP e
  do NCBI".

Falhas da 1ª coleta (voltam sozinhas na próxima execução): 1.192 consultas de gênero (queda
de energia; só afetam a nota "o gênero tem N registros") e 13 do GenBank.

- [x] **Fotos**: a busca por nome (1 espécie por chamada, ~1/s) levaria ~20 semanas para as
      16 mil da fila. Agora o nome vira id pela taxonomia que o iNaturalist publica todo mês
      (DwC-A, ~80 MB, `data/cache/inat_ids.csv`) e a foto vem de `/v1/taxa/<30 ids>`: ~540
      chamadas, ~10 min. Conferido contra a busca por nome em 400 espécies: mesmos 273 ids e
      as mesmas 98 fotos. ~60% das espécies sem DNA estão no iNat; ~1/3 delas com foto CC.

## F3: validação

- Parceira botânica: **Maria Teresa Buril** (validação da amostra de 120 espécies e eventual
  artigo). O Ricardo vai conversar com ela.
- [x] Amostra (120), checagem assistida e julgamento em 2026-11-05: ver `validacao/PROTOCOLO.md`.
      0 falsa órfã em 50 "sem DNA", 0 falso positivo em 20 "com DNA"; das 50 "a verificar", 40
      são sem DNA, 8 com DNA e 2 seguem a verificar.
- [ ] Auditoria humana: `validacao/auditoria.xlsx` (20 espécies, 14 delas "a verificar"), Ricardo
      e Maria Teresa, cada um sem ver a resposta do outro.

Correções no pipeline que a validação sugeriu (feitas em 2026-11-05; teste com 6 espécies da
amostra: as 6 mudaram como o julgamento; 3.514 espécies voltam para a fila do GenBank):

- [x] **WCVP junta com outra espécie aceita na Flora e Funga** (8 de 50 na amostra). Quando o
      nome aceito no WCVP é outra espécie da lista, não somar os sinônimos do WCVP que a Flora e
      Funga dá a essa outra espécie (nem o basiônimo dela). Regra: um nome que a Flora e Funga
      atribui a outra espécie (nome aceito ou sinônimo) sai dos nomes alternativos.
- [x] **Homônimos** (6 de 50). Um sinônimo cujo binômio é nome aceito de outra planta no WCVP
      (*Terminalia sericea* Cambess. × Burch. ex DC.) sai da busca. E, na taxonomia do NCBI, um
      nome que lá é sinônimo de uma espécie aceita na Flora e Funga que não é a nossa também sai.
- [x] **`mesmo_epiteto()`** compara a raiz cortando 2 letras: *ctenophylla* e *ctenophyllum* não
      casam. Cortar a terminação (-a, -us, -um, -is, -e) e aceitar 1 letra de diferença na raiz
      (pega também *occidentallis*/*occidentalis*).
- [x] **Táxon novo no NCBI** depois do taxdump: quando o texto livre acha registros e o organismo
      deles é o nome da lista, contar como com DNA (*Megalastrum eugenii*). O taxdump semanal
      resolve na semana seguinte, mas vale a regra.
- [x] **Sinônimo da Flora e Funga com DNA conta como "com DNA"** (`n_genbank_ffb`, `n_bold_ffb`);
      só nomes que vêm apenas do WCVP deixam "a verificar". No WCVP, 565 espécies juntadas a outra
      da lista e 4.349 com algum nome ambíguo; nomes buscados: de 133 mil para 119 mil.
- [x] **Nome só como hospedeiro** (fungo, bactéria, vírus): o alerta de texto livre não deveria
      virar "a verificar" sozinho; pode ser só nota, como o do gênero.

- [x] **Nome ausente do WCVP** sem nenhuma sequência deixa de ser "a verificar" e vira nota (17 de
      17 na amostra eram sem DNA; eram 474 das 549 que sobraram).

**2ª coleta, com as regras novas** (2026-11-05, 18:13 a 19:07, local): GenBank 3.515 espécies em
37 min, SRA 787 em 5 min. Resultado: **18.303 com DNA (52%), 16.829 sem DNA (48%), 75 a
verificar**. Por grupo (com / sem / a verificar): angiospermas 17.344 / 16.384 / 68; samambaias e
licófitas 941 / 438 / 7; gimnospermas 18 / 7 / 0. Os 390 "eventos" da coleta eram efeito da mudança
de regra, não DNA novo: `eventos.csv` voltou a ter só o cabeçalho. Detalhes em
`validacao/PROTOCOLO.md` (seção 3, "Depois das correções").

- [ ] **BOLD bloqueado (403 do Cloudflare)** para este IP desde a tarde de 2026-11-05: o portal inteiro,
      não só a API. Provavelmente por causa das centenas de buscas da checagem assistida. A coleta usou
      o resumo salvo às 15:21 do mesmo dia. Conferir se passa; na Action o IP é outro.
- [ ] **Sinônimo da Flora e Funga que no NCBI é outra planta fora da lista** (*Loranthus dichotomus*
      → *Aetanthus dichotomus*, 1 caso na amostra). `limpa_nomes_ncbi()` só tira o nome quando a
      outra planta está na lista. Avaliar se vale tirar também quando o nome científico do NCBI não
      é nenhum dos nomes da espécie.

Corrigido nos testes: o SRA e as fotos do iNaturalist só rodam para espécies já consultadas no
GenBank; os eventos ("ganhou DNA") só contam quem já tinha sido consultado antes.

## F4: site (2026-11-05)

- [x] **Dados enxutos para o site:** `data/site.csv` (`grava_site()` em `R/junta.R`, gravado a cada
      atualização): 19 colunas, sem a frase padrão das "sem DNA". 7,3 MB (1 MB com gzip, que o
      GitHub Pages aplica) contra 13 MB do `cobertura.csv`, que segue para download.
- [x] Textos de todas as páginas para plantas (Flora e Funga, WCVP, taxonomia do NCBI, Portaria
      1.783/2026, validação de plantas); silhuetas do PhyloPic (*Xylopia*, *Araucaria*,
      *Polypodium*; créditos em `assets/silhuetas/CREDITOS.md`).
- [x] Capa: mosaico em canvas (35 mil quadradinhos em SVG pesariam), três cartões de grupo,
      domínios fitogeográficos e a fração da família na lacuna.
- [x] **Por domínio** (`dominios.qmd`): seis cartões clicáveis, exclusivas do domínio, lista por
      prioridade e a lacuna por forma de vida e substrato.
- [x] Espécies sem DNA: filtros de domínio e forma de vida, link para a ficha na Flora e Funga.
      Ameaça por prefixo ("CR (infra)", "CR (PE)" contam como ameaçadas).
- [x] Conferido no Edge headless (desktop e iframes de 375 px).
- [ ] O que o roteiro pede e ficou para depois: link que guarda os filtros (pendência do
      tetrapoDNA também).
