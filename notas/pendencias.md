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

Corrigido nos testes: o SRA e as fotos do iNaturalist só rodam para espécies já consultadas no
GenBank; os eventos ("ganhou DNA") só contam quem já tinha sido consultado antes.

## F4: lembrar

- `data/cobertura.csv` tem ~12 MB com 35 mil espécies: pesado para o site no celular. Avaliar
  uma versão enxuta para o navegador (colunas do site, ou só as não "com DNA" com detalhe).
- `index.qmd` e `sobre.qmd` ainda têm silhuetas e textos do tetrapoDNA (classes, "uma consulta
  por classe" ao BOLD).
