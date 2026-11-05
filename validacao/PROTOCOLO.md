# Validação da classificação

Mesmo método do tetrapoDNA. Objetivo: medir quantas vezes o painel erra, nos dois sentidos.

- **Falsa órfã:** o painel diz "sem DNA" (ou "a verificar"), mas há sequência de DNA depositada da
  espécie.
- **Falso positivo:** o painel diz "com DNA", mas as sequências não são da espécie.

## 1. Amostra

`scripts/amostra_validacao.R` sorteia de `data/cobertura.csv` (dados de 2026-11-05), com semente
20261105. Dentro de cada situação, o número por grupo é proporcional ao tamanho do grupo
(gimnospermas, com 25 espécies, não entram).

| Situação no painel | No painel | Amostra | Para quê |
|---|---|---|---|
| sem DNA | 15.928 | 50 (49 angiospermas, 1 samambaia) | taxa de falsa órfã |
| a verificar | 1.349 | 50 (47 angiospermas, 3 samambaias) | o que as "a verificar" são de fato |
| com DNA | 17.930 | 20 (19 angiospermas, 1 samambaia) | controle de falso positivo |

`amostra.csv` guarda o sorteio, com os nomes buscados (Flora e Funga e WCVP) e os links de busca
de cada espécie (BOLD, GenBank, taxonomia do NCBI, Flora e Funga, POWO).

## 2. Checagem assistida

`scripts/confere_amostra.R` junta evidências por caminhos diferentes dos do pipeline, que conta o
BOLD pelo nome exato no resumo do filo e o GenBank pelo campo Organism com os nomes da Flora e
Funga e do WCVP que existem na taxonomia do NCBI:

- **Taxonomia do NCBI:** nomes que o NCBI conhece para a espécie (sinônimos, grafias) e o taxid.
- **nuccore pelo taxid** (`txid[Organism:exp]`), **SRA** e **BioSample**.
- **nuccore em todos os campos:** o nome, os sinônimos e os nomes do NCBI como frase; e o epíteto em
  registros do gênero ou da família ("sp.", "cf.", gêneros antigos).
- **BOLD:** cada nome exato e, no gênero de cada nome, os nomes com a mesma raiz do epíteto.
- **Para as "com DNA":** título e organismo dos primeiros registros do GenBank.

A saída fica em `checagem.csv`. Cada caso com algum sinal é aberto e julgado (título dos
registros, tipo de dado no SRA, gênero antigo no GenBank); o julgamento de cada espécie, com o nome
no banco, um número de acesso e o motivo, fica em `julgamento.csv`. A checagem e o julgamento são
feitos por um assistente de IA (Claude) e passam por auditoria humana (seção 4).

**Critérios** (os mesmos do tetrapoDNA). Conta como DNA montado: sequência no GenBank ou no BOLD
identificada até a espécie, sob o nome da lista, um sinônimo, um gênero antigo ou um nome
infraespecífico da espécie (variedades e subespécies somam na espécie). Não contam: registros só até
gênero ("sp.", "cf.", "aff.") e sequências de outra espécie que só compartilham o epíteto. "Só SRA"
é a espécie que tem apenas dados brutos de sequenciamento no SRA, sem nada montado no GenBank ou no
BOLD; continua "sem DNA" no painel, com a marca "só dados brutos". "A verificar" fica para os casos
em que as sequências estão sob um nome de circunscrição mais ampla (espécie desmembrada, sinônimo
pro parte) e não dá para saber, sem olhar o voucher, se alguma é da espécie da lista.

## 3. Resultados

`scripts/resume_validacao.R` calcula as proporções, com intervalo de confiança de Wilson (95%), e a
estimativa para o painel inteiro (`resultado.csv`).

Checagem e julgamento em 2026-11-05 (dados de 2026-11-05).

| No painel | Conferido | Amostra | % (IC 95%) | Estimativa no painel |
|---|---|---|---|---|
| sem DNA (15.928) | sem DNA | 50/50 | 100% (93–100%) | |
| a verificar (1.349) | sem DNA | 40/50 | 80% (67–89%) | ~1.079 (903–1.197) |
| | com DNA | 8/50 | 16% (8–29%) | ~216 (112–385) |
| | a verificar | 2/50 | 4% (1–13%) | ~54 (15–182) |
| com DNA (17.930) | com DNA | 20/20 | 100% (84–100%) | |

Das 50 "sem DNA", 4 têm só dados brutos no SRA (já marcadas pelo painel); nas outras 46, os sinais
eram o epíteto em outra espécie. Entre as "com DNA", *Callianthe scabrida* está sob o basiônimo
*Bakeridesia scabrida*.

O que isso diz:

1. **Não houve falsa órfã** entre as 50 "sem DNA" **nem falso positivo** entre as 20 "com DNA".
2. **A maioria das "a verificar" é sem DNA** (80%). Os motivos, na amostra:
   - **Sequências de outra espécie aceita na Flora e Funga** (8): o WCVP junta a espécie da lista
     com outra que a Flora e Funga aceita (*Chamaecrista amphibola* com *C. exsudans*, *Croton
     allemii* com *C. triqueter*, *Senegalia tubulifera* com *S. riparia*, *Myrcia rufopilosa* com
     *M. neoobscura*…), e os sinônimos dessa outra espécie entram na busca. **Erro do pipeline.**
   - **Homônimo** (6): o sinônimo é, no NCBI, outra planta com o mesmo binômio (*Terminalia
     sericea* e *Eriocaulon latifolium* africanas, *Verbesina helianthoides* norte-americana,
     *Eupatorium paniculatum* = *Brickellia paniculata*, *Salvinia natans* eurasiática).
   - **Conceito amplo, material de fora** (5): o WCVP inclui a espécie numa mais ampla e as
     sequências são de outro lugar (*Anthurium cordatum* das Ilhas Virgens, *Syngonium
     podophyllum* cultivado).
   - **Nome só como hospedeiro** (5): o nome aparece em fungos endofíticos, bactérias de nódulo e
     vírus.
   - **Nada em nenhuma base** (17): nome ausente do WCVP e da taxonomia do NCBI.
3. **As 8 "a verificar" com DNA** estão sob um sinônimo heterotípico da Flora e Funga (3), um nome
   novo ou transferência (*Calyptranthes lucida* → *Myrcia neolucida*; *Miconia pleiosetulosa* ←
   *Pleiochiton setulosum*), outra grafia (*Hebanthe occidentallis* na Flora e Funga), o epíteto
   noutro gênero com outra terminação (*Encholirium ctenophyllum*: o pipeline não reconheceu
   *ctenophylla*/*ctenophyllum*) ou um táxon novo no NCBI (*Megalastrum eugenii*, 21 sequências; o
   alerta de texto livre pegou).
4. **Restam 2 a verificar** de fato (*Hydrocotyle asterias*, *Varronia polystachya*): o WCVP junta
   a espécie num complexo e falta um especialista para dizer de quem são as sequências.

Correções no pipeline sugeridas pela validação: ver `notas/pendencias.md` (F3).

### Depois das correções (coleta de 2026-11-05, 18h)

As regras novas (seção F3 de `notas/pendencias.md`) reconsultaram 3.515 espécies no GenBank. Também
deixou de ser motivo de "a verificar" o "nome não encontrado no WCVP" sem nenhuma sequência: as 17
assim, na amostra, eram todas sem DNA.

| Situação | Antes | Depois |
|---|---|---|
| com DNA | 17.930 | 18.303 |
| sem DNA | 15.928 | 16.829 |
| a verificar | 1.349 | 75 |

- Das 1.349 "a verificar": 389 viraram com DNA (a estimativa era ~216, IC 112–385), 885 sem DNA e
  75 seguem a verificar, todas com sequências só sob um sinônimo do WCVP.
- 17 "com DNA" viraram sem DNA. As sequências eram de outra planta com o mesmo binômio (*Vitis
  palmata*, norte-americana, para *Cissus palmata*; *Derris scandens*, asiática; *Dioscorea
  spicata*) ou de outra espécie da lista (*Sophronitis brevicaulis* é *Cattleya harpophylla* no
  NCBI; *Matelea linearis* é *Blepharodon lineare*). Eram falsos positivos (~0,1% das "com DNA").
- 1 "sem DNA" virou com DNA (*Vanilla verrucosa*, sob o sinônimo *Vanilla angustipetala*).
- Na amostra, o painel agora concorda com o julgamento em **115 de 120**:
  - *Hibiscus multiformis* e *Syngonium vellozianum* seguem "a verificar". O julgamento olhou a
    origem das sequências (material de fora), o que o pipeline não faz.
  - *Hydrocotyle asterias* e *Varronia polystachya* ficaram "sem DNA". O julgamento dizia "a
    verificar", mas o WCVP junta as duas com outra espécie e os nomes do WCVP saíram da busca.
  - *Psittacanthus clusiifolius* ficou "com DNA" por um sinônimo da Flora e Funga (*Loranthus
    dichotomus*) que, no NCBI, é outra planta (*Aetanthus dichotomus*, andina).

## 4. Auditoria humana

`auditoria.xlsx` traz 20 espécies sorteadas por estrato, com mais peso nos casos que mudam de
situação. Para cada uma estão o julgamento da checagem assistida e os links de busca.

Quem audita preenche `concorda` (sim, não ou em parte), `situacao_auditor` e um comentário. Duas
pessoas auditam sem ver a resposta uma da outra, para medir a concordância: **Ricardo Silveira** e
**Maria Teresa Buril** (botânica). Leva cerca de 5 minutos por espécie.
