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

## Decisões tomadas sem o Ricardo (rever)

- [ ] **Híbridos fora** (80 nototáxons, como *Dyckia ×elisabethae*). Dá para incluir: o GenBank
      grafa "Dyckia x elisabethae".
- [ ] **Sem origem informada fora** (3.791 aceitas sem nenhuma linha em `distribution`). Na
      amostra há exóticas (*Eucalyptus*, *Gasteria*) e nativas mal preenchidas
      (*Pleurothallis*, *Rudgea*). O Reflora também não as conta.
- [ ] O DwC-A (20 MB) fica fora do git e é baixado a cada execução; o histórico da lista fica
      no diff de `data/especies.csv`.

## F2: DNA (a fazer)

- [ ] Pré-filtro pelo `taxdump` do NCBI (`names.dmp`).
- [ ] Sinônimos globais do WCVP (`rWCVP`, `rWCVPdata`, não instalados) e ano de descrição.
- [ ] BOLD: como o portal v5 organiza Tracheophyta; resumo por classe ou por família.
- [ ] `mesmo_epiteto()`: em plantas, variedade elevada a espécie muda o epíteto da espécie
      (*Aus bus* var. *cus* → *Xus cus*); tratar o epíteto infraespecífico como o da espécie.
- [ ] Trocar `ctfb_situacao`/`id_ctfb` por `id_ffb` em `R/junta.R` e no site.
- [ ] Cronometrar a 1ª coleta (limite de 6 h por job na Action).
