# traqueophyDNA: a lacuna de DNA das plantas vasculares do Brasil

> **Em construção.** Esqueleto copiado do [tetrapoDNA](https://github.com/rsilveirafh/tetrapoDNA)
> (mesma pergunta, mesmo método, mesmo visual).

Painel que responde a uma pergunta simples: **quais espécies de plantas vasculares nativas do Brasil
ainda não têm nenhuma sequência de DNA pública?** Para cada espécie da
[Flora e Funga do Brasil](https://floradobrasil.jbrj.gov.br) (JBRJ), o painel conta os registros no
[BOLD Systems](https://boldsystems.org) e no [GenBank](https://www.ncbi.nlm.nih.gov/genbank/), mostra
a lacuna por grupo, família, estado e domínio fitogeográfico, e convida pesquisadores a **adotar uma
espécie**: dizer em público que vão sequenciá-la.

O recorte é de plantas vasculares (angiospermas, gimnospermas, samambaias e licófitas) **nativas**;
briófitas, algas, fungos e espécies cultivadas ou naturalizadas não entram. Variedades e subespécies
somam na espécie.

Atualização automática semanal (GitHub Actions); site estático no GitHub Pages.

## Situação de cada espécie

| Situação | Critério |
|---|---|
| com DNA | ao menos um registro no BOLD ou no GenBank sob o nome da lista ou sob outra combinação do mesmo epíteto |
| a verificar | sem DNA sob o nome da lista, mas há motivo para desconfiar do nome (sequências só sob sinônimo, nome ausente da taxonomia do NCBI ou do WCVP, nome que aparece no GenBank fora do campo Organism...) |
| sem DNA | nenhuma sequência depositada sob o nome e os sinônimos conhecidos, e nenhum sinal acima |

O painel conta sequências de DNA **depositadas** (qualquer marcador) no BOLD ou no GenBank; não é um
levantamento de genomas nem de bases de referência para eDNA. A marca **só dados brutos** vai nas
espécies sem sequência montada que têm leituras no SRA. O painel nunca afirma "não existe DNA" (o
risco da **falsa órfã**: o banco usa um nome antigo); a coluna `observacao` diz o motivo de cada
"a verificar".

## Fontes (previstas)

- **Flora e Funga do Brasil** (JBRJ), Darwin Core Archive no IPT
  (`ipt.jbrj.gov.br/jbrj`, recurso `lista_especies_flora_brasil`;
  [doi:10.15468/1mtkaw](https://doi.org/10.15468/1mtkaw)), CC-BY: lista, sinônimos, UF, domínio
  fitogeográfico, endemismo, origem e forma de vida.
- **WCVP** (Royal Botanic Gardens, Kew), pacotes `rWCVP`/`rWCVPdata`: sinônimos globais.
- **Taxonomia do NCBI** (`taxdump`, `names.dmp`): pré-filtro antes de consultar o GenBank.
- **BOLD Systems** (portal v5) e **NCBI E-utilities** (`nuccore` e `sra`).
- **Lista Nacional Oficial das Espécies da Flora Ameaçadas de Extinção** (MMA, 2026).
- **IBGE**, malha de UFs, para o mapa.

## Como rodar

Requisitos: R ≥ 4.3 e Quarto ≥ 1.5.

```sh
Rscript scripts/atualiza.R   # coleta (ainda a adaptar para plantas)
quarto preview               # site
```

Variáveis de ambiente opcionais: `NCBI_API_KEY` e `NCBI_EMAIL` (em `.Renviron`, fora do git).

## Licença

Código: MIT. Tabelas derivadas em `data/`: CC-BY 4.0; cite também a Flora e Funga do Brasil.

---

**In English.** This dashboard tracks the public DNA sequence gap for the native vascular plants of
Brazil. For every species in the Flora e Funga do Brasil checklist it counts public records in BOLD
Systems and NCBI GenBank, checking synonyms to avoid "false orphans", and highlights species with no
public DNA at all by group, family, state and phytogeographic domain. Researchers can "adopt" a
species through a GitHub issue form. Work in progress.
