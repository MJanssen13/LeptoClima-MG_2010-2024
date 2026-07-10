# LeptoClima-MG_2010-2024

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20617304.svg)](https://doi.org/10.5281/zenodo.20617304) [![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](LICENSE)

Associação entre o clima (precipitação, temperatura, umidade relativa, radiação
solar, vento e evapotranspiração), a densidade demográfica e a incidência de
**leptospirose** nas 12 mesorregiões de **Minas Gerais**, de **2010 a 2024**,
analisada por **semana epidemiológica (SE)** com defasagem de 2 SE (período de
incubação). Inclui **análise temporal** (correlação de Kendall e modelo Binomial
Negativa) e **análise espacial** (autocorrelação de Moran, LISA e mapas), com a
camada socioambiental (IVS e saneamento).

## Estrutura do repositório

```
Scripts/                          Código R organizado por análise (5 arquivos — ver Pipeline)
Bancos_rds/                       Bancos processados (.rds), prontos para análise
Resultados/Figuras/               Mapas e diagramas (.png) gerados pelas análises
Resultados/Tabelas/               Tabelas principais e suplementares (.docx)
Resultados/Material_suplementar/  Figuras suplementares (S1–S9) e documentos do material suplementar
Dados_brutos/                     Dados brutos (parcialmente versionados — ver "Fontes de dados")
```

## Pipeline (executar a partir da raiz do projeto, nesta ordem)

Os 24 scripts originais foram reunidos em **5 arquivos temáticos**, cada um com o
código de uma etapa da análise, delimitado internamente por blocos que indicam a
origem (`# [bloco: ...]`).

| # | Script | Conteúdo (etapas reunidas) | Principais saídas |
|---|--------|----------------------------|-------------------|
| 01 | `01_tratamento_dados.R` | Reconstrói os bancos a partir dos dados brutos: população e malha (IBGE), casos de leptospirose (SINAN/DATASUS), clima (BR-DWGD: precip./temp. e RH/Rs/u2/ETo), saneamento (Censo 2022) e IVS (Atlas IPEA). *(antigos 01–05, 15)* | bancos em `Bancos_rds/` |
| 02 | `02_analise_temporal.R` | Correlação de Kendall (SE, lag 2; soma móvel 4 SE), modelo Binomial Negativa (precip. + temp.) e figura da série temporal. *(antigos 06, 07, 12)* | `dados_temporais`, `fig_serie_temporal.png` |
| 03 | `03_analise_espacial.R` | Moran global (uni e bivariado), variáveis extras + densidade, LISA e mapas, mapas de notificados/LISA bivariado, diagramas de Moran e Figura 3. *(antigos 08, 16, 09, 10, 11, 19_figura3)* | `dados_espaciais_municipal`, mapas e diagramas |
| 04 | `04_caracterizacao_tabelas.R` | Caracterização descritiva dos casos, Tabelas 1 e 2 (CSV) e manuscrito em `.docx` (uso local; a saída `.docx` não é versionada). *(antigos 17, 13, 14)* | `tabela1.csv`, `tabela2.csv` |
| 05 | `05_sensibilidade_robustez.R` | Sensibilidade (regionalização e defasagem), análises complementares (BN sazonal e erro espacial multivariável), confundimento por confirmação, validação do alerta, IC robustos, FDR e figuras de robustez. *(antigos 18, 19_analises_complementares, 20–24)* | `sensibilidade`, `analises_complementares`, `confirmacao_covariavel`, `validacao_alerta`, `ic_robustos`, `fdr`, figuras de robustez |

O script **01** reconstrói os bancos a partir dos dados brutos. Como os bancos
processados (`Bancos_rds/`) estão versionados, é possível rodar as análises
(**02–05**) sem reprocessar tudo. As figuras (mapas e diagramas) ficam em
`Resultados/Figuras/`.

> **Nota:** ao reunir os scripts numa mesma sessão de R, o `MASS` (carregado no
> modelo temporal) mascara `dplyr::select`; por isso a chamada afetada em
> `02_analise_temporal.R` é qualificada como `dplyr::select`.

## Fontes de dados brutos (`Dados_brutos/`)

| Fonte | Conteúdo | Onde / como obter |
|-------|----------|-------------------|
| **SINAN / DATASUS** | Notificações de leptospirose (`LEPTBR*.dbc`, 2010–2024) | `Dados_brutos/Datasus/` — TabNet/FTP do DATASUS |
| **BR-DWGD** (Xavier et al.) | Grade diária NetCDF de `pr`, `Tmax`, `Tmin`, `RH`, `Rs`, `u2`, `ETo` | `Dados_brutos/dados_clima/raw/` — sites.google.com/site/alexandrecandidoxavierufes |
| **IBGE** | Estimativas e Censos (população, saneamento) | online via pacote `sidrar` |
| **Malha territorial** | Municípios e mesorregiões de MG | online via pacote `geobr` |
| **Atlas IPEA (IVS)** | Base Completa do IVS (`atlasivs_dadosbrutos_pt_v2 - MG.xlsx`) | `Dados_brutos/dados_pop/` — ivs.ipea.gov.br |
| **SES-MG (PDR 2026)** | Regionalização de saúde (município → macrorregião de saúde) — sensibilidade (script 05) | `Dados_brutos/dados_pop/Planilha-de-Regionalizacao_SES-MG_2026.xlsx` — saude.mg.gov.br |

> **Não versionados (excedem 100 MB/arquivo do GitHub):** as grades NetCDF do
> BR-DWGD (`Dados_brutos/dados_clima/raw/*.nc`, ~2,7 GB cada). O clima diário já
> extraído por município (`mg_clima_diario_2010_2024_FINAL.rds`, precip/temp, e
> `mg_clima_extra_diario.rds`, RH/Rs/u2/ETo) **está incluído**, então a pipeline
> roda sem as grades. A base bruta do IVS (Atlas IPEA) também não é versionada.

## Como rodar

```r
# a partir da raiz do projeto, nesta ordem:
Rscript Scripts/01_tratamento_dados.R        # tratamento dos dados (requer internet e Rtools)
Rscript Scripts/02_analise_temporal.R        # Kendall, Binomial Negativa e série temporal
Rscript Scripts/03_analise_espacial.R        # Moran, LISA, mapas e diagramas (Figura 3)
Rscript Scripts/04_caracterizacao_tabelas.R  # caracterização descritiva e Tabelas 1–2
Rscript Scripts/05_sensibilidade_robustez.R  # sensibilidade, complementares e robustez
```

## Software

R (≥ 4.6). Pacotes: `read.dbc`, `dplyr`, `tidyr`, `lubridate`, `sidrar`, `geobr`,
`sf`, `terra`, `exactextractr`, `readxl`, `MASS`, `spdep`, `spatialreg`,
`sandwich`, `lmtest`, `geepack`, `ggplot2`, `ggspatial`, `forcats`, `patchwork`,
`scales`, `officer`, `flextable`, `png`.
A leitura dos `.dbc` do DATASUS (`read.dbc`) requer **Rtools** (compilação).

## Licença

Os códigos e os bancos processados deste repositório são distribuídos sob a licença
**Creative Commons Atribuição 4.0 Internacional (CC BY 4.0)** — ver `LICENSE`. O uso,
a redistribuição e a adaptação são livres, inclusive para fins comerciais, desde que
citada a fonte. Os dados brutos originais permanecem sob os termos de suas fontes
públicas (DATASUS/SINAN, BR-DWGD, IBGE e IPEA).

## Como citar

Ao utilizar estes dados ou códigos, cite o repositório (metadados em `CITATION.cff`):

> Janssen MA, Resende ES, Fernandes AP. LeptoClima-MG: dados e códigos — Fatores
> climáticos e socioambientais associados à incidência de leptospirose em Minas Gerais,
> Brasil, 2010–2024. v1.1.0. Zenodo; 2026. https://doi.org/10.5281/zenodo.20617304

Quando publicado, cite também o artigo correspondente (submetido à *Cadernos de Saúde Pública*).
