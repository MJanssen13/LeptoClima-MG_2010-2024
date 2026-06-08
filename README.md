# LeptoClima-MG_2010-2024

Associação entre o clima (precipitação e temperatura) e a incidência de
**leptospirose** nas 12 mesorregiões de **Minas Gerais**, de **2010 a 2024**,
analisada por **semana epidemiológica (SE)** com defasagem de 2 SE (período de
incubação). Inclui **análise temporal** (correlação de Kendall e modelo Binomial
Negativa) e **análise espacial** (autocorrelação de Moran, LISA e mapas), com a
camada socioambiental (IVS e saneamento).

## Estrutura do repositório

```
Scripts/             Código R — tratamento (01-05), análise temporal (06-07), espacial (08-11)
Bancos_rds/          Bancos processados (.rds), prontos para análise
Resultados/Figuras/  Mapas e diagramas (.png) gerados pelas análises espaciais
Dados_brutos/        Dados brutos (parcialmente versionados — ver "Fontes de dados")
```

## Pipeline (executar a partir da raiz do projeto, nesta ordem)

| # | Script | Banco / Análise | Saídas |
|---|--------|-----------------|--------|
| 01 | `01_dados_populacao_ibge.R` | População (IBGE) + malha município→mesorregião | `lookup_muni_meso`, `pop_municipal_2010_2024`, `pop_mesorregiao_2010_2024` |
| 02 | `02_dados_casos_sinan.R` | Casos de leptospirose (SINAN/DATASUS) | `banco_leptospirose_unificado`, `base_MG_2010_2024`, `serie_semanal_2010_2024` |
| 03 | `03_dados_clima_brdwgd.R` | Precipitação e temperatura (BR-DWGD) | `clima_semanal_municipal`, `clima_semanal_mesorregiao` |
| 04 | `04_dados_saneamento_censo2022.R` | Saneamento básico (Censo 2022) | `saneamento_mg_2022` |
| 05 | `05_dados_ivs_ipea.R` | IVS (Atlas IPEA) + vulnerabilidade | `ivs_mg_2010`, `vulnerabilidade_mg` |
| 06 | `06_analise_temporal.R` | Correlação de Kendall (SE, lag 2; soma móvel 4 SE) | `dados_temporais` |
| 07 | `07_modelo_temporal.R` | Modelo Binomial Negativa (precip + temp) | — |
| 08 | `08_analise_espacial.R` | Moran global (univariado e bivariado) por município | `dados_espaciais_municipal` |
| 09 | `09_lisa_mapas.R` | LISA (clusters) + mapas de incidência e de cluster (confirmados) | figuras |
| 10 | `10_mapas_extras.R` | Mapas de notificados + LISA bivariado (incidência × precipitação) | figuras |
| 11 | `11_moran_scatterplots.R` | Diagramas de Moran (scatterplots) univariado e bivariado | figuras |

Os scripts 01–05 reconstroem os bancos a partir dos dados brutos. Como os bancos
processados (`Bancos_rds/`) estão versionados, é possível rodar as análises
(06–11) sem reprocessar tudo. As figuras (mapas e diagramas) ficam em `Resultados/Figuras/`.

## Fontes de dados brutos (`Dados_brutos/`)

| Fonte | Conteúdo | Onde / como obter |
|-------|----------|-------------------|
| **SINAN / DATASUS** | Notificações de leptospirose (`LEPTBR*.dbc`, 2010–2024) | `Dados_brutos/Datasus/` — TabNet/FTP do DATASUS |
| **BR-DWGD** (Xavier et al.) | Grade diária NetCDF de `pr`, `Tmax`, `Tmin` | `Dados_brutos/dados_clima/raw/` — sites.google.com/site/alexandrecandidoxavierufes |
| **IBGE** | Estimativas e Censos (população, saneamento) | online via pacote `sidrar` |
| **Malha territorial** | Municípios e mesorregiões de MG | online via pacote `geobr` |
| **Atlas IPEA (IVS)** | Base Completa do IVS (`atlasivs_dadosbrutos_pt_v2 - MG.xlsx`) | `Dados_brutos/dados_pop/` — ivs.ipea.gov.br |

> **Não versionados (excedem 100 MB/arquivo do GitHub):** as grades NetCDF do
> BR-DWGD (`Dados_brutos/dados_clima/raw/*.nc`, ~2,7 GB cada). O clima diário já
> extraído por município (`mg_clima_diario_2010_2024_FINAL.rds`) **está incluído**,
> então a pipeline roda sem as grades.

## Como rodar

```r
# a partir da raiz do projeto:
Rscript Scripts/01_dados_populacao_ibge.R
Rscript Scripts/02_dados_casos_sinan.R
Rscript Scripts/03_dados_clima_brdwgd.R
Rscript Scripts/04_dados_saneamento_censo2022.R
Rscript Scripts/05_dados_ivs_ipea.R
Rscript Scripts/06_analise_temporal.R
Rscript Scripts/07_modelo_temporal.R
Rscript Scripts/08_analise_espacial.R
Rscript Scripts/09_lisa_mapas.R
Rscript Scripts/10_mapas_extras.R
Rscript Scripts/11_moran_scatterplots.R
```

## Software

R (≥ 4.6). Pacotes: `read.dbc`, `dplyr`, `tidyr`, `lubridate`, `sidrar`, `geobr`,
`sf`, `terra`, `exactextractr`, `readxl`, `MASS`, `spdep`, `ggplot2`, `ggspatial`.
A leitura dos `.dbc` do DATASUS (`read.dbc`) requer **Rtools** (compilação).
