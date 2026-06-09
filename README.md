# LeptoClima-MG_2010-2024

Associação entre o clima (precipitação, temperatura, umidade relativa, radiação
solar, vento e evapotranspiração), a densidade demográfica e a incidência de
**leptospirose** nas 12 mesorregiões de **Minas Gerais**, de **2010 a 2024**,
analisada por **semana epidemiológica (SE)** com defasagem de 2 SE (período de
incubação). Inclui **análise temporal** (correlação de Kendall e modelo Binomial
Negativa) e **análise espacial** (autocorrelação de Moran, LISA e mapas), com a
camada socioambiental (IVS e saneamento).

## Estrutura do repositório

```
Scripts/             Código R — tratamento (01-05, 15), descritiva (17), temporal (06-07), espacial (08-11, 16), tabelas/figuras (12-14)
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
| 12 | `12_figura_temporal.R` | Figura da série temporal (incidência mensal × precipitação) | figura |
| 13 | `13_tabelas.R` | Tabelas 1 (Kendall) e 2 (IRR + Moran bivariado de 11 variáveis) | `tabela1.csv`, `tabela2.csv` |
| 14 | `14_gerar_docx.R` | Gera o manuscrito em `.docx` (uso local; não versionado) | — |
| 15 | `15_extrai_clima_extra.R` | Extrai RH, Rs, u2 e ETo (BR-DWGD) por município | `mg_clima_extra_diario.rds` |
| 16 | `16_variaveis_extras_espacial.R` | Adiciona clima extra + densidade à base espacial e calcula o Moran bivariado | `dados_espaciais_municipal` (atualizado) |
| 17 | `17_caracterizacao_casos.R` | Caracterização descritiva dos casos (sexo, idade, raça/cor, evolução/letalidade, hospitalização, antecedentes, tendência anual Mann-Kendall e sazonalidade) | console |
| 18 | `18_sensibilidade.R` | Sensibilidade: regionalização (mesorregião × macrorregião de saúde, PDR/SES-MG) e defasagem temporal (2 SE × 0 SE) | `lookup_macrorregiao_saude`, `sensibilidade` |
| 19 | `19_analises_complementares.R` | Complementares: BN com termo sazonal (R3) e modelo de erro espacial multivariável (R4) | `analises_complementares` |

Os scripts 01–05 reconstroem os bancos a partir dos dados brutos. Como os bancos
processados (`Bancos_rds/`) estão versionados, é possível rodar as análises
(06–16) sem reprocessar tudo. As figuras (mapas e diagramas) ficam em `Resultados/Figuras/`.

## Fontes de dados brutos (`Dados_brutos/`)

| Fonte | Conteúdo | Onde / como obter |
|-------|----------|-------------------|
| **SINAN / DATASUS** | Notificações de leptospirose (`LEPTBR*.dbc`, 2010–2024) | `Dados_brutos/Datasus/` — TabNet/FTP do DATASUS |
| **BR-DWGD** (Xavier et al.) | Grade diária NetCDF de `pr`, `Tmax`, `Tmin`, `RH`, `Rs`, `u2`, `ETo` | `Dados_brutos/dados_clima/raw/` — sites.google.com/site/alexandrecandidoxavierufes |
| **IBGE** | Estimativas e Censos (população, saneamento) | online via pacote `sidrar` |
| **Malha territorial** | Municípios e mesorregiões de MG | online via pacote `geobr` |
| **Atlas IPEA (IVS)** | Base Completa do IVS (`atlasivs_dadosbrutos_pt_v2 - MG.xlsx`) | `Dados_brutos/dados_pop/` — ivs.ipea.gov.br |
| **SES-MG (PDR 2026)** | Regionalização de saúde (município → macrorregião de saúde) — sensibilidade (script 18) | `Dados_brutos/dados_pop/Planilha-de-Regionalizacao_SES-MG_2026.xlsx` — saude.mg.gov.br |

> **Não versionados (excedem 100 MB/arquivo do GitHub):** as grades NetCDF do
> BR-DWGD (`Dados_brutos/dados_clima/raw/*.nc`, ~2,7 GB cada). O clima diário já
> extraído por município (`mg_clima_diario_2010_2024_FINAL.rds`, precip/temp, e
> `mg_clima_extra_diario.rds`, RH/Rs/u2/ETo) **está incluído**, então a pipeline
> roda sem as grades.

## Como rodar

```r
# a partir da raiz do projeto:
Rscript Scripts/01_dados_populacao_ibge.R
Rscript Scripts/02_dados_casos_sinan.R
Rscript Scripts/03_dados_clima_brdwgd.R
Rscript Scripts/04_dados_saneamento_censo2022.R
Rscript Scripts/05_dados_ivs_ipea.R
Rscript Scripts/15_extrai_clima_extra.R       # RH, Rs, u2, ETo (BR-DWGD)
Rscript Scripts/06_analise_temporal.R
Rscript Scripts/07_modelo_temporal.R
Rscript Scripts/08_analise_espacial.R
Rscript Scripts/16_variaveis_extras_espacial.R  # + densidade e clima extra (espacial)
Rscript Scripts/09_lisa_mapas.R
Rscript Scripts/10_mapas_extras.R
Rscript Scripts/11_moran_scatterplots.R
Rscript Scripts/12_figura_temporal.R
Rscript Scripts/13_tabelas.R
Rscript Scripts/17_caracterizacao_casos.R    # caracterizacao descritiva dos casos
```

## Software

R (≥ 4.6). Pacotes: `read.dbc`, `dplyr`, `tidyr`, `lubridate`, `sidrar`, `geobr`,
`sf`, `terra`, `exactextractr`, `readxl`, `MASS`, `spdep`, `ggplot2`, `ggspatial`.
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
> Brasil, 2010–2024. v1.0.0. Zenodo; 2026. [inserir DOI do Zenodo após o depósito]

Quando publicado, cite também o artigo correspondente (submetido à *Cadernos de Saúde Pública*).
