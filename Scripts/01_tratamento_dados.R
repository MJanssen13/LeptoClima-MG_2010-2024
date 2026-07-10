# =============================================================================
# 01 - TRATAMENTO DOS DADOS
#   Prepara os bancos a partir dos dados brutos: populacao (IBGE), casos de
#   leptospirose (SINAN/DATASUS), clima (BR-DWGD: precip/temp e RH/Rs/u2/ETo),
#   saneamento (Censo 2022) e IVS (Atlas IPEA).
#   Reune os antigos scripts 01, 02, 03, 04, 05 e 15, nesta ordem de dependencia.
#   Requer internet (sidrar/geobr) e Rtools (read.dbc para os .dbc do DATASUS).
# =============================================================================

# -----------------------------------------------------------------------------
# [bloco: 01_dados_populacao_ibge.R]
# -----------------------------------------------------------------------------
# =============================================================================
# BANCO 01 - POPULACAO MUNICIPAL E POR MESORREGIAO (Minas Gerais, 2010-2024)
# Fontes (IBGE):
#   - Estimativas de populacao (SIDRA tab. 6579) ...... 2011-2021, 2024
#   - Censo 2010 (SIDRA tab. 1378, total via API) ..... 2010
#   - Censo 2022 (SIDRA tab. 4709) .................... 2022
#   - 2023 por interpolacao geometrica (2022 <-> 2024)
#   - Malha territorial (municipio -> mesorregiao) via geobr (join espacial)
# Saidas: lookup_muni_meso.rds, pop_municipal_2010_2024.rds, pop_mesorregiao_2010_2024.rds
# Obs.: requer internet (SIDRA/geobr).
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("sidrar","geobr","sf","dplyr","tidyr"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(sidrar); library(geobr); library(sf); library(dplyr); library(tidyr)})

# ---- 1. Lookup municipio (7 dig.) -> mesorregiao (join espacial geobr) -------
sf_use_s2(FALSE)
meso <- read_meso_region(code_meso = 31, year = 2020, simplified = TRUE, showProgress = FALSE)
muni <- read_municipality(code_muni = 31, year = 2020, simplified = TRUE, showProgress = FALSE)
cent <- suppressWarnings(st_centroid(muni)) |> st_transform(st_crs(meso))
lookup <- suppressWarnings(st_join(cent, meso[, "name_meso"], join = st_within)) |>
  st_drop_geometry() |>
  transmute(cod = as.integer(code_muni), name_muni, name_meso)
if (any(is.na(lookup$name_meso))) {                       # fallback: centroide fora do poligono
  miss <- which(is.na(lookup$name_meso))
  lookup$name_meso[miss] <- meso$name_meso[st_nearest_feature(cent[miss, ], meso)]
}
saveRDS(lookup, "Bancos_rds/lookup_muni_meso.rds")

# ---- 2. Populacao municipal por ano ----------------------------------------
clean <- function(d) d |>
  transmute(cod = as.integer(`Município (Código)`), pop = as.numeric(Valor)) |>
  filter(!is.na(cod))

# Estimativas oficiais IBGE (validadas contra os arquivos estimativa_dou do FTP)
est <- get_sidra(6579, variable = 9324, period = as.character(c(2011:2021, 2024)),
                 geo = "City", geo.filter = list("State" = 31)) |>
  transmute(cod = as.integer(`Município (Código)`), ano = as.integer(Ano), pop = as.numeric(Valor))
# Censo 2010 - tabela classificada por sexo/idade; total via API (todas classif. = 0)
c10 <- get_sidra(api = "/t/1378/n6/all/v/93/p/2010/c1/0/c2/0/c287/0/c455/0") |>
  clean() |> filter(substr(as.character(cod), 1, 2) == "31") |> mutate(ano = 2010L)
# Censo 2022
c22 <- get_sidra(4709, variable = 93, period = "2022",
                 geo = "City", geo.filter = list("State" = 31)) |> clean() |> mutate(ano = 2022L)

pop_municipal <- bind_rows(est, c10, c22) |>
  distinct(cod, ano, .keep_all = TRUE) |>
  pivot_wider(names_from = ano, values_from = pop, names_prefix = "a") |>
  mutate(a2023 = round(sqrt(a2022 * a2024))) |>              # interpolacao 2023
  pivot_longer(starts_with("a"), names_to = "ano", values_to = "pop") |>
  mutate(ano = as.integer(sub("a", "", ano))) |>
  filter(ano >= 2010, ano <= 2024, !is.na(pop)) |>
  arrange(cod, ano)
saveRDS(pop_municipal, "Bancos_rds/pop_municipal_2010_2024.rds")

# ---- 3. Populacao por mesorregiao ------------------------------------------
pop_meso <- pop_municipal |> left_join(lookup, by = "cod") |>
  filter(!is.na(name_meso)) |>
  group_by(name_meso, ano) |> summarise(pop = sum(pop), .groups = "drop")
saveRDS(pop_meso, "Bancos_rds/pop_mesorregiao_2010_2024.rds")

cat("OK populacao:", n_distinct(pop_municipal$cod), "municipios x",
    n_distinct(pop_municipal$ano), "anos | mesorregioes:", n_distinct(pop_meso$name_meso), "\n")

# -----------------------------------------------------------------------------
# [bloco: 02_dados_casos_sinan.R]
# -----------------------------------------------------------------------------
# =============================================================================
# BANCO 02 - CASOS DE LEPTOSPIROSE (SINAN/DATASUS, MG, 2010-2024)
# Entradas: Datasus/LEPTBR*.dbc  (arquivos de notificacao do SINAN, 2010-2025)
#           lookup_muni_meso.rds (gerado em 01_dados_populacao_ibge.R)
# Tratamento:
#   - Unificacao dos .dbc num unico banco nacional
#   - Recorte MG (municipio de residencia), agregacao por SEMANA EPIDEMIOLOGICA
#     (domingo) com base na data dos PRIMEIROS SINTOMAS
#   - Definicao de caso: confirmado (CLASSI_FIN==1) e notificado
# Saidas: banco_leptospirose_unificado.rds, base_MG_2010_2024.rds,
#         serie_semanal_2010_2024.rds
# Obs.: read.dbc exige Rtools (compilacao) e remotes::install_github("danicat/read.dbc")
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("remotes",  quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("read.dbc", quietly = TRUE)) remotes::install_github("danicat/read.dbc")
suppressMessages({library(read.dbc); library(dplyr); library(lubridate); library(tidyr)})

# ---- 1. Unificar arquivos .dbc do DATASUS ----------------------------------
arqs <- list.files("Dados_brutos/Datasus", pattern = "\\.dbc$", full.names = TRUE)
ano_arq <- function(n) {                                   # LEPTBR22.dbc -> 2022
  s <- as.integer(sub(".*BR(\\d{2})\\.dbc$", "\\1", basename(n), ignore.case = TRUE))
  ifelse(s <= 30, 2000L + s, 1900L + s)
}
banco <- bind_rows(lapply(arqs, function(a) {
  d <- read.dbc(a); d[] <- lapply(d, as.character); d$ANO_ARQUIVO <- as.character(ano_arq(a)); d
}))
saveRDS(banco, "Bancos_rds/banco_leptospirose_unificado.rds")

# ---- 2. Lookup municipio (6 digitos do SINAN) -> mesorregiao -----------------
lookup6 <- readRDS("Bancos_rds/lookup_muni_meso.rds") |>            # cod 7 dig.; SINAN usa 6 dig.
  transmute(cod6 = cod %/% 10L, name_meso)

# ---- 3. Casos de MG por semana epidemiologica (primeiros sintomas) ----------
banco$DT_SIN_PRI <- as.Date(banco$DT_SIN_PRI)
mg <- banco |>
  filter(substr(ID_MN_RESI, 1, 2) == "31", !is.na(DT_SIN_PRI)) |>
  mutate(sem_ini    = floor_date(DT_SIN_PRI, "week", week_start = 7),   # domingo (SE)
         ano_epi    = year(sem_ini),
         confirmado = CLASSI_FIN == "1",
         cod6       = as.integer(ID_MN_RESI)) |>
  filter(ano_epi >= 2010, ano_epi <= 2024) |>
  left_join(lookup6, by = "cod6")
saveRDS(mg, "Bancos_rds/base_MG_2010_2024.rds")

# ---- 4. Serie semanal por mesorregiao (confirmados e notificados) -----------
mesos   <- sort(unique(na.omit(lookup6$name_meso)))
semanas <- seq(floor_date(as.Date("2010-01-03"), "week", week_start = 7),
               floor_date(as.Date("2024-12-29"), "week", week_start = 7), by = "7 days")
agg <- function(x) x |> filter(!is.na(name_meso)) |> count(name_meso, sem_ini, name = "n")
serie <- expand_grid(sem_ini = semanas, name_meso = mesos) |>
  left_join(agg(filter(mg, confirmado %in% TRUE)), by = c("name_meso", "sem_ini")) |> rename(conf = n) |>
  left_join(agg(mg),                                by = c("name_meso", "sem_ini")) |> rename(noti = n) |>
  mutate(across(c(conf, noti), ~replace_na(., 0)), ano = year(sem_ini))
saveRDS(serie, "Bancos_rds/serie_semanal_2010_2024.rds")

cat("OK casos: banco nacional", nrow(banco), "| MG 2010-2024", nrow(mg),
    "casos | confirmados", sum(mg$confirmado, na.rm = TRUE),
    "| sem mesorregiao", sum(is.na(mg$name_meso)), "\n")

# -----------------------------------------------------------------------------
# [bloco: 03_dados_clima_brdwgd.R]
# -----------------------------------------------------------------------------
# =============================================================================
# BANCO 03 - CLIMA: PRECIPITACAO E TEMPERATURA (BR-DWGD, MG, 2010-2024)
# Fonte: Brazilian Daily Weather Gridded Data (Xavier et al.), grade diaria
#        0,1 graus. Arquivos NetCDF brutos da particao 2001-2025 em dados_clima/raw/:
#          pr_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc   (precipitacao)
#          Tmax_..._v_3.2.4.nc  Tmin_..._v_3.2.4.nc              (temperatura)
# Tratamento:
#   - Estatistica zonal (media das celulas) por municipio de MG -> serie diaria
#   - Agregacao por SEMANA EPIDEMIOLOGICA: precip = soma, temperatura = media
#   - Mesorregiao = media ponderada pela populacao municipal
# Entradas auxiliares: lookup_muni_meso.rds, pop_municipal_2010_2024.rds (banco 01)
# Saidas: dados_clima/mg_clima_diario_2010_2024_FINAL.rds,
#         clima_semanal_municipal.rds, clima_semanal_mesorregiao.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("terra","sf","exactextractr","geobr","dplyr","lubridate"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(terra); library(sf); library(exactextractr); library(geobr)
                  library(dplyr); library(lubridate)})

# ---- 1. Extracao diaria por municipio (estatistica zonal das grades) --------
fout <- "Dados_brutos/dados_clima/mg_clima_diario_2010_2024_FINAL.rds"
if (!file.exists(fout)) {
  muni <- read_municipality(code_muni = 31, year = 2020, simplified = TRUE, showProgress = FALSE)
  ex_var <- function(file) {
    r <- rast(file); if (is.na(crs(r, describe = TRUE)$code)) crs(r) <- "EPSG:4326"
    m <- st_transform(muni, crs(r)); tt <- terra::time(r)
    idx <- which(tt >= as.Date("2010-01-01") & tt <= as.Date("2024-12-31"))
    ex <- exact_extract(crop(r[[idx]], vect(m)), m, "mean", progress = FALSE)
    list(mat = as.matrix(ex), cods = as.integer(m$code_muni), dates = as.Date(tt[idx]))
  }
  P <- ex_var("Dados_brutos/dados_clima/raw/pr_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc")
  X <- ex_var("Dados_brutos/dados_clima/raw/Tmax_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc")
  N <- ex_var("Dados_brutos/dados_clima/raw/Tmin_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc")
  nd <- length(P$dates); nc <- length(P$cods)
  clima <- data.frame(cod = rep(P$cods, times = nd), date = rep(P$dates, each = nc),
                      pr = as.vector(P$mat), tmax = as.vector(X$mat), tmin = as.vector(N$mat))
  clima$tmean <- (clima$tmax + clima$tmin) / 2
  saveRDS(clima, fout)
}
clima <- readRDS(fout)

# ---- 2. Agregacao por semana epidemiologica (sem_ini = domingo, classe Date) -
clima$sem_ini <- floor_date(as.Date(clima$date), "week", week_start = 7)
sem_mun <- clima |>
  group_by(cod, sem_ini) |>
  summarise(pr = sum(pr), tmean = mean(tmean), tmax = mean(tmax), tmin = mean(tmin),
            ndias = n(), .groups = "drop") |>
  mutate(ano = year(sem_ini)) |> filter(ano >= 2010, ano <= 2024)
saveRDS(sem_mun, "Bancos_rds/clima_semanal_municipal.rds")

# ---- 3. Mesorregiao: media ponderada pela populacao municipal ---------------
lookup <- readRDS("Bancos_rds/lookup_muni_meso.rds")
pop    <- readRDS("Bancos_rds/pop_municipal_2010_2024.rds")
sem_meso <- sem_mun |>
  left_join(lookup, by = "cod") |> left_join(pop, by = c("cod", "ano")) |>
  filter(!is.na(name_meso), !is.na(pop)) |>
  group_by(name_meso, sem_ini) |>
  summarise(pr = weighted.mean(pr, pop), tmean = weighted.mean(tmean, pop),
            ndias = max(ndias), .groups = "drop")
saveRDS(sem_meso, "Bancos_rds/clima_semanal_mesorregiao.rds")

cat("OK clima: diario", nrow(clima), "linhas | semanal munic", nrow(sem_mun),
    "| semanal mesorreg", nrow(sem_meso), "\n")

# -----------------------------------------------------------------------------
# [bloco: 04_dados_saneamento_censo2022.R]
# -----------------------------------------------------------------------------
# =============================================================================
# BANCO 04 - SANEAMENTO BASICO (Censo 2022, MG, por municipio)
# Fonte (IBGE/SIDRA, dados do universo do Censo 2022):
#   - tab. 6805  Esgotamento sanitario
#   - tab. 6803  Abastecimento de agua
#   - tab. 6892  Destino do lixo
# Indicadores (% de domicilios particulares permanentes ocupados):
#   - esgoto: "Rede geral, rede pluvial ou fossa ligada a rede"
#   - agua:   "Possui ligacao a rede geral e a utiliza como forma principal"
#   - lixo:   "Coletado"
# Saida: saneamento_mg_2022.rds
# Obs.: requer internet (SIDRA).
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("sidrar","dplyr")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(sidrar); library(dplyr)})

# colunas-padrao do retorno do SIDRA (a coluna de classificacao e o complemento)
std <- c("Nível Territorial (Código)","Nível Territorial","Unidade de Medida (Código)",
         "Unidade de Medida","Valor","Município (Código)","Município","Ano (Código)","Ano",
         "Variável (Código)","Variável")

pct_adeq <- function(tab, label) {                # % da categoria 'label' sobre o Total
  d  <- get_sidra(tab, period = "2022", geo = "City", geo.filter = list("State" = 31))
  cl <- setdiff(names(d), std); cl <- cl[!grepl("Código", cl)][1]   # coluna de categoria
  d2 <- data.frame(cod = as.integer(d[["Município (Código)"]]), cat = d[[cl]],
                   val = as.numeric(d[["Valor"]]))
  tot <- d2 |> filter(cat == "Total") |> select(cod, total = val)
  adq <- d2 |> filter(cat == label)   |> select(cod, adq   = val)
  inner_join(tot, adq, by = "cod") |> mutate(pct = round(100 * adq / total, 2)) |> select(cod, pct)
}

san <- pct_adeq(6805, "Rede geral, rede pluvial ou fossa ligada à rede") |> rename(esgoto = pct) |>
  inner_join(pct_adeq(6803, "Possui ligação à rede geral e a utiliza como forma principal") |>
               rename(agua = pct), by = "cod") |>
  inner_join(pct_adeq(6892, "Coletado") |> rename(lixo = pct), by = "cod")
saveRDS(san, "Bancos_rds/saneamento_mg_2022.rds")

cat("OK saneamento:", nrow(san), "municipios | medias MG -> esgoto",
    round(mean(san$esgoto), 1), "% agua", round(mean(san$agua), 1),
    "% lixo", round(mean(san$lixo), 1), "%\n")

# -----------------------------------------------------------------------------
# [bloco: 05_dados_ivs_ipea.R]
# -----------------------------------------------------------------------------
# =============================================================================
# BANCO 05 - IVS: INDICE DE VULNERABILIDADE SOCIAL (Atlas IPEA, MG, 2010)
# Entrada: dados_pop/atlasivs_dadosbrutos_pt_v2 - MG.xlsx
#   (Base Completa do Atlas da Vulnerabilidade Social - ivs.ipea.gov.br,
#    ja filtrada para Minas Gerais; aba "Dados brutos IVS")
# Tratamento:
#   - Recorte do nivel MUNICIPIO (nivel == "regiao,uf,rm,municipio"), ano 2010,
#     e do TOTAL geral (cor/sexo/situacao de domicilio = Total)
#   - IVS e 3 subindices: Infraestrutura Urbana, Capital Humano, Renda e Trabalho
#   - Combina com saneamento (banco 04) -> camada municipal de vulnerabilidade
# Saidas: ivs_mg_2010.rds, vulnerabilidade_mg.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("readxl","dplyr")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(readxl); library(dplyr)})

f <- "Dados_brutos/dados_pop/atlasivs_dadosbrutos_pt_v2 - MG.xlsx"
d <- suppressMessages(read_excel(f, sheet = 1, col_types = "text"))   # tudo texto (codigos/decimais)
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)))))

ivs <- d |>
  filter(nivel == "regiao,uf,rm,municipio", ano == "2010",
         label_cor == "Total Cor", label_sexo == "Total Sexo",
         label_sit_dom == "Total Situação de Domicílio") |>
  transmute(cod    = as.integer(municipio),
            ivs    = num(ivs),
            ivs_iu = num(ivs_infraestrutura_urbana),   # Infraestrutura Urbana
            ivs_ch = num(ivs_capital_humano),          # Capital Humano
            ivs_rt = num(ivs_renda_e_trabalho)) |>     # Renda e Trabalho
  arrange(cod)
saveRDS(ivs, "Bancos_rds/ivs_mg_2010.rds")

# Camada municipal de vulnerabilidade (IVS 2010 + saneamento Censo 2022)
vuln <- full_join(ivs, readRDS("Bancos_rds/saneamento_mg_2022.rds"), by = "cod")
saveRDS(vuln, "Bancos_rds/vulnerabilidade_mg.rds")

cat("OK IVS:", nrow(ivs), "municipios | NA:", sum(is.na(ivs$ivs)),
    "| IVS range:", paste(round(range(ivs$ivs, na.rm = TRUE), 3), collapse = "-"),
    "| vulnerabilidade combinada:", nrow(vuln), "munic.\n")

# -----------------------------------------------------------------------------
# [bloco: 15_extrai_clima_extra.R]
# -----------------------------------------------------------------------------
# =============================================================================
# EXTRACAO das variaveis adicionais do BR-DWGD por municipio de MG (2010-2024)
#   RH (umidade relativa, %), Rs (radiacao solar, MJ/m2), u2 (vento, m/s),
#   ETo (evapotranspiracao, mm). Estatistica zonal (media das celulas).
# Entrada: Dados_brutos/dados_clima/raw/{RH,Rs,u2,ETo}_20010101_20251231_*.nc
# Saida:   Dados_brutos/dados_clima/mg_clima_extra_diario.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(terra); library(sf); library(exactextractr); library(geobr)})

muni <- read_municipality(code_muni = 31, year = 2020, simplified = TRUE, showProgress = FALSE)
ex_var <- function(file) {
  r <- rast(file); if (is.na(crs(r, describe = TRUE)$code)) crs(r) <- "EPSG:4326"
  m <- st_transform(muni, crs(r)); tt <- terra::time(r)
  idx <- which(tt >= as.Date("2010-01-01") & tt <= as.Date("2024-12-31"))
  ex <- exact_extract(crop(r[[idx]], vect(m)), m, "mean", progress = FALSE)
  cat(basename(file), "ok", format(Sys.time(), "%H:%M:%S"), "\n")
  list(mat = as.matrix(ex), cods = as.integer(m$code_muni), dates = as.Date(tt[idx]))
}
base <- "Dados_brutos/dados_clima/raw/"
RH  <- ex_var(paste0(base, "RH_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
RS  <- ex_var(paste0(base, "Rs_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
U2  <- ex_var(paste0(base, "u2_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
ETo <- ex_var(paste0(base, "ETo_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))

nd <- length(RH$dates); nc <- length(RH$cods)
extra <- data.frame(cod = rep(RH$cods, times = nd), date = rep(RH$dates, each = nc),
                    rh = as.vector(RH$mat), rs = as.vector(RS$mat),
                    u2 = as.vector(U2$mat), eto = as.vector(ETo$mat))
saveRDS(extra, "Dados_brutos/dados_clima/mg_clima_extra_diario.rds")
cat("Linhas:", nrow(extra), "| munic:", length(unique(extra$cod)),
    "| NA:", sum(is.na(extra$rh) | is.na(extra$rs) | is.na(extra$u2) | is.na(extra$eto)), "\nEXTRA_OK\n")
