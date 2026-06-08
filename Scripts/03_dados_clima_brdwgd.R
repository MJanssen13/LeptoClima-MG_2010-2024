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
