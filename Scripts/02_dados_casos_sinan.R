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
