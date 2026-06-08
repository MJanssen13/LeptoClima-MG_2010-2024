# =============================================================================
# ANALISE ESPACIAL - Autocorrelacao espacial (Moran) da leptospirose em MG
#   Unidade: municipio (853). Incidencia acumulada 2010-2024 (casos/100 mil hab).
#   (A) Moran global univariado  -> a incidencia se agrupa no espaco?
#   (B) Moran global bivariado    -> incidencia x precipitacao / IVS / saneamento
#   Pesos: contiguidade de rainha (queen), padronizados por linha.
#   Substitui o mapa de Kernel (criticado pelo revisor) por analise de incidencia.
# Bancos: base_MG, lookup_muni_meso, pop_municipal, clima_semanal_municipal,
#         vulnerabilidade_mg ; geometria via geobr.
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("spdep","sf","geobr","dplyr","lubridate"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(spdep); library(sf); library(geobr); library(dplyr); library(lubridate)})

# ---- 1. Dataset municipal --------------------------------------------------
casos  <- readRDS("Bancos_rds/base_MG_2010_2024.rds")        # cod6, confirmado
lookup <- readRDS("Bancos_rds/lookup_muni_meso.rds")         # cod (7 dig)
pop    <- readRDS("Bancos_rds/pop_municipal_2010_2024.rds")
cli    <- readRDS("Bancos_rds/clima_semanal_municipal.rds")
vuln   <- readRDS("Bancos_rds/vulnerabilidade_mg.rds")

lk6 <- lookup |> transmute(cod6 = cod %/% 10L, cod)
cas_mun <- casos |> filter(!is.na(cod6)) |>
  group_by(cod6) |> summarise(conf = sum(confirmado %in% TRUE), noti = n(), .groups = "drop") |>
  left_join(lk6, by = "cod6")

popm <- pop |> group_by(cod) |> summarise(pop = mean(pop), .groups = "drop")   # pop. media do periodo
clim <- cli |> mutate(ano = year(sem_ini)) |>
  group_by(cod, ano) |> summarise(pr_ano = sum(pr), tmean = mean(tmean), .groups = "drop") |>
  group_by(cod) |> summarise(precip = mean(pr_ano), temp = mean(tmean), .groups = "drop")

dados <- popm |>
  left_join(select(cas_mun, cod, conf, noti), by = "cod") |>
  mutate(conf = coalesce(conf, 0L), noti = coalesce(noti, 0L),
         inc_conf = conf / pop * 1e5, inc_noti = noti / pop * 1e5) |>
  left_join(clim, by = "cod") |>
  left_join(vuln, by = "cod") |>
  left_join(select(lookup, cod, name_meso), by = "cod")   # mesorregiao p/ relatorios

# ---- 2. Geometria + matriz de pesos espaciais (rainha) ---------------------
sf_use_s2(FALSE)
muni <- read_municipality(code_muni = 31, year = 2020, simplified = TRUE, showProgress = FALSE) |>
  mutate(cod = as.integer(code_muni)) |> left_join(dados, by = "cod")
nb <- poly2nb(muni, queen = TRUE)
W  <- nb2listw(nb, style = "W", zero.policy = TRUE)
cat("Municipios:", nrow(muni), "| vizinhos medios:", round(mean(card(nb)), 1), "\n\n")

# ---- 3. Moran global UNIVARIADO (incidencia) -------------------------------
mglob <- function(x) { m <- moran.mc(x, W, nsim = 999, zero.policy = TRUE)
  sprintf("I=%.3f  p=%.3f", m$statistic, m$p.value) }
cat("=== (A) Moran global univariado (incidencia acumulada) ===\n")
cat("  Confirmados:", mglob(muni$inc_conf), "\n")
cat("  Notificados:", mglob(muni$inc_noti), "\n")

# ---- 4. Moran global BIVARIADO (incidencia x covariavel) -------------------
bimoran <- function(x, y, nsim = 999) {
  zx <- as.numeric(scale(x)); zy <- as.numeric(scale(y))
  Ixy <- function(zy) { ly <- lag.listw(W, zy, zero.policy = TRUE); sum(zx * ly) / sum(zx^2) }
  I <- Ixy(zy)
  perm <- replicate(nsim, Ixy(sample(zy)))
  sprintf("I=%.3f  p=%.3f", I, (sum(abs(perm) >= abs(I)) + 1) / (nsim + 1))
}
cat("\n=== (B) Moran global bivariado: incidencia (confirmados) x covariavel ===\n")
for (v in c("precip", "temp", "ivs", "esgoto", "agua", "lixo")) {
  cat(sprintf("  inc_conf x %-7s: %s\n", v, bimoran(muni$inc_conf, muni[[v]])))
}
cat("\n  (sensibilidade - notificados)\n")
for (v in c("precip", "ivs", "esgoto")) {
  cat(sprintf("  inc_noti x %-7s: %s\n", v, bimoran(muni$inc_noti, muni[[v]])))
}

saveRDS(muni, "Bancos_rds/dados_espaciais_municipal.rds")
cat("\nSalvo: Bancos_rds/dados_espaciais_municipal.rds (sf p/ mapas)\n")
