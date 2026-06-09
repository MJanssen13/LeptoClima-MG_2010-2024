# =============================================================================
# AUGMENTA a base espacial municipal com as variaveis adicionais e calcula o
# Moran bivariado (incidencia x covariavel) de todas elas.
#   Novas covariaveis:
#     - Clima BR-DWGD: RH (umidade %), Rs (radiacao MJ/m2), u2 (vento m/s),
#       ETo (evapotranspiracao mm/dia) -> media municipal do periodo 2010-2024
#     - Densidade demografica = populacao media / area (hab/km2); usa-se log()
# Depende de: 08 (base espacial) e 15 (clima extra diario)
# Entrada: Bancos_rds/dados_espaciais_municipal.rds, mg_clima_extra_diario.rds
# Saida:   Bancos_rds/dados_espaciais_municipal.rds (com novas colunas)
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(sf); library(spdep)})
set.seed(1)

extra <- readRDS("Dados_brutos/dados_clima/mg_clima_extra_diario.rds")
em <- extra |> group_by(cod) |>   # media municipal do periodo (na.rm: 5 NA de u2 em 2011-03-05)
  summarise(rh = mean(rh, na.rm = TRUE), rs = mean(rs, na.rm = TRUE),
            u2 = mean(u2, na.rm = TRUE), eto = mean(eto, na.rm = TRUE), .groups = "drop")

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds")
muni <- dplyr::select(muni, -dplyr::any_of(c("rh","rs","u2","eto","area_km2","densidade","logdens")))  # idempotente
muni$area_km2  <- as.numeric(st_area(muni)) / 1e6
muni$densidade <- muni$pop / muni$area_km2
muni$logdens   <- log(muni$densidade)
muni <- left_join(muni, em, by = "cod")

cat("Medianas: RH", round(median(muni$rh),1), "% | Rs", round(median(muni$rs),1),
    "MJ/m2 | u2", round(median(muni$u2),2), "m/s | ETo", round(median(muni$eto),2),
    "mm/dia | dens", round(median(muni$densidade),1), "hab/km2\n")
cat("NA apos join:", sum(is.na(muni$rh)), "\n")
saveRDS(muni, "Bancos_rds/dados_espaciais_municipal.rds")

# ---- Moran bivariado (permutacao) ----
sf_use_s2(FALSE); nb <- poly2nb(muni, queen = TRUE); W <- nb2listw(nb, style = "W", zero.policy = TRUE)
st  <- function(p) ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns")))
bim <- function(x, y, nsim = 999) {
  zx <- as.numeric(scale(x)); zy <- as.numeric(scale(y))
  Ix <- function(zy){ ly <- lag.listw(W, zy, zero.policy = TRUE); sum(zx*ly)/sum(zx^2) }
  I <- Ix(zy); perm <- replicate(nsim, Ix(sample(zy)))
  c(I, (sum(abs(perm) >= abs(I)) + 1)/(nsim + 1))
}
vars <- c(precip="Precipitacao", temp="Temperatura", rh="Umidade relativa", rs="Radiacao solar",
          u2="Vento", eto="Evapotranspiracao", logdens="Densidade (log)",
          ivs="IVS", esgoto="Esgoto", agua="Agua", lixo="Lixo")
cat("\n=== Moran bivariado  (incidencia confirmados x covariavel) ===\n")
for (v in names(vars)) { r <- bim(muni$inc_conf, muni[[v]]); cat(sprintf("  %-18s I=%+.2f %s\n", vars[v], r[1], st(r[2]))) }
cat("\n=== Moran bivariado  (incidencia notificados x covariavel) ===\n")
for (v in names(vars)) { r <- bim(muni$inc_noti, muni[[v]]); cat(sprintf("  %-18s I=%+.2f %s\n", vars[v], r[1], st(r[2]))) }
cat("OK16\n")
