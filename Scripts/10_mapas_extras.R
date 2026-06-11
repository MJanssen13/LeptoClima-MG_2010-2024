# =============================================================================
# MAPAS EXTRAS - leptospirose em MG (municipal, 2010-2024)
#   - Incidencia e clusters LISA (NOTIFICADOS)
#   - LISA BIVARIADO: incidencia (confirmados) x precipitacao (lag espacial)
# Entrada: Bancos_rds/dados_espaciais_municipal.rds (com lisa_conf/lisa_noti, de 09)
# Saidas:  Resultados/Figuras/*.png
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("spdep","sf","ggplot2","ggspatial","dplyr"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(spdep); library(sf); library(ggplot2); library(ggspatial); library(dplyr)})

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds")
sf_use_s2(FALSE)
nb <- poly2nb(muni, queen = TRUE); W <- nb2listw(nb, style = "W", zero.policy = TRUE)

base_carto <- list(
  annotation_scale(location = "bl", width_hint = 0.25),
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_minimal(), height = unit(1, "cm"), width = unit(1, "cm")),
  theme_minimal(base_size = 11),
  theme(axis.text = element_blank(), panel.grid = element_blank()))
cores <- c("Alto-Alto" = "#d7191c", "Baixo-Baixo" = "#2c7bb6", "Alto-Baixo" = "#fdae61",
           "Baixo-Alto" = "#abd9e9", "Nao significativo" = "grey90")

# ---- Notificados: incidencia + LISA ----------------------------------------
ggsave("Resultados/Figuras/mapa_incidencia_notificados.png",
  ggplot(muni) + geom_sf(aes(fill = inc_noti), color = "grey85", linewidth = 0.05) +
    scale_fill_viridis_c(option = "rocket", direction = -1, trans = "sqrt",
                         name = "Incidencia\n(casos/100 mil hab)") +
    labs(title = "Incidencia acumulada de leptospirose (notificados)",
         subtitle = "Minas Gerais, 2010-2024") + base_carto,
  width = 9, height = 7, dpi = 300)

ggsave("Resultados/Figuras/mapa_lisa_notificados.png",
  ggplot(muni) + geom_sf(aes(fill = lisa_noti), color = "grey70", linewidth = 0.05) +
    scale_fill_manual(values = cores, name = "Cluster LISA", drop = FALSE) +
    labs(title = "Clusters espaciais (LISA) da incidencia (notificados)",
         subtitle = "Minas Gerais, 2010-2024") + base_carto,
  width = 9, height = 7, dpi = 300)

# ---- LISA bivariado: incidencia (confirmados) x precipitacao ----------------
biv_lisa <- function(x, y, nsim = 999) {
  set.seed(1); zx <- as.numeric(scale(x)); zy <- as.numeric(scale(y))
  lagy <- lag.listw(W, zy, zero.policy = TRUE); Ii <- zx * lagy
  cnt <- integer(length(zx))
  for (s in 1:nsim) { lp <- lag.listw(W, sample(zy), zero.policy = TRUE); cnt <- cnt + (abs(zx * lp) >= abs(Ii)) }
  p <- (cnt + 1) / (nsim + 1)
  quad <- ifelse(zx > 0 & lagy > 0, "Alto-Alto",
          ifelse(zx < 0 & lagy < 0, "Baixo-Baixo",
          ifelse(zx > 0 & lagy < 0, "Alto-Baixo", "Baixo-Alto")))
  factor(ifelse(p < 0.05, quad, "Nao significativo"),
         levels = c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Nao significativo"))
}
muni$lisa_biv <- biv_lisa(muni$inc_conf, muni$precip)
cat("LISA bivariado (incidencia confirmados x precipitacao):\n"); print(table(muni$lisa_biv))

ggsave("Resultados/Figuras/mapa_lisa_bivariado_precip.png",
  ggplot(muni) + geom_sf(aes(fill = lisa_biv), color = "grey70", linewidth = 0.05) +
    scale_fill_manual(values = cores, name = "Cluster LISA\nbivariado", drop = FALSE) +
    base_carto,
  width = 9, height = 7, dpi = 300)

saveRDS(muni, "Bancos_rds/dados_espaciais_municipal.rds")
cat("\nMapas extras salvos em Resultados/Figuras/:\n",
    "- mapa_incidencia_notificados.png\n - mapa_lisa_notificados.png\n - mapa_lisa_bivariado_precip.png\n")
