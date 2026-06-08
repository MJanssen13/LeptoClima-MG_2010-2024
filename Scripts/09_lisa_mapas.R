# =============================================================================
# LISA (Moran local) + MAPAS - leptospirose em MG (municipal, 2010-2024)
#   - Indicadores locais de associacao espacial (LISA): clusters Alto-Alto (hotspot),
#     Baixo-Baixo, Alto-Baixo, Baixo-Alto -> substitui o mapa de Kernel (criticado).
#   - Mapas coropleticos com escala, indicacao do Norte e legenda.
# Entrada: Bancos_rds/dados_espaciais_municipal.rds (gerado em 08)
# Saidas:  Resultados/Figuras/*.png ; Bancos_rds/dados_espaciais_municipal.rds (+ LISA)
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("spdep","sf","ggplot2","ggspatial","dplyr"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(spdep); library(sf); library(ggplot2); library(ggspatial); library(dplyr)})
dir.create("Resultados/Figuras", recursive = TRUE, showWarnings = FALSE)

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds")
sf_use_s2(FALSE)
nb <- poly2nb(muni, queen = TRUE); W <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ---- LISA: classificacao em quadrantes (significancia p<0.05) ---------------
lisa_cluster <- function(x) {
  lm   <- localmoran(x, W, zero.policy = TRUE)
  zx   <- as.numeric(scale(x)); lagz <- lag.listw(W, zx, zero.policy = TRUE)
  quad <- ifelse(zx > 0 & lagz > 0, "Alto-Alto",
          ifelse(zx < 0 & lagz < 0, "Baixo-Baixo",
          ifelse(zx > 0 & lagz < 0, "Alto-Baixo", "Baixo-Alto")))
  factor(ifelse(lm[, 5] < 0.05, quad, "Nao significativo"),
         levels = c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Nao significativo"))
}
muni$lisa_conf <- lisa_cluster(muni$inc_conf)
muni$lisa_noti <- lisa_cluster(muni$inc_noti)
saveRDS(muni, "Bancos_rds/dados_espaciais_municipal.rds")

cat("=== Clusters LISA (confirmados) ===\n"); print(table(muni$lisa_conf))
cat("\nMunicipios HOTSPOT (Alto-Alto, confirmados) por mesorregiao:\n")
print(sort(table(muni$name_meso[muni$lisa_conf == "Alto-Alto"]), decreasing = TRUE))

# ---- Mapas ------------------------------------------------------------------
base_carto <- list(
  annotation_scale(location = "bl", width_hint = 0.25),
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_minimal(), height = unit(1, "cm"), width = unit(1, "cm")),
  theme_minimal(base_size = 11),
  theme(axis.text = element_blank(), panel.grid = element_blank())
)

# Mapa 1 - incidencia acumulada (confirmados)
m1 <- ggplot(muni) +
  geom_sf(aes(fill = inc_conf), color = "grey85", linewidth = 0.05) +
  scale_fill_viridis_c(option = "rocket", direction = -1, trans = "sqrt",
                       name = "Incidencia\n(casos/100 mil hab)") +
  labs(title = "Incidencia acumulada de leptospirose (confirmados)",
       subtitle = "Minas Gerais, 2010-2024") + base_carto
ggsave("Resultados/Figuras/mapa_incidencia_confirmados.png", m1, width = 9, height = 7, dpi = 300)

# Mapa 2 - clusters LISA (confirmados)
cores <- c("Alto-Alto" = "#d7191c", "Baixo-Baixo" = "#2c7bb6", "Alto-Baixo" = "#fdae61",
           "Baixo-Alto" = "#abd9e9", "Nao significativo" = "grey90")
m2 <- ggplot(muni) +
  geom_sf(aes(fill = lisa_conf), color = "grey70", linewidth = 0.05) +
  scale_fill_manual(values = cores, name = "Cluster LISA", drop = FALSE) +
  labs(title = "Clusters espaciais (LISA) da incidencia de leptospirose (confirmados)",
       subtitle = "Minas Gerais, 2010-2024  |  Alto-Alto = aglomerado de alto risco") + base_carto
ggsave("Resultados/Figuras/mapa_lisa_confirmados.png", m2, width = 9, height = 7, dpi = 300)

cat("\nMapas salvos em Resultados/Figuras/:\n - mapa_incidencia_confirmados.png\n - mapa_lisa_confirmados.png\n")
