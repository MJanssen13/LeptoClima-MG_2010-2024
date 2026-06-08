# =============================================================================
# DIAGRAMAS DE MORAN (Moran scatterplots) - leptospirose em MG (2010-2024)
#   Eixo X: incidencia do municipio (z-score, padronizada)
#   Eixo Y: defasagem espacial (media dos vizinhos) da variavel (z-score)
#   Os 4 quadrantes = os tipos LISA; a inclinacao da reta = Moran's I.
#   CLASSIFICACAO Alto/Baixo: relativa a MEDIA estadual (z-score > 0 = Alto;
#   z-score < 0 = Baixo). Cor por significancia local (LISA, p<0,05).
# Entrada: Bancos_rds/dados_espaciais_municipal.rds (com lisa_conf, lisa_biv)
# Saidas:  Resultados/Figuras/scatter_moran_*.png
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("spdep","sf","ggplot2","dplyr"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(spdep); library(sf); library(ggplot2); library(dplyr)})

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds")
sf_use_s2(FALSE)
nb <- poly2nb(muni, queen = TRUE); W <- nb2listw(nb, style = "W", zero.policy = TRUE)
cores <- c("Alto-Alto" = "#d7191c", "Baixo-Baixo" = "#2c7bb6", "Alto-Baixo" = "#fdae61",
           "Baixo-Alto" = "#abd9e9", "Nao significativo" = "grey80")

scatter <- function(x, yvar, cluster, titulo, ylab) {
  zx   <- as.numeric(scale(x))                              # incidencia padronizada (z)
  ylag <- lag.listw(W, as.numeric(scale(yvar)), zero.policy = TRUE)  # defasagem dos vizinhos
  I    <- coef(lm(ylag ~ zx))[2]                            # inclinacao = Moran's I
  df   <- data.frame(zx, ylag, cluster)
  ggplot(df, aes(zx, ylag, color = cluster)) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey60") +
    geom_vline(xintercept = 0, linetype = 2, color = "grey60") +
    geom_point(alpha = 0.7, size = 1.3) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.7, aes(group = 1)) +
    scale_color_manual(values = cores, name = "Cluster LISA", drop = FALSE) +
    annotate("text", x =  Inf, y =  Inf, label = "Alto-Alto",   hjust = 1.1, vjust = 1.6,  fontface = 2, color = "#b2182b", size = 3.5) +
    annotate("text", x = -Inf, y = -Inf, label = "Baixo-Baixo", hjust = -0.1, vjust = -0.8, fontface = 2, color = "#2166ac", size = 3.5) +
    annotate("text", x =  Inf, y = -Inf, label = "Alto-Baixo",  hjust = 1.1, vjust = -0.8, fontface = 2, color = "#e08214", size = 3.5) +
    annotate("text", x = -Inf, y =  Inf, label = "Baixo-Alto",  hjust = -0.1, vjust = 1.6, fontface = 2, color = "#4393c3", size = 3.5) +
    labs(title = titulo, subtitle = sprintf("Inclinacao da reta = Moran's I = %.2f", I),
         x = "Incidencia do municipio (z-score)", y = ylab) +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
}

# Univariado: incidencia x defasagem da propria incidencia
g1 <- scatter(muni$inc_conf, muni$inc_conf, muni$lisa_conf,
              "Diagrama de Moran - incidencia de leptospirose (confirmados), MG 2010-2024",
              "Defasagem espacial da incidencia (media dos vizinhos, z)")
ggsave("Resultados/Figuras/scatter_moran_univariado.png", g1, width = 8, height = 6.5, dpi = 300)

# Bivariado: incidencia x defasagem da precipitacao dos vizinhos
g2 <- scatter(muni$inc_conf, muni$precip, muni$lisa_biv,
              "Diagrama de Moran bivariado - incidencia (confirmados) x precipitacao, MG 2010-2024",
              "Defasagem espacial da PRECIPITACAO (media dos vizinhos, z)")
ggsave("Resultados/Figuras/scatter_moran_bivariado.png", g2, width = 8, height = 6.5, dpi = 300)

cat("Diagramas de Moran salvos em Resultados/Figuras/:\n",
    "- scatter_moran_univariado.png\n - scatter_moran_bivariado.png\n")
