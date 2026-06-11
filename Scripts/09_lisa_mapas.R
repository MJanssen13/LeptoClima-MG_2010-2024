# =============================================================================
# LISA (Moran local) + MAPAS - leptospirose em MG (municipal, 2010-2024)
#   - Indicadores locais de associacao espacial (LISA): clusters Alto-Alto (hotspot),
#     Baixo-Baixo, Alto-Baixo, Baixo-Alto -> substitui o mapa de Kernel (criticado).
#   - Mapas coropleticos com escala, indicacao do Norte e legenda.
# Entrada: Bancos_rds/dados_espaciais_municipal.rds (gerado em 08)
# Saidas:  Resultados/Figuras/*.png ; Bancos_rds/dados_espaciais_municipal.rds (+ LISA)
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("spdep","sf","ggplot2","ggspatial","dplyr","cowplot"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(spdep); library(sf); library(ggplot2); library(ggspatial); library(dplyr); library(cowplot)})
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

# ---- Cartografia: inset do Brasil (situa MG) + rosa dos ventos --------------
# Estados do Brasil (gerados/cacheados em uf_brasil.rds); MG destacado.
uf_br <- readRDS("Bancos_rds/uf_brasil.rds") |> st_transform(st_crs(muni))
mg_uf <- uf_br[uf_br$abbrev_state == "MG", ]

inset_brasil <- function() {
  ggplot(uf_br) +
    geom_sf(fill = "white", color = "gray55", linewidth = 0.18) +
    geom_sf(data = mg_uf, fill = "black", color = "black", linewidth = 0.25) +
    annotation_custom(grid::textGrob("BRASIL", x = unit(0.04, "npc"), y = unit(0.05, "npc"),
                      hjust = 0, vjust = 0,
                      gp = grid::gpar(fontsize = 10, fontface = "bold", col = "gray20"))) +
    annotation_scale(location = "br", width_hint = 0.3, style = "bar", text_cex = 0.65,
                     line_width = 0.5, height = unit(0.12, "cm"),
                     pad_x = unit(0.15, "cm"), pad_y = unit(0.05, "cm"),
                     bar_cols = c("gray20","white")) +
    coord_sf(expand = TRUE, clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.12, 0.02))) +
    theme_void() +
    theme(plot.background  = element_rect(fill = "white", color = "gray85", linewidth = 0.3),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(2, 2, 4, 2))
}
plot_rosa_ventos <- function() {
  ggplot() + geom_blank() + coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE) +
    annotation_north_arrow(location = "tl", which_north = "grid",
      pad_x = unit(0,"cm"), pad_y = unit(0,"cm"),
      height = unit(2.0,"cm"), width = unit(2.0,"cm"),
      style = north_arrow_fancy_orienteering(text_size = 12, fill = c("white","gray15"),
                                             line_col = "gray15", text_col = "gray15")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
}
# Compoe um mapa coropletico de MG com o inset do Brasil e a rosa dos ventos.
montar_mapa <- function(mapa_principal) {
  mp <- mapa_principal +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.position = "top", legend.direction = "horizontal",
          legend.title.position = "top",
          legend.justification.top = "left",
          legend.title = element_text(size = 12, face = "bold", hjust = 0),
          legend.text  = element_text(size = 10))
  legenda    <- cowplot::get_legend(mp)
  mapa_clean <- mp + theme(legend.position = "none")
  cowplot::ggdraw() +
    cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = NA))) +
    cowplot::draw_plot(mapa_clean,          x = 0.00, y = 0.00, width = 0.76, height = 1.00) +
    cowplot::draw_plot(legenda,             x = 0.02, y = 0.82, width = 0.72, height = 0.16, halign = 0) +
    cowplot::draw_plot(plot_rosa_ventos(),  x = 0.79, y = 0.68, width = 0.19, height = 0.24) +
    cowplot::draw_plot(inset_brasil(),      x = 0.75, y = 0.22, width = 0.24, height = 0.40)
}

# ---- Mapas ------------------------------------------------------------------
base_carto <- list(
  annotation_scale(location = "bl", width_hint = 0.25, style = "bar",
                   bar_cols = c("gray15","white"), text_cex = 0.8),
  theme_void(base_size = 11)
)

# Mapa 1 - incidencia acumulada (confirmados)
m1 <- ggplot(muni) +
  geom_sf(aes(fill = inc_conf), color = "grey85", linewidth = 0.05) +
  scale_fill_viridis_c(option = "rocket", direction = -1, trans = "sqrt",
                       name = "Incidência (casos/100 mil hab.)",
                       guide = guide_colourbar(title.position = "top")) +
  base_carto +
  theme(legend.key.width = unit(1.6, "cm"), legend.key.height = unit(0.45, "cm"))
ggsave("Resultados/Figuras/mapa_incidencia_confirmados.png", montar_mapa(m1),
       width = 10, height = 7, dpi = 300, bg = "white")

# Mapa 2 - clusters LISA (confirmados)
cores <- c("Alto-Alto" = "#d7191c", "Baixo-Baixo" = "#2c7bb6", "Alto-Baixo" = "#fdae61",
           "Baixo-Alto" = "#abd9e9", "Nao significativo" = "grey90")
m2 <- ggplot(muni) +
  geom_sf(aes(fill = lisa_conf), color = "grey70", linewidth = 0.05) +
  scale_fill_manual(values = cores, name = "Cluster LISA", drop = FALSE,
                    guide = guide_legend(title.position = "top", nrow = 2)) +
  base_carto +
  theme(legend.key.size = unit(0.5, "cm"))
ggsave("Resultados/Figuras/mapa_lisa_confirmados.png", montar_mapa(m2),
       width = 10, height = 7, dpi = 300, bg = "white")

cat("\nMapas salvos em Resultados/Figuras/:\n - mapa_incidencia_confirmados.png\n - mapa_lisa_confirmados.png\n")
