# =============================================================================
# 03 - ANALISE ESPACIAL
#   Moran global (uni e bivariado), variaveis extras + densidade, LISA e mapas,
#   mapas extras (notificados/LISA bivariado), diagramas de Moran e Figura 3.
#   Reune os antigos scripts 08, 16, 09, 10, 11 e 19_figura3_combinada.
# =============================================================================

# -----------------------------------------------------------------------------
# [bloco: 08_analise_espacial.R]
# -----------------------------------------------------------------------------
# =============================================================================
# ANALISE ESPACIAL - Autocorrelacao espacial (Moran) da leptospirose em MG
#   Unidade: municipio (853). Incidencia acumulada 2010-2024 (casos/100 mil hab).
#   (A) Moran global univariado  -> a incidencia se agrupa no espaco?
#   (B) Moran global bivariado    -> incidencia x precipitacao / IVS / saneamento
#   Pesos: contiguidade de rainha (queen), padronizados por linha.
#   Desfecho espacial: incidencia acumulada (casos/100 mil hab.) por municipio.
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

# -----------------------------------------------------------------------------
# [bloco: 16_variaveis_extras_espacial.R]
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# [bloco: 09_lisa_mapas.R]
# -----------------------------------------------------------------------------
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
       width = 10, height = 7, dpi = 1200, bg = "white")

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
       width = 10, height = 7, dpi = 1200, bg = "white")

cat("\nMapas salvos em Resultados/Figuras/:\n - mapa_incidencia_confirmados.png\n - mapa_lisa_confirmados.png\n")

# -----------------------------------------------------------------------------
# [bloco: 10_mapas_extras.R]
# -----------------------------------------------------------------------------
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
  width = 9, height = 7, dpi = 1200)

ggsave("Resultados/Figuras/mapa_lisa_notificados.png",
  ggplot(muni) + geom_sf(aes(fill = lisa_noti), color = "grey70", linewidth = 0.05) +
    scale_fill_manual(values = cores, name = "Cluster LISA", drop = FALSE) +
    labs(title = "Clusters espaciais (LISA) da incidencia (notificados)",
         subtitle = "Minas Gerais, 2010-2024") + base_carto,
  width = 9, height = 7, dpi = 1200)

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
  width = 9, height = 7, dpi = 1200)

saveRDS(muni, "Bancos_rds/dados_espaciais_municipal.rds")
cat("\nMapas extras salvos em Resultados/Figuras/:\n",
    "- mapa_incidencia_notificados.png\n - mapa_lisa_notificados.png\n - mapa_lisa_bivariado_precip.png\n")

# -----------------------------------------------------------------------------
# [bloco: 11_moran_scatterplots.R]
# -----------------------------------------------------------------------------
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
ggsave("Resultados/Figuras/scatter_moran_univariado.png", g1, width = 8, height = 6.5, dpi = 1200)

# Bivariado: incidencia x defasagem da precipitacao dos vizinhos
g2 <- scatter(muni$inc_conf, muni$precip, muni$lisa_biv,
              "Diagrama de Moran bivariado - incidencia (confirmados) x precipitacao, MG 2010-2024",
              "Defasagem espacial da PRECIPITACAO (media dos vizinhos, z)")
g2 <- g2 + labs(title = NULL, subtitle = NULL)   # Figura 3: titulo no caption, nao no painel
ggsave("Resultados/Figuras/scatter_moran_bivariado.png", g2, width = 8, height = 6.5, dpi = 1200)

cat("Diagramas de Moran salvos em Resultados/Figuras/:\n",
    "- scatter_moran_univariado.png\n - scatter_moran_bivariado.png\n")

# -----------------------------------------------------------------------------
# [bloco: 19_figura3_combinada.R]
# -----------------------------------------------------------------------------
# =============================================================================
# FIGURA 3 (painel duplo): (A) mapa do LISA bivariado (incidencia x precipitacao)
# + (B) diagrama de dispersao de Moran bivariado. Junta dois PNGs ja gerados
# (scripts 10 e 11) em uma unica figura, respeitando o limite de 5 ilustracoes da CSP.
# Entradas: Resultados/Figuras/{mapa_lisa_bivariado_precip,scatter_moran_bivariado}.png
# Saida:    Resultados/Figuras/fig3_lisa_biv_scatter.png
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")
suppressMessages(library(magick))

a <- image_read("Resultados/Figuras/mapa_lisa_bivariado_precip.png")
b <- image_read("Resultados/Figuras/scatter_moran_bivariado.png")
H <- 1100                                                   # altura comum (px)

# Adiciona uma faixa branca no topo e coloca o rotulo (A)/(B) nela, acima do
# titulo de cada painel, evitando a sobreposicao com o titulo/legenda.
rotular <- function(img, lab) {
  img   <- image_resize(img, geometry_size_pixels(height = H))
  faixa <- image_blank(width = image_info(img)$width, height = 80, color = "white")
  image_annotate(image_append(c(faixa, img), stack = TRUE),
                 lab, size = 60, weight = 700, gravity = "northwest", location = "+15+8")
}
a2 <- rotular(a, "(A)")
b2 <- rotular(b, "(B)")
combo <- image_border(image_append(c(a2, b2)), "white", "25x20")
image_write(combo, "Resultados/Figuras/fig3_lisa_biv_scatter.png")
cat("Figura 3 salva:", paste(image_info(combo)[c("width","height")], collapse = "x"), "px\n")
