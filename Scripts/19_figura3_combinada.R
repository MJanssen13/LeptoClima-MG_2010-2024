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
a2 <- image_annotate(image_resize(a, geometry_size_pixels(height = H)),
                     "(A)", size = 60, weight = 700, gravity = "northwest", location = "+15+8")
b2 <- image_annotate(image_resize(b, geometry_size_pixels(height = H)),
                     "(B)", size = 60, weight = 700, gravity = "northwest", location = "+15+8")
combo <- image_border(image_append(c(a2, b2)), "white", "25x20")
image_write(combo, "Resultados/Figuras/fig3_lisa_biv_scatter.png")
cat("Figura 3 salva:", paste(image_info(combo)[c("width","height")], collapse = "x"), "px\n")
