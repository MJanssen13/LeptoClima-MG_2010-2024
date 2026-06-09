# =============================================================================
# FIGURA 1 - serie temporal mensal, Minas Gerais, 2010-2024 (dois paineis):
#   A) incidencia mensal de leptospirose (confirmados)
#   B) climograma: precipitacao (barras, eixo esq.) + temperatura (linha, eixo dir.)
# Eixos honestos (3 escalas nao cabem em 2 eixos no mesmo painel); cores distintas
# e unidades explicitas, atendendo a critica do revisor ao duplo eixo confuso.
# Entradas: clima diario (FINAL), populacao municipal, casos
# Saida: Resultados/Figuras/fig_serie_temporal.png
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("patchwork")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(dplyr); library(lubridate); library(ggplot2); library(patchwork)})

clima <- readRDS("Dados_brutos/dados_clima/mg_clima_diario_2010_2024_FINAL.rds")
pop   <- readRDS("Bancos_rds/pop_municipal_2010_2024.rds")
casos <- readRDS("Bancos_rds/base_MG_2010_2024.rds")

clima$ano <- year(clima$date); clima$ym <- floor_date(clima$date, "month")
clim_m <- clima |>                                              # precip (soma) e temp (media) mensais/munic.
  group_by(cod, ym, ano) |>
  summarise(pr = sum(pr), tmean = mean(tmean), .groups = "drop") |>
  left_join(pop, by = c("cod", "ano")) |>
  group_by(ym) |>                                                # media estadual ponderada pela populacao
  summarise(precip = weighted.mean(pr, pop), temp = weighted.mean(tmean, pop), .groups = "drop")

popstate <- pop |> group_by(ano) |> summarise(p = sum(pop), .groups = "drop")
inc <- casos |> mutate(ym = floor_date(as.Date(DT_SIN_PRI), "month"), ano = year(ym)) |>
  filter(confirmado %in% TRUE, ano >= 2010, ano <= 2024) |>
  count(ym, ano) |> left_join(popstate, by = "ano") |> mutate(incidencia = n / p * 1e5)

df <- clim_m |> left_join(select(inc, ym, incidencia), by = "ym") |> arrange(ym) |>
  mutate(incidencia = coalesce(incidencia, 0))

# ---- Painel A: incidencia ----
pA <- ggplot(df, aes(ym, incidencia)) +
  geom_line(color = "#d73027", linewidth = 0.6) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", accuracy = 0.1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Incidência mensal de leptospirose (confirmados), precipitação e temperatura",
       subtitle = "Minas Gerais, 2010–2024", x = NULL, y = "Incidência\n(casos/100 mil hab.)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(size = 11.5, face = "bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank())

# ---- Painel B: climograma (temperatura reescalada ao eixo da precipitacao) ----
tmin <- min(df$temp); tmax <- max(df$temp); a <- max(df$precip) / (tmax - tmin); b <- -a * tmin
pB <- ggplot(df, aes(ym)) +
  geom_col(aes(y = precip), fill = "#4575b4", alpha = 0.55) +
  geom_line(aes(y = a * temp + b), color = "#fc8d59", linewidth = 0.6) +
  scale_y_continuous(name = "Precipitação (mm/mês)",
                     labels = scales::label_number(decimal.mark = ",", accuracy = 1),
                     sec.axis = sec_axis(~ (. - b) / a, name = "Temperatura (°C)",
                                         labels = scales::label_number(decimal.mark = ",", accuracy = 0.1))) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.title.y.left = element_text(color = "#4575b4"),
        axis.title.y.right = element_text(color = "#fc8d59"),
        panel.grid.minor = element_blank())

g <- pA / pB + plot_layout(heights = c(1, 1.15))
ggsave("Resultados/Figuras/fig_serie_temporal.png", g, width = 10, height = 6.6, dpi = 300)
cat("Figura salva: Resultados/Figuras/fig_serie_temporal.png |",
    "temp estadual:", round(tmin,1), "-", round(tmax,1), "C\n")
