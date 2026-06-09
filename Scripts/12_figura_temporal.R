# =============================================================================
# FIGURA - serie temporal: incidencia mensal de leptospirose x precipitacao
#   Estadual, 2010-2024. Eixo duplo, cores distintas (precip azul / incidencia
#   vermelho), unidades explicitas. Atende a critica do revisor a figura de
#   duplo eixo (linhas/cores distinguiveis).
# Entradas: clima diario (FINAL), populacao municipal, casos
# Saida: Resultados/Figuras/fig_serie_temporal.png
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(lubridate); library(ggplot2)})

clima <- readRDS("Dados_brutos/dados_clima/mg_clima_diario_2010_2024_FINAL.rds")
pop   <- readRDS("Bancos_rds/pop_municipal_2010_2024.rds")
casos <- readRDS("Bancos_rds/base_MG_2010_2024.rds")

clima$ano <- year(clima$date); clima$ym <- floor_date(clima$date, "month")
prm <- clima |>
  group_by(cod, ym, ano) |> summarise(pr = sum(pr), .groups = "drop") |>     # precip mensal/municipio
  left_join(pop, by = c("cod", "ano")) |>
  group_by(ym) |> summarise(precip = weighted.mean(pr, pop), .groups = "drop")  # media estadual ponderada

popstate <- pop |> group_by(ano) |> summarise(p = sum(pop), .groups = "drop")
inc <- casos |> mutate(ym = floor_date(as.Date(DT_SIN_PRI), "month"), ano = year(ym)) |>
  filter(confirmado %in% TRUE, ano >= 2010, ano <= 2024) |>
  count(ym, ano) |> left_join(popstate, by = "ano") |>
  mutate(incidencia = n / p * 1e5)

df <- full_join(prm, select(inc, ym, incidencia), by = "ym") |> arrange(ym) |>
  mutate(incidencia = coalesce(incidencia, 0))
esc <- max(df$precip, na.rm = TRUE) / max(df$incidencia, na.rm = TRUE)

g <- ggplot(df, aes(ym)) +
  geom_col(aes(y = precip), fill = "#4575b4", alpha = 0.55) +
  geom_line(aes(y = incidencia * esc), color = "#d73027", linewidth = 0.6) +
  scale_y_continuous(name = "Precipitação (mm/mês)",
                     sec.axis = sec_axis(~ . / esc, name = "Incidência (casos/100 mil hab.)")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Incidência mensal de leptospirose (confirmados) e precipitação",
       subtitle = "Minas Gerais, 2010–2024  (barras = precipitação; linha = incidência)", x = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.title.y.left = element_text(color = "#4575b4"),
        axis.title.y.right = element_text(color = "#d73027"),
        panel.grid.minor = element_blank())
ggsave("Resultados/Figuras/fig_serie_temporal.png", g, width = 10, height = 5, dpi = 300)
cat("Figura salva: Resultados/Figuras/fig_serie_temporal.png\n")
