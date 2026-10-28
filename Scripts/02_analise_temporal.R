# =============================================================================
# 02 - ANALISE TEMPORAL
#   Correlacao de Kendall (SE, lag 2; soma movel 4 SE), modelo Binomial Negativa
#   (precipitacao + temperatura) e figura da serie temporal.
#   Reune os antigos scripts 06, 07 e 12.
# =============================================================================

# -----------------------------------------------------------------------------
# [bloco: 06_analise_temporal.R]
# -----------------------------------------------------------------------------
# =============================================================================
# ANALISE TEMPORAL - Correlacao incidencia de leptospirose x clima, por SE
#   Incidencia semanal por mesorregiao (casos/pop x 100 mil), lag de 2 SE
#   (precipitacao/temperatura na SE w-2 -> incidencia na SE w; periodo de incubacao)
# (A) Kendall com a serie SEMANAL
# (B) Sensibilidade: soma movel de 4 SE (atenua a esparsidade dos confirmados)
# (C) Temperatura como 2a variavel climatica
# (D) Colinearidade precipitacao x temperatura
# Bancos (gerados em 01-03): serie_semanal, pop_mesorregiao, clima_semanal_mesorregiao
# =============================================================================
suppressMessages({library(dplyr); library(lubridate); library(tidyr)})
cas <- readRDS("Bancos_rds/serie_semanal_2010_2024.rds")
pop <- readRDS("Bancos_rds/pop_mesorregiao_2010_2024.rds")
cli <- readRDS("Bancos_rds/clima_semanal_mesorregiao.rds")        # name_meso, sem_ini (Date), pr, tmean

rsum <- function(x, k = 4) as.numeric(stats::filter(x, rep(1, k), sides = 1))   # soma movel passada
ktau <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 10) return(c(NA, NA))
  r <- suppressWarnings(tryCatch(cor.test(x, y, method = "kendall"), error = function(e) NULL))
  if (is.null(r) || is.na(r$estimate)) c(NA, NA) else c(round(unname(r$estimate), 2), r$p.value)
}
st <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns"))))

# Painel semanal por mesorregiao com defasagens e somas moveis
panel <- cas |> mutate(ano = year(sem_ini)) |>
  left_join(pop, by = c("name_meso", "ano")) |>
  left_join(select(cli, name_meso, sem_ini, pr, tmean), by = c("name_meso", "sem_ini")) |>
  arrange(name_meso, sem_ini) |>
  group_by(name_meso) |>
  mutate(inc_conf   = conf / pop * 1e5, inc_noti = noti / pop * 1e5,
         pr_l2      = lag(pr, 2),  t_l2 = lag(tmean, 2),
         r_inc_conf = rsum(conf, 4) / pop * 1e5,
         r_pr_l2    = lag(rsum(pr, 4), 2)) |>
  ungroup()
saveRDS(panel, "Bancos_rds/dados_temporais.rds")

# ---- (A)+(B)+(C) por mesorregiao (confirmados; precip. notif. p/ comparacao) ----
res <- panel |> group_by(name_meso) |> group_modify(function(d, ...) {
  a  <- ktau(d$inc_conf,   d$pr_l2)    # (A) precip semanal
  b  <- ktau(d$r_inc_conf, d$r_pr_l2)  # (B) precip soma-movel 4 SE
  cc <- ktau(d$inc_conf,   d$t_l2)     # (C) temperatura
  nn <- ktau(d$inc_noti,   d$pr_l2)    # precip - notificados (sensibilidade)
  tibble(conf = sum(d$conf),
         tauP = a[1],  P  = st(a[2]),
         tauP4 = b[1], P4 = st(b[2]),
         tauT = cc[1], T  = st(cc[2]),
         tauPn = nn[1], Pn = st(nn[2]))
}) |> ungroup() |> arrange(desc(tauP))
cat("=== Kendall por mesorregiao (lag 2 SE) | confirmados ===\n")
cat("tauP=precip semanal  tauP4=precip 4SE  tauT=temperatura  tauPn=precip(notificados)\n")
print(as.data.frame(res))

# ---- Estadual ----
est <- panel |> group_by(sem_ini) |>
  summarise(pr = weighted.mean(pr, pop), tmean = weighted.mean(tmean, pop),
            conf = sum(conf), noti = sum(noti), pop = sum(pop), .groups = "drop") |>
  arrange(sem_ini) |>
  mutate(inc_conf = conf / pop * 1e5, inc_noti = noti / pop * 1e5,
         pr_l2 = lag(pr, 2), t_l2 = lag(tmean, 2),
         r_inc_conf = rsum(conf, 4) / pop * 1e5, r_pr_l2 = lag(rsum(pr, 4), 2))
eA <- ktau(est$inc_conf, est$pr_l2); eB <- ktau(est$r_inc_conf, est$r_pr_l2)
eC <- ktau(est$inc_conf, est$t_l2); eN <- ktau(est$inc_noti, est$pr_l2)
cat(sprintf("\n=== ESTADUAL === precip semanal tau=%.2f%s | precip 4SE tau=%.2f%s | temperatura tau=%.2f%s | precip(notif) tau=%.2f%s\n",
            eA[1], st(eA[2]), eB[1], st(eB[2]), eC[1], st(eC[2]), eN[1], st(eN[2])))

# ---- (D) Colinearidade precip x temperatura ----
cat(sprintf("\n=== Colinearidade (estadual, semanal): Spearman(precip, temp) = %.2f ===\n",
            cor(est$pr, est$tmean, method = "spearman", use = "complete.obs")))

# -----------------------------------------------------------------------------
# [bloco: 07_modelo_temporal.R]
# -----------------------------------------------------------------------------
# =============================================================================
# MODELO TEMPORAL MULTIVARIAVEL - separar os efeitos de chuva e temperatura
#   casos_semanais ~ precip(lag2SE) + temperatura(lag2SE) + mesorregiao + ano
#                    + offset(log populacao)
#   Distribuicao Binomial Negativa (sobredispersao). IRR por +1 desvio-padrao.
#   Justificativa: precip e temp sao colineares (Spearman ~0,50); o modelo
#   estima o efeito de cada uma ajustado pela outra.
# Entrada: dados_temporais.rds (painel semanal por mesorregiao, gerado em 06)
# =============================================================================
suppressMessages({library(MASS); library(dplyr)})

d <- readRDS("Bancos_rds/dados_temporais.rds") |>
  filter(!is.na(pr_l2), !is.na(t_l2), !is.na(pop), pop > 0) |>
  mutate(pr_z = as.numeric(scale(pr_l2)),       # padronizadas (efeito por DP)
         t_z  = as.numeric(scale(t_l2)),
         meso = factor(name_meso),
         yr   = ano - 2017)

fit_nb <- function(y)
  glm.nb(reformulate(c("pr_z", "t_z", "meso", "yr", "offset(log(pop))"), response = y), data = d)

irr <- function(m, v) { s <- summary(m)$coefficients[v, ]
  sprintf("%.3f (%.3f-%.3f) p=%.2g",
          exp(s["Estimate"]), exp(s["Estimate"] - 1.96 * s["Std. Error"]),
          exp(s["Estimate"] + 1.96 * s["Std. Error"]), s["Pr(>|z|)"]) }

cat("Modelo NB: casos ~ precip_z + temp_z + mesorregiao + ano + offset(log pop)\n")
cat("IRR (IC95%) por +1 desvio-padrao, lag de 2 SE\n")
for (y in c("noti", "conf")) {
  m <- suppressWarnings(fit_nb(y))
  cat(sprintf("\n=== %s (theta=%.2f) ===\n", ifelse(y == "noti", "NOTIFICADOS", "CONFIRMADOS"), m$theta))
  cat("  precipitacao: IRR =", irr(m, "pr_z"), "\n")
  cat("  temperatura : IRR =", irr(m, "t_z"), "\n")
}

# Comparacao univariavel x multivariavel (atenuacao pela colinearidade), notificados
uni <- function(x) { m <- suppressWarnings(glm.nb(reformulate(c(x, "meso", "yr", "offset(log(pop))"),
                     response = "noti"), data = d)); exp(coef(m)[x]) }
cat(sprintf("\n=== Univariavel vs multivariavel (IRR, notificados) ===\n"))
cat(sprintf("  precip sozinho: %.3f  ->  ajustado p/ temp: %.3f\n", uni("pr_z"),
            exp(coef(fit_nb("noti"))["pr_z"])))
cat(sprintf("  temp   sozinho: %.3f  ->  ajustado p/ precip: %.3f\n", uni("t_z"),
            exp(coef(fit_nb("noti"))["t_z"])))

# -----------------------------------------------------------------------------
# [bloco: 12_figura_temporal.R]
# -----------------------------------------------------------------------------
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

df <- full_join(prm, dplyr::select(inc, ym, incidencia), by = "ym") |> arrange(ym) |>   # dplyr:: (MASS mascara select nesta sessao unificada)
  mutate(incidencia = coalesce(incidencia, 0))
esc <- max(df$precip, na.rm = TRUE) / max(df$incidencia, na.rm = TRUE)

# Titulo/subtitulo NAO vao embutidos na imagem (exigencia da revista: legenda
# fora da figura). A chave de leitura do eixo duplo, que estava no subtitulo,
# passa a ser uma legenda de cores no topo do painel (barras x linha).
# Rotulos numericos com virgula decimal (norma pt-BR).
SERIE_PR  <- "Precipitação"
SERIE_INC <- "Incidência (casos confirmados)"
brk_x <- seq(as.Date("2010-01-01"), as.Date("2024-01-01"), by = "2 years")  # sem marca alem dos dados

g <- ggplot(df, aes(ym)) +
  geom_col(aes(y = precip, fill = SERIE_PR), alpha = 0.55) +
  geom_line(aes(y = incidencia * esc, color = SERIE_INC), linewidth = 0.6) +
  scale_fill_manual(values = setNames("#4575b4", SERIE_PR), name = NULL) +
  scale_color_manual(values = setNames("#d73027", SERIE_INC), name = NULL) +
  scale_y_continuous(name = "Precipitação (mm/mês)",
                     labels = scales::label_number(decimal.mark = ","),
                     sec.axis = sec_axis(~ . / esc, name = "Incidência (casos/100 mil hab.)",
                                         labels = scales::label_number(accuracy = 0.1,
                                                                       decimal.mark = ","))) +
  scale_x_date(breaks = brk_x, date_labels = "%Y") +
  labs(x = "Ano") +
  theme_minimal(base_size = 11) +
  theme(axis.title.y.left = element_text(color = "#4575b4"),
        axis.title.y.right = element_text(color = "#d73027"),
        panel.grid.minor = element_blank(),
        legend.position = "top", legend.direction = "horizontal",
        legend.justification = "left", legend.box = "horizontal",
        legend.box.spacing = unit(2, "pt"), legend.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(0.45, "cm"), legend.text = element_text(size = 9.5))
ggsave("Resultados/Figuras/fig_serie_temporal.png", g, width = 10, height = 5, dpi = 1200)
# Versao vetorial (PDF) exigida pela revista para graficos e mapas; cairo_pdf
# embute as fontes, evitando substituicao no sistema de submissao.
ggsave("Resultados/Figuras/fig_serie_temporal.pdf", g, width = 10, height = 5,
       device = cairo_pdf, bg = "white")
cat("Figura salva: Resultados/Figuras/fig_serie_temporal.png\n")
