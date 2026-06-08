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
