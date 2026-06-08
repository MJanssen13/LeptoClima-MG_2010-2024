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
