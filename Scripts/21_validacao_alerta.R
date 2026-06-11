# =============================================================================
# VALIDACAO PREDITIVA DO ALERTA CLIMATOLOGICO (fora da amostra)
#   Avalia se um gatilho climatologico simples antecipa as semanas de maior
#   incidencia, com validacao preditiva fora da amostra:
#   GATILHO  : chuva acumulada em 4 SE (pr4) acima do percentil climatologico
#              (p80/p90) da mesorregiao, estimado SO no treino.
#   DESFECHO : "semana de surto" = casos confirmados acima do percentil (p80) da
#              propria mesorregiao no treino.
#   REGRA    : o gatilho na SE w-2 preve surto na SE w (lead = 2 SE, periodo de
#              incubacao, fixado A PRIORI — nao escolhido por maximizar acerto).
#   PARTICAO : treino <= 2019 ; teste 2020-2024 (validacao temporal).
# Reporta: sensibilidade, taxa de falso-alarme (1-VPP), especificidade, VPP,
#          Heidke Skill Score (HSS) e lead-time (varredura de leads 0-4 SE).
# Saidas: console + Bancos_rds/validacao_alerta.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr)})

dt <- readRDS("Bancos_rds/dados_temporais.rds") |>
  mutate(sem_ini = as.Date(sem_ini), ano = as.integer(format(sem_ini, "%Y"))) |>
  arrange(name_meso, sem_ini) |>
  group_by(name_meso) |>
  mutate(pr4 = as.numeric(stats::filter(pr, rep(1,4), sides=1))) |>  # chuva acumulada 4 SE
  ungroup()

TR <- 2019  # ultimo ano de treino

# Avalia um gatilho (percentil de chuva qp) e um desfecho (percentil de casos qo),
# para um dado lead (em SE). Limiares estimados SO no treino, por mesorregiao.
avaliar <- function(qp, qo, lead) {
  tab <- dt |> group_by(name_meso) |> group_modify(function(d, ...) {
    tr <- d |> filter(ano <= TR)
    thr_pr <- quantile(tr$pr4,  qp, na.rm=TRUE)
    thr_ca <- max(quantile(tr$conf, qo, na.rm=TRUE), 1)         # limiar de surto (>=1 caso)
    d <- d |> mutate(alarme = pr4 > thr_pr,
                     prev   = dplyr::lag(alarme, lead),         # gatilho ha 'lead' SE
                     surto  = conf > thr_ca)
    d |> filter(ano > TR, is.finite(prev), is.finite(surto))    # avalia so no teste
  }) |> ungroup()
  TP <- sum(tab$prev & tab$surto); FP <- sum(tab$prev & !tab$surto)
  FN <- sum(!tab$prev & tab$surto); TN <- sum(!tab$prev & !tab$surto); N <- TP+FP+FN+TN
  sens <- TP/(TP+FN); spec <- TN/(TN+FP); ppv <- TP/(TP+FP); far <- FP/(TP+FP)
  # Heidke Skill Score (vs acerto esperado ao acaso)
  pe  <- ((TP+FP)*(TP+FN) + (TN+FN)*(TN+FP)) / N^2
  hss <- ((TP+TN)/N - pe) / (1 - pe)
  list(TP=TP,FP=FP,FN=FN,TN=TN, sens=sens, spec=spec, ppv=ppv, far=far, hss=hss,
       n_surto=TP+FN, n_alarme=TP+FP)
}

pr1 <- function(x) sprintf("%.0f%%", 100*x)
cat("=====================================================================\n")
cat(" Validacao preditiva do alerta (treino <=2019 | teste 2020-2024)\n")
cat("=====================================================================\n")

for (qp in c(0.80, 0.90)) {
  r <- avaliar(qp, 0.80, 2)
  cat(sprintf("\n[Gatilho: chuva 4 SE > p%.0f climatologico | surto: casos > p80 | lead 2 SE]\n", qp*100))
  cat(sprintf("  semanas de surto no teste: %d | alarmes emitidos: %d\n", r$n_surto, r$n_alarme))
  cat(sprintf("  Sensibilidade (surtos detectados) : %s\n", pr1(r$sens)))
  cat(sprintf("  Taxa de falso-alarme (1-VPP)      : %s\n", pr1(r$far)))
  cat(sprintf("  Especificidade                    : %s\n", pr1(r$spec)))
  cat(sprintf("  Valor preditivo positivo (VPP)    : %s\n", pr1(r$ppv)))
  cat(sprintf("  Heidke Skill Score (HSS)          : %.2f\n", r$hss))
}

# Lead-time: varredura para mostrar que 2 SE NAO foi escolhido por maximizar acerto
cat("\n-- Lead-time (sensibilidade x HSS por defasagem; gatilho p80, surto p80) --\n")
leads <- lapply(0:4, function(L){ r<-avaliar(0.80,0.80,L); c(lead=L, sens=r$sens, far=r$far, hss=r$hss) })
for (L in leads) cat(sprintf("  lead %d SE: sens=%s | falso-alarme=%s | HSS=%.2f\n",
                             L["lead"], pr1(L["sens"]), pr1(L["far"]), L["hss"]))

res <- list(p80=avaliar(0.80,0.80,2), p90=avaliar(0.90,0.80,2),
            leads=do.call(rbind, lapply(leads, as.list)))
saveRDS(res, "Bancos_rds/validacao_alerta.rds")
cat("\nVALID_OK\n")
