# =============================================================================
# INTERVALOS DE CONFIANCA ROBUSTOS A AUTOCORRELACAO TEMPORAL
#   O modelo Binomial Negativa trata ~9.400 regiao-semanas como independentes,
#   ignorando a autocorrelacao temporal das contagens semanais e subestimando os
#   IC da IRR. Aqui reestimam-se os erros-padrao do efeito de precipitacao e
#   temperatura de tres formas que respeitam a dependencia temporal:
#   (1) robusto por agrupamento na mesorregiao (12 clusters);
#   (2) robusto por agrupamento na mesorregiao-ano (180 clusters);
#   (3) GEE Poisson com correlacao de trabalho AR(1) dentro da mesorregiao.
# Saidas: console + Bancos_rds/ic_robustos.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(MASS); library(sandwich); library(lmtest); library(geepack)})
st <- function(p) ifelse(is.na(p),"", ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns"))))

dt <- readRDS("Bancos_rds/dados_temporais.rds") |>
  filter(is.finite(pr_l2), is.finite(t_l2), pop > 0) |>
  mutate(pr_z = as.numeric(scale(pr_l2)), t_z = as.numeric(scale(t_l2)),
         meso = factor(name_meso), yr = ano - 2017) |>
  arrange(meso, sem_ini) |>
  group_by(meso) |> mutate(wave = row_number()) |> ungroup() |>
  mutate(mesoyr = interaction(meso, ano, drop = TRUE))

m <- suppressWarnings(glm.nb(conf ~ pr_z + t_z + meso + yr + offset(log(pop)), data = dt))

# IRR (CI 95%) a partir de uma matriz de variancia-covariancia
irr_from <- function(coef, se, v) {
  b <- coef[v]; s <- se[v]
  sprintf("%.2f (%.2f-%.2f)", exp(b), exp(b - 1.96*s), exp(b + 1.96*s))
}
report <- function(label, vc) {
  se <- sqrt(diag(vc)); co <- coef(m)
  cat(sprintf("  %-34s precip: %-18s | temp: %-18s\n",
              label, irr_from(co, se, "pr_z"), irr_from(co, se, "t_z")))
  c(precip = irr_from(co, se, "pr_z"), temp = irr_from(co, se, "t_z"))
}

cat("=====================================================================\n")
cat(" IC robustos a autocorrelacao temporal (IRR por desvio-padrao)\n")
cat("=====================================================================\n")
cat(sprintf("n = %d regiao-semanas | %d mesorregioes | %d mesorregiao-ano\n",
            nrow(dt), nlevels(dt$meso), nlevels(dt$mesoyr)))
r_naive <- report("(1) Naive (iid, atual)",            vcov(m))
r_meso  <- report("(2) Robusto p/ mesorregiao (12)",   vcovCL(m, cluster = dt$meso,   type = "HC0"))
r_myr   <- report("(3) Robusto p/ mesorreg.-ano (180)", vcovCL(m, cluster = dt$mesoyr, type = "HC0"))

# (4) GEE Poisson com AR(1) dentro da mesorregiao (modela diretamente a autocorrelacao)
g <- geeglm(conf ~ pr_z + t_z + meso + yr, family = poisson("log"),
            offset = log(pop), id = meso, waves = wave, corstr = "ar1", data = dt)
sg <- summary(g)$coefficients
gee_irr <- function(v){ b<-sg[v,"Estimate"]; s<-sg[v,"Std.err"]
  sprintf("%.2f (%.2f-%.2f)%s", exp(b), exp(b-1.96*s), exp(b+1.96*s), st(sg[v,"Pr(>|W|)"])) }
cat(sprintf("  %-34s precip: %-18s | temp: %-18s\n",
            "(4) GEE Poisson AR(1) por mesorr.", gee_irr("pr_z"), gee_irr("t_z")))
cat(sprintf("      alpha (AR1) estimado: %.2f\n", summary(g)$corr["alpha","Estimate"]))

saveRDS(list(naive = r_naive, cl_meso = r_meso, cl_mesoano = r_myr,
             gee = c(precip = gee_irr("pr_z"), temp = gee_irr("t_z")),
             ar1_alpha = unname(summary(g)$corr["alpha","Estimate"])),
        "Bancos_rds/ic_robustos.rds")
cat("\nIC_OK\n")
