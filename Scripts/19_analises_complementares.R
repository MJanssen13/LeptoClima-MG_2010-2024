# =============================================================================
# ANALISES COMPLEMENTARES (parecer, Revisao Maior)
#   R3) Binomial Negativa temporal COM termo de sazonalidade (mes) — testa se o
#       efeito da precipitacao sobrevive ao ajuste pela estacao do ano.
#   R4) Regressao espacial multivariavel (modelo de erro espacial) da incidencia
#       municipal ~ precipitacao + temperatura + IVS + densidade (ajuste conjunto
#       + autocorrelacao espacial), complementando o Moran bivariado univariado.
# Saidas: console.
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("spatialreg")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(dplyr); library(lubridate); library(MASS); library(sf); library(spdep); library(spatialreg)})
st <- function(p) ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns")))

# ---------- R3: sazonalidade no modelo temporal ----------
dt <- readRDS("Bancos_rds/dados_temporais.rds")
d <- dt |> filter(is.finite(pr_l2), is.finite(t_l2), pop > 0) |>
  mutate(pr_z = as.numeric(scale(pr_l2)), t_z = as.numeric(scale(t_l2)),
         meso = factor(name_meso), yr = ano - 2017, mes = factor(month(sem_ini)))
irr <- function(m,v){ s<-summary(m)$coefficients[v,]; sprintf("%.2f (%.2f-%.2f)%s",
  exp(s[1]), exp(s[1]-1.96*s[2]), exp(s[1]+1.96*s[2]), st(s[4])) }
m_base <- suppressWarnings(glm.nb(conf ~ pr_z+t_z+meso+yr+offset(log(pop)), data=d))
m_seas <- suppressWarnings(glm.nb(conf ~ pr_z+t_z+meso+yr+mes+offset(log(pop)), data=d))
cat("== R3) IRR por desvio-padrao — efeito da sazonalidade ==\n")
cat("  Sem termo sazonal : precip", irr(m_base,"pr_z"), "| temp", irr(m_base,"t_z"), "\n")
cat("  COM termo sazonal : precip", irr(m_seas,"pr_z"), "| temp", irr(m_seas,"t_z"), "\n")

# ---------- R4: regressao espacial multivariavel ----------
muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds"); sf_use_s2(FALSE)
nb <- poly2nb(muni, queen=TRUE); W <- nb2listw(nb, style="W", zero.policy=TRUE)
muni <- muni |> mutate(precip_z=as.numeric(scale(precip)), temp_z=as.numeric(scale(temp)),
                       ivs_z=as.numeric(scale(ivs)), dens_z=as.numeric(scale(logdens)))
muni$linc <- log(muni$inc_conf + 1)                          # escala log (apropriada p/ taxas)
f_log <- linc     ~ precip_z + temp_z + ivs_z + dens_z
f_raw <- inc_conf ~ precip_z + temp_z + ivs_z + dens_z
mt   <- lm.morantest(lm(f_log, data=muni), W, zero.policy=TRUE)  # autocorrelacao residual -> justifica modelo espacial
sem  <- errorsarlm(f_log, data=muni, listw=W, zero.policy=TRUE)  # PRIMARIO (incidencia em log)
semr <- errorsarlm(f_raw, data=muni, listw=W, zero.policy=TRUE)  # sensibilidade (incidencia bruta)
cf <- summary(sem)$Coef; cfr <- summary(semr)$Coef
fmt <- function(cc,v){ s<-cc[v,]; sprintf("%+.3f (EP %.3f)%s", s[1], s[2], st(s[4])) }
cat("\n== R4) Modelo de erro espacial multivariavel ==\n")
cat("  I de Moran dos residuos (OLS, log):", sprintf("%.2f, p=%.3g", mt$estimate[1], mt$p.value), "(justifica o modelo espacial)\n")
cat("  PRIMARIO (incidencia em log) — coeficiente por DP:\n")
for (v in c("precip_z","temp_z","ivs_z","dens_z")) cat("    ", v, ":", fmt(cf,v), "\n")
cat("  lambda (erro espacial):", sprintf("%.2f", sem$lambda), "\n")
cat("  SENSIBILIDADE (incidencia bruta) — precip:", fmt(cfr,"precip_z"), "| temp:", fmt(cfr,"temp_z"), "\n")
saveRDS(list(irr_base=exp(coef(m_base)["pr_z"]), irr_seas=exp(coef(m_seas)["pr_z"]),
             sem_log=cf, sem_raw=cfr), "Bancos_rds/analises_complementares.rds")
cat("\nCOMPL_OK\n")
