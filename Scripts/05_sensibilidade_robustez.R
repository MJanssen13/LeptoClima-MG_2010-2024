# =============================================================================
# 05 - SENSIBILIDADE E ROBUSTEZ
#   Sensibilidade (regionalizacao e defasagem), analises complementares (BN
#   sazonal e erro espacial multivariavel), confundimento por confirmacao,
#   validacao do alerta, IC robustos, FDR e figuras de robustez.
#   Reune os antigos scripts 18, 19_analises_complementares, 20, 21, 22, 23 e 24.
# =============================================================================

# -----------------------------------------------------------------------------
# [bloco: 18_sensibilidade.R]
# -----------------------------------------------------------------------------
# =============================================================================
# ANALISES DE SENSIBILIDADE (compoem o artigo; nao alteram o texto principal)
#   A) Regionalizacao: mesorregiao IBGE (atual) x MACRORREGIAO DE SAUDE (PDR/SES-MG)
#   B) Defasagem temporal: 2 SE (atual) x 0 SE (contemporanea)
# Saidas (console + rds): comparacao de incidencia por regiao, Kendall e IRR (BN).
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(lubridate); library(MASS); library(readxl)})
# Executar a partir da raiz do projeto (caminhos relativos, como os demais scripts).
st <- function(p) ifelse(is.na(p),"", ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns"))))
ktau <- function(x,y){ ok<-is.finite(x)&is.finite(y); if(sum(ok)<10) return(c(NA,NA))
  r<-suppressWarnings(cor.test(x[ok],y[ok],method="kendall")); c(unname(r$estimate), r$p.value) }
irr <- function(m,v){ s<-summary(m)$coefficients[v,]; sprintf("%.2f (%.2f-%.2f)%s",
  exp(s[1]), exp(s[1]-1.96*s[2]), exp(s[1]+1.96*s[2]), st(s[4])) }

# ---- Lookup municipio -> macrorregiao de saude (PDR/SES-MG, versao 2026, oficial) ----
#   Planilha de Regionalizacao da SES-MG: col 1 = Codigo IBGE (7 dig); col 2 = cod DATASUS (6 dig);
#   col 9 = Macrorregiao de Saude. Cabecalho na linha 3 (skip=3 -> dados a partir da linha 4).
fx <- "Dados_brutos/dados_pop/Planilha-de-Regionalizacao_SES-MG_2026.xlsx"
x  <- suppressMessages(read_excel(fx, skip = 3, col_names = FALSE))
macro_lk <- data.frame(cod  = suppressWarnings(as.integer(x[[1]])),
                       cod6 = suppressWarnings(as.integer(x[[2]])),
                       macro = trimws(as.character(x[[9]])))
macro_lk <- macro_lk[!is.na(macro_lk$cod) & !is.na(macro_lk$macro) & macro_lk$macro != "", ]
saveRDS(macro_lk, "Bancos_rds/lookup_macrorregiao_saude.rds")
cat("== Macrorregioes de saude (PDR/SES-MG 2026):", length(unique(macro_lk$macro)),
    "| municipios:", nrow(macro_lk), "==\n")
print(sort(unique(macro_lk$macro)))

# ---- Reconstruir painel semanal por MACRORREGIAO DE SAUDE ----
clima <- readRDS("Bancos_rds/clima_semanal_municipal.rds")
pop   <- readRDS("Bancos_rds/pop_municipal_2010_2024.rds")
base  <- readRDS("Bancos_rds/base_MG_2010_2024.rds")

casos <- base |> filter(confirmado %in% TRUE) |>
  mutate(cod6 = as.integer(cod6), sem_ini = as.Date(sem_ini)) |>
  count(cod6, sem_ini, name = "conf") |>
  left_join(macro_lk[c("cod6","macro")], by = "cod6") |> filter(!is.na(macro)) |>
  group_by(macro, sem_ini) |> summarise(conf = sum(conf), .groups = "drop")

clima2 <- clima |> left_join(macro_lk[c("cod","macro")], by = "cod") |>
  left_join(pop, by = c("cod","ano")) |> filter(!is.na(macro), !is.na(pop)) |>
  group_by(macro, sem_ini) |> summarise(pr = weighted.mean(pr, pop), tmean = weighted.mean(tmean, pop), .groups = "drop")

popm <- pop |> left_join(macro_lk[c("cod","macro")], by = "cod") |> filter(!is.na(macro)) |>
  group_by(macro, ano) |> summarise(pop = sum(pop), .groups = "drop")

panel <- clima2 |> mutate(ano = year(sem_ini)) |>
  left_join(popm, by = c("macro","ano")) |>
  left_join(casos, by = c("macro","sem_ini")) |>
  mutate(conf = coalesce(conf, 0L)) |> filter(!is.na(pop)) |>
  arrange(macro, sem_ini) |>
  group_by(macro) |>
  mutate(inc_conf = conf/pop*1e5, pr_l2 = dplyr::lag(pr,2), t_l2 = dplyr::lag(tmean,2)) |> ungroup()

# ---- SENSIBILIDADE A: incidencia acumulada por macrorregiao de saude ----
incA <- panel |> group_by(macro) |>
  summarise(casos = sum(conf), pop = mean(pop), inc = casos/pop*1e5, .groups="drop") |>
  arrange(desc(inc))
cat("\n== [A] Incidencia acumulada por MACRORREGIAO DE SAUDE ==\n"); print(as.data.frame(round_df <- incA |> mutate(inc=round(inc,1))))

# Kendall por macro (lag 2, semanal)
cat("\n== [A] Kendall (inc x precip, lag 2 SE) por macrorregiao ==\n")
kA <- panel |> group_by(macro) |> group_modify(function(d,...){ a<-ktau(d$inc_conf,d$pr_l2); tibble(tau=a[1],p=a[2]) }) |>
  ungroup() |> arrange(desc(tau)) |> mutate(tau=sprintf("%.2f%s",tau,st(p)))
print(as.data.frame(kA[c("macro","tau")]))

# ---- SENSIBILIDADE B: defasagem 2 SE x 0 SE (painel por mesorregiao = atual) ----
dt <- readRDS("Bancos_rds/dados_temporais.rds")
# Estadual semanal
est <- dt |> group_by(sem_ini) |>
  summarise(pr=weighted.mean(pr,pop), conf=sum(conf), pop=sum(pop), .groups="drop") |>
  arrange(sem_ini) |> mutate(inc=conf/pop*1e5, pr_l2=dplyr::lag(pr,2),
                             inc4=as.numeric(stats::filter(inc,rep(1,4),sides=1)),
                             pr4=as.numeric(stats::filter(pr,rep(1,4),sides=1)),
                             pr4_l2=dplyr::lag(pr4,2))
cat("\n== [B] Kendall ESTADUAL incidencia x precipitacao ==\n")
a0<-ktau(est$inc,est$pr); a2<-ktau(est$inc,est$pr_l2)
b0<-ktau(est$inc4,est$pr4); b2<-ktau(est$inc4,est$pr4_l2)
cat(sprintf("  semanal  lag0: tau=%.2f%s | lag2: tau=%.2f%s\n", a0[1],st(a0[2]), a2[1],st(a2[2])))
cat(sprintf("  4 SE     lag0: tau=%.2f%s | lag2: tau=%.2f%s\n", b0[1],st(b0[2]), b2[1],st(b2[2])))

# ---- BN: IRR por DP, comparando regionalizacao x defasagem ----
nb_irr <- function(d, lag) {
  prv <- if (lag==2) "pr_l2" else "pr"; tv <- if (lag==2) "t_l2" else "tmean"
  d <- d |> filter(is.finite(d[[prv]]), is.finite(d[[tv]]), pop>0) |>
    mutate(pr_z=as.numeric(scale(.data[[prv]])), t_z=as.numeric(scale(.data[[tv]])), yr=ano-2017)
  m <- suppressWarnings(glm.nb(conf~pr_z+t_z+reg+yr+offset(log(pop)), data=d))
  c(prec=irr(m,"pr_z"), temp=irr(m,"t_z"))
}
dt$reg <- factor(dt$name_meso); panel$reg <- factor(panel$macro)
cat("\n== IRR (BN, por DP) — regionalizacao x defasagem ==\n")
cat("  [meso FE,  lag2] ", nb_irr(dt, 2)["prec"],    " | temp ", nb_irr(dt, 2)["temp"], "\n")
cat("  [meso FE,  lag0] ", nb_irr(dt, 0)["prec"],    " | temp ", nb_irr(dt, 0)["temp"], "\n")
cat("  [macroSaude FE, lag2] ", nb_irr(panel, 2)["prec"], " | temp ", nb_irr(panel, 2)["temp"], "\n")
cat("  [macroSaude FE, lag0] ", nb_irr(panel, 0)["prec"], " | temp ", nb_irr(panel, 0)["temp"], "\n")
# ---- Viés de averiguacao: taxa de confirmacao (confirmados/notificados) por regiao ----
conf_meso <- base |> group_by(name_meso) |>
  summarise(noti = n(), conf = sum(confirmado %in% TRUE), .groups = "drop") |>
  mutate(pconf = round(conf/noti*100, 1)) |> arrange(desc(pconf))
cat("\n== Taxa de confirmacao por mesorregiao (viés de averiguacao) ==\n"); print(as.data.frame(conf_meso))
t1 <- tryCatch(read.csv("Bancos_rds/tabela1.csv", fileEncoding = "UTF-8"), error = function(e) NULL)
if (!is.null(t1)) {
  cm <- left_join(conf_meso, t1[c("Mesorregiao","Incidencia_100mil")], by = c("name_meso"="Mesorregiao"))
  cat("Amplitude %confirmados:", min(conf_meso$pconf), "a", max(conf_meso$pconf),
      "| Spearman(%conf, incidencia):", round(cor(cm$pconf, cm$Incidencia_100mil, method="spearman"), 2), "\n")
}

saveRDS(list(incA=incA, kA=kA, est=est, conf_meso=conf_meso), "Bancos_rds/sensibilidade.rds")
cat("\nSENS_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 19_analises_complementares.R]
# -----------------------------------------------------------------------------
# =============================================================================
# ANALISES COMPLEMENTARES
#   (A) Binomial Negativa temporal COM termo de sazonalidade (mes): testa se o
#       efeito da precipitacao sobrevive ao ajuste pela estacao do ano.
#   (B) Regressao espacial multivariavel (modelo de erro espacial) da incidencia
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

# -----------------------------------------------------------------------------
# [bloco: 20_confirmacao_covariavel.R]
# -----------------------------------------------------------------------------
# =============================================================================
# CONFUNDIMENTO POR ACESSO AO DIAGNOSTICO
#   A taxa de confirmacao (confirmados/notificados) varia de ~9% no Norte a ~33%
#   no Sul/Sudoeste e correlaciona-se 0,77 com a incidencia confirmada, podendo
#   confundir o gradiente espacial da incidencia. Aqui:
#   (a) modela-se a taxa de confirmacao como COVARIAVEL no modelo de erro espacial;
#   (b) verifica-se quanto do sinal de Moran/LISA sobrevive ao ajuste;
#   (c) analisam-se os casos NOTIFICADOS como desfecho co-primario (Moran/SEM/LISA).
# Banco: dados_espaciais_municipal ; geometria via sf.
# Saidas: console + Bancos_rds/confirmacao_covariavel.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(sf); library(spdep); library(spatialreg)})
st <- function(p) ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns")))
fmt <- function(cc,v){ s<-cc[v,]; sprintf("%+.3f (EP %.3f)%s", s[1], s[2], st(s[4])) }

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds"); sf_use_s2(FALSE)
nb <- poly2nb(muni, queen=TRUE); W <- nb2listw(nb, style="W", zero.policy=TRUE)

# ---- Taxa de confirmacao (proxy de acesso ao diagnostico) ----
# Mesorregional: robusta (a municipal e ruidosa onde ha poucas notificacoes) e
# alinhada ao gradiente regional de acesso ao diagnostico. Cada municipio
# recebe a taxa da sua mesorregiao; reporta-se tambem a versao municipal.
pconf_meso <- muni |> st_drop_geometry() |> group_by(name_meso) |>
  summarise(pconf_meso = sum(conf)/sum(noti)*100, .groups="drop")
muni <- muni |> left_join(pconf_meso, by="name_meso") |>
  mutate(pconf_muni = ifelse(noti>0, conf/noti*100, pconf_meso))  # imputa mesorregional onde noti=0

z <- function(x) as.numeric(scale(x))
muni <- muni |> mutate(precip_z=z(precip), temp_z=z(temp), ivs_z=z(ivs),
                       dens_z=z(logdens), pconf_z=z(pconf_meso), pconfm_z=z(pconf_muni))
muni$linc  <- log(muni$inc_conf + 1)   # desfecho primario   (confirmados, escala log)
muni$lincn <- log(muni$inc_noti + 1)   # desfecho co-primario (notificados,  escala log)

mglob <- function(x) { mt <- moran.test(x, W, zero.policy=TRUE);
  sprintf("%.2f (p=%.3g)", mt$estimate[1], mt$p.value) }

cat("===========================================================\n")
cat(" Confundimento por acesso ao diagnostico\n")
cat("===========================================================\n")
cat("Taxa de confirmacao por mesorregiao (%):\n")
print(as.data.frame(pconf_meso |> arrange(desc(pconf_meso)) |> mutate(pconf_meso=round(pconf_meso,1))))
cat(sprintf("\nSpearman(pconf_meso, inc_conf) municipal = %.2f\n",
            cor(muni$pconf_meso, muni$inc_conf, method="spearman")))

# ---- (1) Moran GLOBAL: confirmados x notificados (co-primarios) ----
cat("\n-- Moran global da incidencia --\n")
cat("  confirmados (inc_conf):", mglob(muni$inc_conf), "\n")
cat("  notificados (inc_noti):", mglob(muni$inc_noti), "  [desfecho co-primario]\n")

# ---- (2) Quanto do Moran sobrevive ao ajuste por confirmacao? ----
# residuos da incidencia (log) explicada SO pela taxa de confirmacao:
r_adj  <- residuals(lm(linc  ~ pconf_z, data=muni))
rn_adj <- residuals(lm(lincn ~ pconf_z, data=muni))
cat("\n-- Moran dos RESIDUOS apos remover a taxa de confirmacao --\n")
cat("  confirmados | bruto:", mglob(muni$linc),  "-> ajustado:", mglob(r_adj), "\n")
cat("  notificados | bruto:", mglob(muni$lincn), "-> ajustado:", mglob(rn_adj), "\n")

# ---- (3) Modelo de erro espacial: efeito da chuva sobrevive a confirmacao? ----
sem0 <- errorsarlm(linc ~ precip_z+temp_z+ivs_z+dens_z,          data=muni, listw=W, zero.policy=TRUE)
sem1 <- errorsarlm(linc ~ precip_z+temp_z+ivs_z+dens_z+pconf_z,  data=muni, listw=W, zero.policy=TRUE)
semN <- errorsarlm(lincn~ precip_z+temp_z+ivs_z+dens_z+pconf_z,  data=muni, listw=W, zero.policy=TRUE)
c0<-summary(sem0)$Coef; c1<-summary(sem1)$Coef; cN<-summary(semN)$Coef
cat("\n-- Modelo de erro espacial (incidencia log) --\n")
cat("  [confirmados, SEM confirmacao]  precip:", fmt(c0,"precip_z"), "| ivs:", fmt(c0,"ivs_z"), "\n")
cat("  [confirmados, COM confirmacao]  precip:", fmt(c1,"precip_z"), "| ivs:", fmt(c1,"ivs_z"),
    "| pconf:", fmt(c1,"pconf_z"), "\n")
cat("  [notificados, COM confirmacao]  precip:", fmt(cN,"precip_z"), "| ivs:", fmt(cN,"ivs_z"),
    "| pconf:", fmt(cN,"pconf_z"), "\n")

# ---- (4) LISA: sobrevivencia do aglomerado de alto risco ----
aa_conf <- which(muni$lisa_conf=="Alto-Alto"); aa_noti <- which(muni$lisa_noti=="Alto-Alto")
cat("\n-- Aglomerado Alto-Alto (LISA) --\n")
cat("  confirmados:", length(aa_conf), "municipios | notificados:", length(aa_noti),
    "| em comum:", length(intersect(aa_conf, aa_noti)), "\n")
cat("  baixo-risco (Baixo-Baixo) notificados:", sum(muni$lisa_noti=="Baixo-Baixo"), "\n")

saveRDS(list(pconf_meso=pconf_meso,
             moran=c(conf=mglob(muni$inc_conf), noti=mglob(muni$inc_noti),
                     conf_adj=mglob(r_adj), noti_adj=mglob(rn_adj)),
             sem0=c0, sem1=c1, semN=cN,
             lisa=c(aa_conf=length(aa_conf), aa_noti=length(aa_noti),
                    comum=length(intersect(aa_conf,aa_noti)))),
        "Bancos_rds/confirmacao_covariavel.rds")
cat("\nCONF_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 21_validacao_alerta.R]
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# [bloco: 22_ic_robustos.R]
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# [bloco: 23_fdr.R]
# -----------------------------------------------------------------------------
# =============================================================================
# CONTROLE DE MULTIPLAS COMPARACOES (FALSE DISCOVERY RATE, Benjamini-Hochberg)
#   Limita a proporcao esperada de falsos positivos entre os testes, aplicado a:
#   (A) 12 correlacoes de Kendall (incidencia x precipitacao, lag 2 SE, 4 SE) por mesorregiao;
#   (B) 11 testes de Moran bivariado (incidencia x covariavel);
#   (C) LISA (Moran local) — n. de municipios significativos antes/depois do FDR.
# Saidas: console + Bancos_rds/fdr.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(sf); library(spdep)})
st <- function(p) ifelse(is.na(p),"", ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns"))))

# ---------- (A) Kendall por mesorregiao ----------
panel <- readRDS("Bancos_rds/dados_temporais.rds")
ktau <- function(x,y){ ok<-is.finite(x)&is.finite(y); x<-x[ok]; y<-y[ok]
  if(length(x)<10) return(c(NA,NA)); r<-suppressWarnings(cor.test(x,y,method="kendall")); c(unname(r$estimate),r$p.value) }
kd <- panel |> group_by(name_meso) |> group_modify(function(d,...){
  a<-ktau(d$r_inc_conf,d$r_pr_l2); tibble(tau=a[1], p=a[2]) }) |> ungroup()
kd$p_fdr <- p.adjust(kd$p, "BH")
kd <- kd |> arrange(desc(tau)) |> mutate(tau=round(tau,2))
cat("== (A) Kendall (inc x precip, 4 SE) por mesorregiao — p bruto vs FDR ==\n")
print(as.data.frame(kd |> transmute(name_meso, tau,
      `p`=sprintf("%.3g%s",p,st(p)), `p_FDR`=sprintf("%.3g%s",p_fdr,st(p_fdr)))))
cat(sprintf("  significativos (p<0,05): bruto %d/12 | FDR %d/12\n",
            sum(kd$p<0.05), sum(kd$p_fdr<0.05)))

# ---------- (B) Moran bivariado ----------
muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds"); sf_use_s2(FALSE)
nb <- poly2nb(muni, queen=TRUE); W <- nb2listw(nb, style="W", zero.policy=TRUE)
bim <- function(x,y,nsim=999){ zx<-as.numeric(scale(x)); zy<-as.numeric(scale(y))
  Ixy<-function(zy){ly<-lag.listw(W,zy,zero.policy=TRUE); sum(zx*ly)/sum(zx^2)}; I<-Ixy(zy)
  perm<-replicate(nsim,Ixy(sample(zy))); c(I,(sum(abs(perm)>=abs(I))+1)/(nsim+1)) }
set.seed(1)
labs <- c(precip="Precipitação", temp="Temperatura", rh="Umidade relativa", rs="Radiação solar",
          u2="Vento", eto="Evapotranspiração", logdens="Densidade (log)", ivs="IVS",
          esgoto="Esgoto", agua="Água", lixo="Lixo")
mb <- t(sapply(names(labs), function(v) bim(muni$inc_conf, muni[[v]])))
mb <- data.frame(Variavel=unname(labs), I=round(mb[,1],2), p=mb[,2])
mb$p_fdr <- p.adjust(mb$p, "BH")
cat("\n== (B) Moran bivariado (inc confirmados x covariavel) — p bruto vs FDR ==\n")
print(data.frame(Variavel=mb$Variavel, I=mb$I,
      p=sprintf("%.3g%s",mb$p,st(mb$p)), p_FDR=sprintf("%.3g%s",mb$p_fdr,st(mb$p_fdr))))
cat(sprintf("  significativos: bruto %d/11 | FDR %d/11\n", sum(mb$p<0.05), sum(mb$p_fdr<0.05)))

# ---------- (C) LISA (Moran local) ----------
set.seed(1)
zc <- as.numeric(scale(muni$inc_conf))
lm_ <- localmoran(zc, W, zero.policy=TRUE)
p_loc <- lm_[,"Pr(z != E(Ii))"]
p_loc_fdr <- p.adjust(p_loc, "BH")
cat("\n== (C) LISA (Moran local da incidencia confirmada) ==\n")
cat(sprintf("  municipios significativos (p<0,05): bruto %d | FDR %d (de %d)\n",
            sum(p_loc<0.05, na.rm=TRUE), sum(p_loc_fdr<0.05, na.rm=TRUE), nrow(muni)))

saveRDS(list(kendall=kd, moran_biv=mb,
             lisa=c(bruto=sum(p_loc<0.05,na.rm=TRUE), fdr=sum(p_loc_fdr<0.05,na.rm=TRUE))),
        "Bancos_rds/fdr.rds")
cat("\nFDR_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 24_figuras_robustez.R]
# -----------------------------------------------------------------------------
# =============================================================================
# FIGURAS DAS ANALISES COMPLEMENTARES E DE ROBUSTEZ
#   Desenha uma figura para cada analise complementar (scripts 20-23):
#   fig_irr_robustos      (script 22) - IC da IRR robustos a autocorrelacao temporal
#   fig_validacao_alerta  (script 21) - validacao preditiva do alerta (skill x lead)
#   fig_confirmacao       (script 20) - efeito da chuva apos ajuste pela taxa de confirmacao
#   fig_fdr               (script 23) - significancia antes/depois do FDR (Benjamini-Hochberg)
# Entradas: Bancos_rds/{ic_robustos,validacao_alerta,confirmacao_covariavel,fdr}.rds
# Saidas:   Resultados/Figuras/*.png
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)
                  library(forcats); library(patchwork)})
dir.create("Resultados/Figuras", recursive = TRUE, showWarnings = FALSE)

AZUL <- "#4575b4"; VERM <- "#d73027"; CINZA <- "#9e9e9e"; VERDE <- "#1a9850"
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text    = element_text(face = "bold"),
        legend.position = "bottom")

# =============================================================================
# FIGURA 1 - IC da IRR robustos a autocorrelacao temporal (script 22)
# =============================================================================
ic <- readRDS("Bancos_rds/ic_robustos.rds")
# transforma "1.36 (1.27-1.46)***" em (irr, lo, hi)
parse_irr <- function(s) {
  m <- regmatches(s, regexec("([0-9.]+) \\(([0-9.]+)-([0-9.]+)\\)", s))[[1]]
  as.numeric(m[2:4])
}
metodos <- c(naive = "Naive (iid, atual)",
             cl_meso = "Robusto: mesorregião (12)",
             cl_mesoano = "Robusto: mesorreg.-ano (180)",
             gee = "GEE Poisson AR(1)")
d1 <- lapply(names(metodos), function(k) {
  pr <- parse_irr(ic[[k]]["precip"]); te <- parse_irr(ic[[k]]["temp"])
  tibble(metodo = metodos[k],
         var = c("Precipitação", "Temperatura"),
         irr = c(pr[1], te[1]), lo = c(pr[2], te[2]), hi = c(pr[3], te[3]))
}) |> bind_rows() |>
  mutate(metodo = factor(metodo, levels = rev(unname(metodos))),
         var = factor(var, levels = c("Precipitação", "Temperatura")))

g1 <- ggplot(d1, aes(irr, metodo, color = var)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), linewidth = 0.7, fatten = 3) +
  geom_text(aes(label = sprintf("%.2f", irr)), vjust = -0.9, size = 3, show.legend = FALSE) +
  facet_wrap(~ var, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("Precipitação" = AZUL, "Temperatura" = VERM), guide = "none") +
  labs(title = "Razão de taxas (IRR) por desvio-padrão, com IC95%\nrobustos à autocorrelação temporal",
       x = "IRR (IC95%)", y = NULL) +
  tema
ggsave("Resultados/Figuras/fig_irr_robustos.png", g1, width = 8.5, height = 5.5, dpi = 1200)
cat("Salvo: fig_irr_robustos.png\n")

# =============================================================================
# FIGURA 2 - validacao preditiva do alerta (script 21)
# =============================================================================
va <- readRDS("Bancos_rds/validacao_alerta.rds")

# Rotulos: o manuscrito escreve "escore de habilidade de Heidke", nunca a sigla
# inglesa HSS. O escore e ADIMENSIONAL (-1 a 1) e nao pode ser exibido em %;
# as demais metricas sao proporcoes. Por isso o eixo y traz duas leituras da
# MESMA posicao: proporcao (%) a esquerda e escore (decimal) a direita.
HEIDKE <- "Escore de Heidke"
pct  <- scales::label_percent(accuracy = 1, decimal.mark = ",")   # proporcoes
esc0 <- scales::label_number(accuracy = 0.01, decimal.mark = ",") # escore (adimensional)
eixo_heidke <- sec_axis(~ ., name = "Escore de Heidke (adimensional)",
                        breaks = seq(0, 1, 0.25), labels = esc0)

# (A) varredura da defasagem do gatilho
L <- matrix(as.numeric(unlist(va$leads)), ncol = 4)
colnames(L) <- c("lead", "sens", "far", "hss")
dA <- as.data.frame(L) |>
  pivot_longer(c(sens, far, hss), names_to = "metrica", values_to = "valor") |>
  mutate(metrica = recode(metrica,
                          sens = "Sensibilidade", far = "Taxa de falso-alarme", hss = HEIDKE),
         metrica = factor(metrica, levels = c("Taxa de falso-alarme", "Sensibilidade", HEIDKE)))
gA <- ggplot(dA, aes(lead, valor, color = metrica)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_color_manual(values = setNames(c(VERM, AZUL, VERDE),
                       c("Taxa de falso-alarme", "Sensibilidade", HEIDKE))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = pct,
                     sec.axis = eixo_heidke) +
  scale_x_continuous(breaks = 0:4) +
  labs(title = "(A) Desempenho por defasagem do gatilho",
       x = "Defasagem do gatilho (semanas epidemiológicas)",
       y = "Proporção (%)", color = NULL) +
  tema

# (B) metricas dos gatilhos p80 vs p90
mtab <- function(x, rotulo) tibble(
  gatilho = rotulo,
  metrica = c("Sensibilidade", "Especificidade", "VPP", "Falso-alarme", HEIDKE),
  valor   = c(x$sens, x$spec, x$ppv, x$far, x$hss))
dB <- bind_rows(mtab(va$p80, "Chuva 4 SE > p80"), mtab(va$p90, "Chuva 4 SE > p90")) |>
  mutate(metrica = factor(metrica,
           levels = c("Sensibilidade", "Especificidade", "VPP", "Falso-alarme", HEIDKE)),
         rotulo = ifelse(metrica == HEIDKE, esc0(valor), pct(valor)))  # % so p/ proporcoes
gB <- ggplot(dB, aes(metrica, valor, fill = gatilho)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = rotulo),
            position = position_dodge(0.75), vjust = -0.4, size = 2.7) +
  scale_fill_manual(values = c("Chuva 4 SE > p80" = AZUL, "Chuva 4 SE > p90" = CINZA)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = pct,
                     sec.axis = eixo_heidke) +
  labs(title = "(B) Métricas no teste (2020–2024), gatilho a 2 semanas epidemiológicas (SE)",
       x = "Métrica (VPP = valor preditivo positivo)", y = "Proporção (%)", fill = NULL) +
  tema + theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Sem titulo embutido: a legenda da figura vai no texto do material suplementar.
g2 <- gA / gB
ggsave("Resultados/Figuras/fig_validacao_alerta.png", g2, width = 9, height = 8.5, dpi = 1200)
cat("Salvo: fig_validacao_alerta.png\n")

# =============================================================================
# FIGURA 3 - o sinal climatico sobrevive ao ajuste por confirmacao (script 20)
# =============================================================================
cc <- readRDS("Bancos_rds/confirmacao_covariavel.rds")
covs <- c(precip_z = "Precipitação", temp_z = "Temperatura", ivs_z = "IVS",
          dens_z = "Densidade (log)", pconf_z = "Taxa de confirmação")
extrai <- function(mat, modelo) {
  v <- intersect(rownames(mat), names(covs))
  tibble(modelo = modelo, termo = covs[v],
         beta = mat[v, 1], se = mat[v, 2]) |>
    mutate(lo = beta - 1.96 * se, hi = beta + 1.96 * se)
}
d3 <- bind_rows(
  extrai(cc$sem0, "Confirmados — sem ajuste"),
  extrai(cc$sem1, "Confirmados — ajustado p/ confirmação"),
  extrai(cc$semN, "Notificados — ajustado p/ confirmação")) |>
  mutate(modelo = factor(modelo, levels = c("Confirmados — sem ajuste",
            "Confirmados — ajustado p/ confirmação", "Notificados — ajustado p/ confirmação")),
         termo = factor(termo, levels = rev(unname(covs))))

g3 <- ggplot(d3, aes(beta, termo, color = modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(0.6), linewidth = 0.6, fatten = 2.5) +
  scale_color_manual(values = c("Confirmados — sem ajuste" = CINZA,
                                "Confirmados — ajustado p/ confirmação" = AZUL,
                                "Notificados — ajustado p/ confirmação" = VERDE)) +
  labs(title = "Modelo de erro espacial: o efeito da chuva\nsobrevive ao ajuste pelo acesso ao diagnóstico",
       x = "β (incidência em escala log, por desvio-padrão)", y = NULL, color = NULL) +
  guides(color = guide_legend(nrow = 3)) +
  tema
ggsave("Resultados/Figuras/fig_confirmacao.png", g3, width = 9, height = 5.5, dpi = 1200)
cat("Salvo: fig_confirmacao.png\n")

# =============================================================================
# FIGURA 4 - controle de multiplas comparacoes (FDR / Benjamini-Hochberg) (script 23)
# =============================================================================
fd <- readRDS("Bancos_rds/fdr.rds")

# (A) Moran bivariado: I por covariavel, significancia apos FDR
dM <- fd$moran_biv |>
  mutate(sig = ifelse(p_fdr < 0.05, "Significativo (FDR)", "Não significativo (FDR)"),
         Variavel = fct_reorder(Variavel, I))
gMA <- ggplot(dM, aes(I, Variavel, color = sig)) +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_segment(aes(x = 0, xend = I, yend = Variavel), linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Significativo (FDR)" = AZUL, "Não significativo (FDR)" = CINZA)) +
  labs(title = "(A) Moran bivariado (incidência × covariável)",
       x = "I de Moran bivariado", y = NULL, color = NULL) +
  tema

# (B) Kendall por mesorregiao: tau, significancia apos FDR
dK <- fd$kendall |>
  mutate(sig = ifelse(p_fdr < 0.05, "Significativo (FDR)", "Não significativo (FDR)"),
         name_meso = fct_reorder(name_meso, tau))
gKB <- ggplot(dK, aes(tau, name_meso, color = sig)) +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_segment(aes(x = 0, xend = tau, yend = name_meso), linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Significativo (FDR)" = AZUL, "Não significativo (FDR)" = CINZA)) +
  labs(title = "(B) Kendall por mesorregião (incidência × precipitação, 4 SE)",
       x = "τ de Kendall", y = NULL, color = NULL) +
  tema

g4 <- (gMA / gKB) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Controle de múltiplas comparações (Benjamini-Hochberg, FDR)",
    theme = theme(plot.title = element_text(face = "bold", size = 13),
                  legend.position = "bottom")) &
  theme(legend.position = "bottom")
ggsave("Resultados/Figuras/fig_fdr.png", g4, width = 9, height = 9, dpi = 1200)
cat("Salvo: fig_fdr.png\n")

cat("\nFIGURAS_OK — 4 figuras geradas em Resultados/Figuras/\n")
