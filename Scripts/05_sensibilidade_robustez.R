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
  # A Tabela 1 grava a incidencia ja formatada no padrao brasileiro ("21,6");
  # converter para numerico antes de correlacionar.
  inc_t1 <- as.numeric(sub(",", ".", cm$Incidencia_100mil, fixed = TRUE))
  cat("Amplitude %confirmados:", min(conf_meso$pconf), "a", max(conf_meso$pconf),
      "| Spearman(%conf, incidencia):", round(cor(cm$pconf, inc_t1, method="spearman"), 2), "\n")
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

# Coeficientes por DP com IC95% e valor-p exato (a impressao acima traz beta e
# erro-padrao). E o MESMO modelo, so muda o que se reporta: o beta do IVS
# ajustado pela taxa de confirmacao e citado no manuscrito com IC95%.
pf3 <- function(p) ifelse(p < 0.001, "p<0,001", paste0("p=", sub("\\.", ",", sprintf("%.3f", p))))
n3  <- function(x) sub("\\.", ",", sprintf("%+.3f", x))
bic <- function(cc, v) { s <- cc[v, ]
  sprintf("beta = %s (IC95%% %s; %s), %s", n3(s[1]), n3(s[1]-1.96*s[2]), n3(s[1]+1.96*s[2]), pf3(s[4])) }
cat("\n-- Coeficientes por desvio-padrao, com IC95% e valor-p exato --\n")
for (mm in list(list(c0, "confirmados, SEM confirmacao"), list(c1, "confirmados, COM confirmacao"),
                list(cN, "notificados, COM confirmacao")))
  for (v in setdiff(rownames(mm[[1]]), "(Intercept)"))
    cat(sprintf("  [%-28s] %-9s %s\n", mm[[2]], v, bic(mm[[1]], v)))

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

# -----------------------------------------------------------------------------
# [bloco: 27_confirmacao_laboratorial.R]
# -----------------------------------------------------------------------------
# =============================================================================
# TAXA DE CONFIRMACAO LABORATORIAL POR MESORREGIAO (CRITERIO do SINAN)
#   O bloco 18 mede a confirmacao total (confirmados/notificados). Aqui separa-se
#   o CRITERIO de confirmacao (1 = laboratorial; 2 = clinico-epidemiologico) para
#   isolar o acesso ao DIAGNOSTICO LABORATORIAL, que e o mecanismo do vies de
#   averiguacao. Os confirmados sem CRITERIO preenchido sao tratados em tres
#   cenarios (A = nao laboratorial, primario; B = limite superior; C = imputacao
#   proporcional, MAR), para mostrar que o gradiente nao depende dessa escolha.
# Entrada: Bancos_rds/base_MG_2010_2024.rds
# Saidas : console + Bancos_rds/confirmacao_laboratorial.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr)})
w0 <- getOption("width"); options(width = 200)   # tabelas largas; largura restaurada ao fim
fmtn <- function(x, d = 1) formatC(unname(x), format = "f", digits = d, decimal.mark = ",", big.mark = ".")
fmti <- function(x) formatC(as.integer(round(unname(x))), format = "d", big.mark = ".", decimal.mark = ",")
pfv  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p))))
pfx  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", pfv(p))))
wilson <- function(x, n, conf = 0.95) {          # IC95% de Wilson (escore)
  if (n == 0) return(c(NA, NA))
  z <- qnorm(1 - (1 - conf)/2); p <- x/n; d <- 1 + z^2/n
  c((p + z^2/(2*n) - z*sqrt(p*(1-p)/n + z^2/(4*n^2))) / d,
    (p + z^2/(2*n) + z*sqrt(p*(1-p)/n + z^2/(4*n^2))) / d)
}

b <- readRDS("Bancos_rds/base_MG_2010_2024.rds")
b <- b[!is.na(b$name_meso), ]
b$conf <- b$confirmado %in% TRUE
b$crit <- ifelse(b$conf, b$CRITERIO, NA)

cat("=============================================================\n")
cat(" TAXA DE CONFIRMACAO LABORATORIAL POR MESORREGIAO\n")
cat("=============================================================\n")
cat("Notificados (com mesorregiao):", nrow(b), "| Confirmados:", sum(b$conf), "\n")
cat("CRITERIO entre os confirmados:\n"); print(table(b$crit, useNA = "ifany"))
sc <- b[b$conf & (is.na(b$CRITERIO) | !(b$CRITERIO %in% c("1", "2"))), ]
cat("\nConfirmados SEM CRITERIO (n =", nrow(sc), "), por mesorregiao e por ano:\n")
print(table(sc$name_meso)); print(table(sc$ANO_ARQUIVO))

# ---- Tabela por mesorregiao -------------------------------------------------
tl <- b %>% group_by(name_meso) %>% summarise(
  N       = n(),                                   # notificados
  C       = sum(conf),                             # confirmados (CLASSI_FIN == 1)
  L       = sum(conf & crit %in% "1"),             # confirmados por criterio laboratorial
  CE      = sum(conf & crit %in% "2"),             # confirmados clinico-epidemiologicos
  SEMCRIT = C - L - CE,                            # confirmados sem CRITERIO
  .groups = "drop") %>%
  mutate(
    tx_conf   = 100 * C / N,                       # taxa de confirmacao total (bloco 18)
    tx_lab    = 100 * L / N,                       # CENARIO A: sem-criterio = NAO laboratorial
    tx_lab_up = 100 * (L + SEMCRIT) / N,           # CENARIO B: sem-criterio = laboratorial
    tx_lab_pr = 100 * (L + SEMCRIT * ifelse(L + CE > 0, L/(L + CE), NA)) / N,  # CENARIO C (MAR)
    pct_lab_entre_conf = 100 * L / (L + CE)) %>%
  arrange(desc(tx_lab))

cat("\n--- TABELA COMPLETA (12 mesorregioes) ---\n")
print(as.data.frame(tl %>% mutate(across(where(is.double), ~round(.x, 2)))))

cat("\n--- Formatado (padrao brasileiro) ---\n")
cat(sprintf("%-34s %8s %7s %7s %10s %10s %10s\n",
            "Mesorregiao", "Notif.", "Conf.", "Lab.", "%conf(N)", "%lab(N)", "%lab(conf)"))
for (i in seq_len(nrow(tl)))
  cat(sprintf("%-34s %8s %7s %7s %9s%% %9s%% %9s%%\n", tl$name_meso[i], fmti(tl$N[i]), fmti(tl$C[i]),
              fmti(tl$L[i]), fmtn(tl$tx_conf[i], 1), fmtn(tl$tx_lab[i], 1), fmtn(tl$pct_lab_entre_conf[i], 1)))
cat(sprintf("%-34s %8s %7s %7s %9s%% %9s%% %9s%%\n", "TOTAL MG", fmti(sum(tl$N)), fmti(sum(tl$C)),
            fmti(sum(tl$L)), fmtn(100*sum(tl$C)/sum(tl$N), 1), fmtn(100*sum(tl$L)/sum(tl$N), 1),
            fmtn(100*sum(tl$L)/sum(tl$L + tl$CE), 1)))

# ---- Gradiente maior/menor --------------------------------------------------
grad <- function(x, rot) cat(sprintf("  %-42s max %s%% (%s) / min %s%% (%s) = %sx\n", rot,
            fmtn(max(x), 1), tl$name_meso[which.max(x)], fmtn(min(x), 1), tl$name_meso[which.min(x)],
            fmtn(max(x)/min(x), 1)))
cat("\n--- RAZAO MAIOR/MENOR ---\n")
grad(tl$tx_conf,   "Taxa de confirmacao total (C/N)")
grad(tl$tx_lab,    "Taxa LABORATORIAL (L/N) [cenario A, primario]")
grad(tl$tx_lab_up, "Taxa LABORATORIAL [cenario B, limite superior]")
grad(tl$tx_lab_pr, "Taxa LABORATORIAL [cenario C, proporcional]")

# ---- Extremos do gradiente --------------------------------------------------
cat("\n--- Extremos do gradiente ---\n")
for (m in c("Norte de Minas", "Sul/Sudoeste de Minas")) {
  r <- tl[tl$name_meso == m, ]
  cat(sprintf("  %-24s: %%lab(N) = %s%%  (%d/%d) | %%conf(N) = %s%%\n",
              m, fmtn(r$tx_lab, 1), r$L, r$N, fmtn(r$tx_conf, 1)))
}
cat(sprintf("  Gradiente laboratorial Sul/Sudoeste / Norte = %s x\n",
            fmtn(tl$tx_lab[tl$name_meso == "Sul/Sudoeste de Minas"]/tl$tx_lab[tl$name_meso == "Norte de Minas"], 1)))
cat(sprintf("  Gradiente total        Sul/Sudoeste / Norte = %s x\n",
            fmtn(tl$tx_conf[tl$name_meso == "Sul/Sudoeste de Minas"]/tl$tx_conf[tl$name_meso == "Norte de Minas"], 1)))

M  <- as.matrix(tl[, c("L", "N")]); M <- cbind(M[, 1], M[, 2] - M[, 1])
ch <- chisq.test(M)
cat(sprintf("\n  Heterogeneidade da taxa laboratorial entre mesorregioes: X2 = %s, gl = %d, %s\n",
            fmtn(ch$statistic, 1), ch$parameter, pfx(ch$p.value)))
for (m in c("Sul/Sudoeste de Minas", "Norte de Minas")) {
  r <- tl[tl$name_meso == m, ]; w <- wilson(r$L, r$N)
  cat(sprintf("  IC95%% (Wilson) %-24s %s%% [%s; %s]\n", m, fmtn(r$tx_lab, 1),
              fmtn(100*w[1], 1), fmtn(100*w[2], 1)))
}
saveRDS(tl, "Bancos_rds/confirmacao_laboratorial.rds")
options(width = w0)   # restaura a largura de impressao
cat("\nLAB_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 28_decomposicao_variancia.R]
# -----------------------------------------------------------------------------
# =============================================================================
# DECOMPOSICAO DA VARIANCIA DO GRADIENTE ESPACIAL (numerador compartilhado)
#   A correlacao de 0,77 entre a taxa de confirmacao (C/N) e a incidencia
#   confirmada (C/P), reportada no bloco 18, compartilha o numerador C e por isso
#   e em parte mecanica. Usando a identidade log(C/P) = log(C/N) + log(N/P):
#   (a) decompoe-se Var[log(C/P)] entre as 12 mesorregioes em uma parcela da taxa
#       de confirmacao e outra da incidencia notificada, com bootstrap;
#   (b) comparam-se as amplitudes dos gradientes de C/P e de N/P;
#   (c) calcula-se a correlacao HONESTA (C/N x N/P, sem numerador compartilhado);
#   (d) quantifica-se por permutacao quanto de 0,77 seria esperado por artefato.
#   Tudo a partir dos microdados, nao da Tabela 1 arredondada.
# Entrada: Bancos_rds/base_MG_2010_2024.rds, Bancos_rds/pop_mesorregiao_2010_2024.rds
# Saidas : console + Bancos_rds/decomposicao_variancia.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr)})
w0 <- getOption("width"); options(width = 200)   # tabelas largas; largura restaurada ao fim
fmtn <- function(x, d = 1) formatC(unname(x), format = "f", digits = d, decimal.mark = ",", big.mark = ".")
pfv  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p))))
pfx  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", pfv(p))))
pc   <- function(x, d = 1) paste0(fmtn(100 * x, d), "%")

b    <- readRDS("Bancos_rds/base_MG_2010_2024.rds")
popm <- readRDS("Bancos_rds/pop_mesorregiao_2010_2024.rds")
pm   <- popm %>% group_by(name_meso) %>% summarise(pop = mean(pop), .groups = "drop")
dd <- b %>% filter(!is.na(name_meso)) %>% group_by(name_meso) %>%
  summarise(C = sum(confirmado %in% TRUE), N = n(), .groups = "drop") %>%
  left_join(pm, by = "name_meso") %>%
  mutate(CP = C/pop*1e5,     # incidencia confirmada  (C/P)
         NP = N/pop*1e5,     # incidencia notificada  (N/P)
         CN = C/N,           # taxa de confirmacao    (C/N)
         lCP = log(CP), lNP = log(NP), lCN = log(CN))

cat("=============================================================\n")
cat(" DECOMPOSICAO DA VARIANCIA DO GRADIENTE ESPACIAL\n")
cat("=============================================================\n")
cat("Populacao media do periodo por mesorregiao (a mesma da Tabela 1).\n\n")
print(as.data.frame(dd %>% transmute(name_meso, C, N, pop = round(pop),
      `C/P` = round(CP, 2), `N/P` = round(NP, 2), `C/N %` = round(100*CN, 1)) %>% arrange(desc(`C/P`))))

# ---- (0) A correlacao com numerador compartilhado (a do bloco 18) -----------
s0 <- suppressWarnings(cor.test(dd$CN, dd$CP, method = "spearman"))
p0 <- suppressWarnings(cor.test(dd$CN, dd$CP, method = "pearson"))
cat(sprintf("\n--- (0) Numerador compartilhado: Spearman(C/N, C/P) rho = %s, %s\n",
            fmtn(unname(s0$estimate), 2), pfx(s0$p.value)))
cat(sprintf("        (Pearson r = %s, %s)\n", fmtn(unname(p0$estimate), 2), pfx(p0$p.value)))

# ---- (a) Decomposicao da variancia -----------------------------------------
#   Var(X+Y) = [Var(X)+Cov(X,Y)] + [Var(Y)+Cov(X,Y)], com X = log(C/N), Y = log(N/P)
X <- dd$lCN; Y <- dd$lNP; S <- dd$lCP
vX <- var(X); vY <- var(Y); cXY <- cov(X, Y); vS <- var(S)
sh_X <- (vX + cXY)/vS; sh_Y <- (vY + cXY)/vS
cat("\n--- (a) DECOMPOSICAO de Var[log(incidencia confirmada)] entre as 12 mesorregioes ---\n")
cat(sprintf("  Var[log(C/P)]          = %s   (conferencia: Var(X)+2Cov+Var(Y) = %s)\n",
            fmtn(vS, 4), fmtn(vX + 2*cXY + vY, 4)))
cat(sprintf("  Var[log(C/N)]          = %s\n", fmtn(vX, 4)))
cat(sprintf("  Var[log(N/P)]          = %s\n", fmtn(vY, 4)))
cat(sprintf("  Cov[log(C/N),log(N/P)] = %s\n", fmtn(cXY, 4)))
cat(sprintf("\n  PARCELA atribuivel a taxa de confirmacao C/N : (Var(X)+Cov)/Var(S) = %s = %s\n",
            fmtn(sh_X, 4), pc(sh_X)))
cat(sprintf("  PARCELA atribuivel a incidencia notificada N/P: (Var(Y)+Cov)/Var(S) = %s = %s\n",
            fmtn(sh_Y, 4), pc(sh_Y)))
cat(sprintf("  Soma = %s\n", pc(sh_X + sh_Y)))
cat(sprintf("  Correlacao (Pearson) entre log(C/N) e log(N/P) = %s\n", fmtn(cXY/sqrt(vX*vY), 3)))

set.seed(1)   # bootstrap das 12 mesorregioes (incerteza da parcela)
bs <- replicate(10000, {
  i <- sample(12, 12, TRUE); x <- dd$lCN[i]; y <- dd$lNP[i]
  if (var(x + y) <= 0) return(NA)
  (var(x) + cov(x, y))/var(x + y)
})
cat("  Bootstrap nao-parametrico (10.000 reamostragens das 12 mesorregioes):\n")
cat(sprintf("    parcela C/N: mediana %s | IC95%% percentil %s a %s\n",
            pc(median(bs, na.rm = TRUE)), pc(quantile(bs, .025, na.rm = TRUE)), pc(quantile(bs, .975, na.rm = TRUE))))

# ---- (b) Amplitude dos gradientes ------------------------------------------
gCP <- max(dd$CP)/min(dd$CP); gNP <- max(dd$NP)/min(dd$NP); gCN <- max(dd$CN)/min(dd$CN)
cat("\n--- (b) AMPLITUDE DOS GRADIENTES (razao maior/menor entre as 12 mesorregioes) ---\n")
cat(sprintf("  Incidencia CONFIRMADA (C/P): %s a %s por 100 mil -> %s x  [%s / %s]\n",
            fmtn(min(dd$CP), 1), fmtn(max(dd$CP), 1), fmtn(gCP, 1),
            dd$name_meso[which.max(dd$CP)], dd$name_meso[which.min(dd$CP)]))
cat(sprintf("  Incidencia NOTIFICADA (N/P): %s a %s por 100 mil -> %s x  [%s / %s]\n",
            fmtn(min(dd$NP), 1), fmtn(max(dd$NP), 1), fmtn(gNP, 1),
            dd$name_meso[which.max(dd$NP)], dd$name_meso[which.min(dd$NP)]))
cat(sprintf("  Taxa de confirmacao   (C/N): %s%% a %s%% -> %s x\n",
            fmtn(100*min(dd$CN), 1), fmtn(100*max(dd$CN), 1), fmtn(gCN, 1)))
cat(sprintf("\n  RAZAO ENTRE OS GRADIENTES (C/P) / (N/P) = %s / %s = %s\n",
            fmtn(gCP, 1), fmtn(gNP, 1), fmtn(gCP/gNP, 2)))
cat(sprintf("  (checagem multiplicativa: gradiente C/N = %s ; %s x %s = %s)\n",
            fmtn(gCN, 1), fmtn(gNP, 1), fmtn(gCN, 1), fmtn(gNP*gCN, 1)))

# ---- (c) Correlacao honesta -------------------------------------------------
sh <- suppressWarnings(cor.test(dd$CN,  dd$NP,  method = "spearman"))
ph <- suppressWarnings(cor.test(dd$lCN, dd$lNP, method = "pearson"))
kh <- suppressWarnings(cor.test(dd$CN,  dd$NP,  method = "kendall"))
cat("\n--- (c) CORRELACAO HONESTA (sem numerador compartilhado): C/N x N/P ---\n")
cat(sprintf("  Spearman rho = %s, %s  (n = 12)\n", fmtn(unname(sh$estimate), 2), pfx(sh$p.value)))
cat(sprintf("  Pearson  r   = %s, %s  [em logaritmo]\n", fmtn(unname(ph$estimate), 2), pfx(ph$p.value)))
cat(sprintf("  Kendall  tau = %s, %s\n", fmtn(unname(kh$estimate), 2), pfx(kh$p.value)))

# ---- (d) Quanto do rho observado e mecanico? -------------------------------
set.seed(1)   # permutacao: C/N e N/P independentes, so o numerador em comum
sim <- replicate(10000, {
  x <- sample(dd$lCN); y <- sample(dd$lNP)
  suppressWarnings(cor(x, x + y, method = "spearman"))
})
cat("\n--- (d) Spearman(C/N, C/P) esperado sob independencia (permutacao) ---\n")
cat(sprintf("  mediana %s | IC95%% %s a %s\n",
            fmtn(median(sim), 2), fmtn(quantile(sim, .025), 2), fmtn(quantile(sim, .975), 2)))
cat(sprintf("  Observado: %s -> %s\n", fmtn(unname(s0$estimate), 2),
    ifelse(s0$estimate <= quantile(sim, .975) & s0$estimate >= quantile(sim, .025),
           "DENTRO do intervalo esperado por puro artefato", "FORA do intervalo esperado")))
saveRDS(list(tabela = dd, parcelas = c(CN = sh_X, NP = sh_Y),
             variancias = c(vCP = vS, vCN = vX, vNP = vY, cov = cXY),
             boot_CN = quantile(bs, c(.025, .5, .975), na.rm = TRUE),
             gradientes = c(CP = gCP, NP = gNP, CN = gCN),
             honesta = c(rho = unname(sh$estimate), p = sh$p.value),
             publicada = c(rho = unname(s0$estimate), p = s0$p.value)),
        "Bancos_rds/decomposicao_variancia.rds")
options(width = w0)   # restaura a largura de impressao
cat("\nDECOMP_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 29_binomial_confirmacao_chuva.R]
# -----------------------------------------------------------------------------
# =============================================================================
# CONFIRMACAO DIFERENCIAL INDUZIDA PELA CHUVA (modelo binomial)
#   Se a chuva aumentar a PROBABILIDADE DE CONFIRMAR uma notificacao (mais
#   suspeita clinica, mais coleta de sorologia), parte do efeito da precipitacao
#   sobre os casos confirmados e vies de averiguacao, e nao incidencia. Testa-se
#   isso diretamente modelando P(confirmado | notificado) no painel mesorregiao-
#   semana com os MESMOS preditores do modelo principal:
#     cbind(conf, noti - conf) ~ pr_z + t_z + mesorregiao (EF) + ano
#   A razao de chances da precipitacao por desvio-padrao e confrontada com a
#   razao IRR_confirmados / IRR_notificados dos dois modelos Binomiais Negativos.
#   Reporta ainda sobredispersao, IC robustos por agrupamento e sensibilidades.
# Entrada: Bancos_rds/dados_temporais.rds
# Saidas : console + Bancos_rds/binomial_confirmacao.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(MASS); library(sandwich); library(lmtest)})
fmtn <- function(x, d = 1) formatC(unname(x), format = "f", digits = d, decimal.mark = ",", big.mark = ".")
fmti <- function(x) formatC(as.integer(round(unname(x))), format = "d", big.mark = ".", decimal.mark = ",")
pfv  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p))))
pfx  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", pfv(p))))
pc   <- function(x, d = 1) paste0(fmtn(100 * x, d), "%")

# Painel e padronizacao IDENTICOS aos do modelo temporal (script 02)
d <- readRDS("Bancos_rds/dados_temporais.rds") %>%
  filter(!is.na(pr_l2), !is.na(t_l2), !is.na(pop), pop > 0) %>%
  mutate(pr_z = as.numeric(scale(pr_l2)), t_z = as.numeric(scale(t_l2)),
         meso = factor(name_meso), yr = ano - 2017)

cat("=============================================================\n")
cat(" CONFIRMACAO DIFERENCIAL INDUZIDA PELA CHUVA\n")
cat("=============================================================\n")
cat(sprintf("Painel: %d mesorregiao-semanas | %d mesorregioes | 2010-2024\n", nrow(d), nlevels(d$meso)))
cat(sprintf("Notificados no painel: %s | confirmados: %s | taxa global %s\n",
            fmti(sum(d$noti)), fmti(sum(d$conf)), pc(sum(d$conf)/sum(d$noti))))
cat(sprintf("Semanas com noti > 0: %d (%s)\n", sum(d$noti > 0), pc(mean(d$noti > 0))))
cat(sprintf("DP de pr_l2 = %s mm/semana | DP de t_l2 = %s C\n", fmtn(sd(d$pr_l2), 1), fmtn(sd(d$t_l2), 2)))

# ---- (1) Os dois modelos Binomiais Negativos, recalculados -----------------
fit_nb <- function(y) suppressWarnings(
  glm.nb(reformulate(c("pr_z", "t_z", "meso", "yr", "offset(log(pop))"), response = y), data = d))
mC <- fit_nb("conf"); mN <- fit_nb("noti")
irr <- function(m, v) { s <- unname(summary(m)$coefficients[v, ])
  c(irr = exp(s[1]), lo = exp(s[1] - 1.96*s[2]), hi = exp(s[1] + 1.96*s[2]), p = s[4]) }
iC <- irr(mC, "pr_z"); iN <- irr(mN, "pr_z")
cat("\n--- (1) IRR da precipitacao por DP (modelos Binomiais Negativos) ---\n")
cat(sprintf("  CONFIRMADOS: IRR = %s (%s; %s), %s\n",
            fmtn(iC["irr"], 4), fmtn(iC["lo"], 4), fmtn(iC["hi"], 4), pfx(iC["p"])))
cat(sprintf("  NOTIFICADOS: IRR = %s (%s; %s), %s\n",
            fmtn(iN["irr"], 4), fmtn(iN["lo"], 4), fmtn(iN["hi"], 4), pfx(iN["p"])))
razao <- iC["irr"]/iN["irr"]
cat(sprintf("  RAZAO IRR_conf / IRR_noti = %s\n", fmtn(razao, 4)))

# ---- (2) Modelo binomial da probabilidade de confirmacao -------------------
mB <- glm(cbind(conf, noti - conf) ~ pr_z + t_z + meso + yr, family = binomial, data = d)
s  <- summary(mB)$coefficients
orf <- function(v, se = s[v, 2]) {
  bb <- unname(s[v, 1]); se <- unname(se); z <- bb/se
  c(or = exp(bb), lo = exp(bb - 1.96*se), hi = exp(bb + 1.96*se), p = 2*pnorm(-abs(z)))
}
oP <- orf("pr_z"); oT <- orf("t_z"); oY <- orf("yr")
cat("\n--- (2) MODELO BINOMIAL: P(confirmado | notificado) ---\n")
cat("  cbind(conf, noti-conf) ~ pr_z + t_z + mesorregiao + ano ; family = binomial(logit)\n")
cat(sprintf("  n = %d linhas com ensaios > 0 | ensaios (notificacoes) = %s\n",
            sum(d$noti > 0), fmti(sum(d$noti))))
cat(sprintf("  PRECIPITACAO: OR = %s (IC95%% %s; %s), %s\n",
            fmtn(oP["or"], 4), fmtn(oP["lo"], 4), fmtn(oP["hi"], 4), pfx(oP["p"])))
cat(sprintf("  TEMPERATURA : OR = %s (IC95%% %s; %s), %s\n",
            fmtn(oT["or"], 4), fmtn(oT["lo"], 4), fmtn(oT["hi"], 4), pfx(oT["p"])))
cat(sprintf("  ANO (yr)    : OR = %s (IC95%% %s; %s), %s\n",
            fmtn(oY["or"], 4), fmtn(oY["lo"], 4), fmtn(oY["hi"], 4), pfx(oY["p"])))

# ---- (3) Sobredispersao e IC robustos --------------------------------------
phi <- sum(residuals(mB, "pearson")^2)/df.residual(mB)
cat(sprintf("\n--- (3) Sobredispersao: phi (Pearson X2/gl) = %s ---\n", fmtn(phi, 3)))
mQ  <- glm(cbind(conf, noti - conf) ~ pr_z + t_z + meso + yr, family = quasibinomial, data = d)
sqp <- unname(suppressWarnings(summary(mQ))$coefficients["pr_z", ])
oPq <- c(or = exp(sqp[1]), lo = exp(sqp[1] - 1.96*sqp[2]), hi = exp(sqp[1] + 1.96*sqp[2]), p = sqp[4])
cat(sprintf("  QUASI-BINOMIAL   precip OR = %s (IC95%% %s; %s), %s\n",
            fmtn(oPq["or"], 4), fmtn(oPq["lo"], 4), fmtn(oPq["hi"], 4), pfx(oPq["p"])))
oPc <- orf("pr_z", se = sqrt(diag(sandwich::vcovCL(mB, cluster = d$meso, type = "HC0")))["pr_z"])
cat(sprintf("  ROBUSTO p/ mesorreg. (12 clusters) precip OR = %s (IC95%% %s; %s), %s\n",
            fmtn(oPc["or"], 4), fmtn(oPc["lo"], 4), fmtn(oPc["hi"], 4), pfx(oPc["p"])))
oPy <- orf("pr_z", se = sqrt(diag(sandwich::vcovCL(mB, cluster = interaction(d$meso, d$ano, drop = TRUE),
                                                  type = "HC0")))["pr_z"])
cat(sprintf("  ROBUSTO p/ mesorreg.-ano (180)   precip OR = %s (IC95%% %s; %s), %s\n",
            fmtn(oPy["or"], 4), fmtn(oPy["lo"], 4), fmtn(oPy["hi"], 4), pfx(oPy["p"])))

# ---- (4) Confronto direto: OR binomial x razao das IRR ----------------------
cat("\n--- (4) CONFRONTO: OR binomial x razao das IRR ---\n")
cat(sprintf("  OR (precip) do modelo binomial de confirmacao : %s (IC95%% %s; %s)\n",
            fmtn(oP["or"], 3), fmtn(oP["lo"], 3), fmtn(oP["hi"], 3)))
cat(sprintf("  Razao IRR_confirmados / IRR_notificados       : %s\n", fmtn(razao, 3)))
cat(sprintf("  A razao esta DENTRO do IC95%% da OR? %s\n",
            ifelse(razao >= oP["lo"] & razao <= oP["hi"], "SIM", "NAO")))
cat(sprintf("  Diferenca relativa: %s\n", pc(razao/oP["or"] - 1)))

# ---- (5) Sensibilidade ------------------------------------------------------
cat("\n--- (5) Sensibilidade ---\n")
alt <- function(f, rot) {
  m <- glm(f, family = binomial, data = d); ss <- summary(m)$coefficients["pr_z", ]
  cat(sprintf("  %-42s OR = %s (IC95%% %s; %s), %s\n", rot, fmtn(exp(ss[1]), 4),
              fmtn(exp(ss[1] - 1.96*ss[2]), 4), fmtn(exp(ss[1] + 1.96*ss[2]), 4), pfx(ss[4])))
}
alt(cbind(conf, noti-conf) ~ pr_z + t_z + meso,               "sem termo de tendencia anual")
alt(cbind(conf, noti-conf) ~ pr_z + t_z + meso + factor(ano), "ano como fator")
alt(cbind(conf, noti-conf) ~ pr_z + t_z + meso + yr + factor(format(as.Date(sem_ini), "%m")),
    "com termo sazonal (mes)")
alt(cbind(conf, noti-conf) ~ pr_z + meso + yr,                "so precipitacao (sem temp.)")

saveRDS(list(or_precip = oP, or_temp = oT, irr_conf = iC, irr_noti = iN, razao = razao, phi = phi),
        "Bancos_rds/binomial_confirmacao.rds")
cat("\nBINOM_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 30_espacial_binomial_negativa.R]
# -----------------------------------------------------------------------------
# =============================================================================
# MODELO ESPACIAL DE CONTAGEM (alternativa ao erro espacial gaussiano)
#   O modelo de erro espacial dos blocos 19-20 trata a incidencia municipal como
#   gaussiana em escala log, apesar de 46% dos municipios terem zero caso. Aqui
#   o desfecho volta a ser CONTAGEM: Binomial Negativa com offset(log(pop)) e as
#   MESMAS 4 covariaveis padronizadas (precipitacao, temperatura, IVS e densidade
#   em log), para confirmados e notificados.
#   (A) Binomial Negativa nao-espacial + I de Moran dos residuos de Pearson;
#   (B) Poisson, so para documentar a sobredispersao;
#   (C) se a autocorrelacao residual persistir, Binomial Negativa ESPACIAL com
#       campo aleatorio ICAR (Besag) e BYM (ICAR + i.i.d.) sobre a contiguidade
#       rainha, ajustados por REML com mgcv (s(bs = "mrf")).
#   Bloco demorado (cerca de 13 min): o ajuste BYM dos notificados e o gargalo.
# Entrada: Bancos_rds/dados_espaciais_municipal.rds
# Saidas : console + Bancos_rds/espacial_binomial_negativa.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("mgcv")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(dplyr); library(sf); library(spdep); library(MASS); library(mgcv)})
set.seed(1)
fmtn <- function(x, d = 1) formatC(unname(x), format = "f", digits = d, decimal.mark = ",", big.mark = ".")
pfv  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p))))
pfx  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", pfv(p))))
pc   <- function(x, d = 1) paste0(fmtn(100 * x, d), "%")

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds"); sf_use_s2(FALSE)
nb <- poly2nb(muni, queen = TRUE); W <- nb2listw(nb, style = "W", zero.policy = TRUE)
z  <- function(x) as.numeric(scale(x))
d  <- st_drop_geometry(muni) %>%
  mutate(precip_z = z(precip), temp_z = z(temp), ivs_z = z(ivs), dens_z = z(logdens))

cat("=============================================================\n")
cat(" MODELO ESPACIAL DE CONTAGEM (Binomial Negativa)\n")
cat("=============================================================\n")
cat(sprintf("Municipios: %d | vizinhos medios %s | ilhas %d\n",
            nrow(d), fmtn(mean(card(nb)), 2), sum(card(nb) == 0)))
cat(sprintf("Municipios com ZERO caso confirmado: %d (%s) | zero notificacao: %d (%s)\n",
            sum(d$conf == 0), pc(mean(d$conf == 0)), sum(d$noti == 0), pc(mean(d$noti == 0))))
cat(sprintf("DP das covariaveis brutas: precip %s mm/ano | temp %s C | IVS %s | log(dens) %s\n",
            fmtn(sd(d$precip), 1), fmtn(sd(d$temp), 2), fmtn(sd(d$ivs), 3), fmtn(sd(d$logdens), 2)))
cat("NA nas covariaveis:",
    sum(!complete.cases(d[, c("precip_z", "temp_z", "ivs_z", "dens_z", "pop", "conf", "noti")])), "\n")

VARS <- c(precip_z = "Precipitacao", temp_z = "Temperatura", ivs_z = "IVS", dens_z = "Densidade (log)")
linha <- function(s, v, rot) {
  bb <- unname(s[v, 1]); se <- unname(s[v, 2]); p <- unname(s[v, 4])
  cat(sprintf("    %-16s beta = %s (IC95%% %s; %s) | RT = %s (%s; %s) | %s\n", rot,
              fmtn(bb, 3), fmtn(bb - 1.96*se, 3), fmtn(bb + 1.96*se, 3),
              fmtn(exp(bb), 3), fmtn(exp(bb - 1.96*se), 3), fmtn(exp(bb + 1.96*se), 3), pfx(p)))
  c(b = bb, se = se, p = p)
}

resultado <- list()
for (y in c("conf", "noti")) {
  rot <- ifelse(y == "conf", "CONFIRMADOS", "NOTIFICADOS")
  cat(sprintf("\n===================== %s =====================\n", rot))
  # ---------------- (A) Binomial Negativa nao-espacial ----------------------
  m <- suppressWarnings(glm.nb(reformulate(c(names(VARS), "offset(log(pop))"), response = y), data = d))
  s <- summary(m)$coefficients
  cat(sprintf("\n  (A) BINOMIAL NEGATIVA  %s ~ 4 covariaveis padronizadas + offset(log pop)\n", y))
  cat(sprintf("      theta = %s (EP %s) | AIC = %s | log-verossimilhanca = %s\n",
              fmtn(m$theta, 3), fmtn(m$SE.theta, 3), fmtn(AIC(m), 1), fmtn(as.numeric(logLik(m)), 1)))
  cat(sprintf("      intercepto = %s\n", fmtn(unname(s["(Intercept)", 1]), 3)))
  for (v in names(VARS)) resultado[[paste0(y, "_bn_", v)]] <- linha(s, v, VARS[v])
  rp <- residuals(m, type = "pearson")
  set.seed(1); mc <- moran.mc(rp, W, nsim = 999, zero.policy = TRUE)
  mt <- moran.test(rp, W, zero.policy = TRUE)
  cat(sprintf("\n      I de MORAN dos residuos de Pearson: I = %s | permutacao (999) p = %s | analitico %s\n",
              fmtn(unname(mc$statistic), 3), pfv(mc$p.value), pfx(mt$p.value)))
  set.seed(1); m0 <- moran.mc(d[[paste0("inc_", y)]], W, nsim = 999, zero.policy = TRUE)
  cat(sprintf("      (referencia: I de Moran da incidencia bruta = %s, p = %s)\n",
              fmtn(unname(m0$statistic), 3), pfv(m0$p.value)))

  # ---------------- (B) Poisson: sobredispersao -----------------------------
  mp <- glm(reformulate(c(names(VARS), "offset(log(pop))"), response = y), family = poisson, data = d)
  cat(sprintf("      [sobredispersao vs Poisson: X2/gl = %s ; AIC Poisson = %s vs BN = %s]\n",
              fmtn(sum(residuals(mp, "pearson")^2)/df.residual(mp), 2), fmtn(AIC(mp), 1), fmtn(AIC(m), 1)))

  # ---------------- (C) BN ESPACIAL: campo ICAR/BYM via mgcv ----------------
  dg <- d; dg$id <- factor(seq_len(nrow(dg))); dg$id2 <- dg$id
  nbm <- nb; names(nbm) <- levels(dg$id); class(nbm) <- "nb"
  f_icar <- reformulate(c(names(VARS), "offset(log(pop))",
                          "s(id, bs='mrf', xt=list(nb=nbm), k=120)"), response = y)
  f_bym  <- reformulate(c(names(VARS), "offset(log(pop))",
                          "s(id, bs='mrf', xt=list(nb=nbm), k=120)", "s(id2, bs='re')"), response = y)
  for (nm in c("ICAR", "BYM")) {
    f <- if (nm == "ICAR") f_icar else f_bym
    g <- try(suppressWarnings(gam(f, family = mgcv::nb(), method = "REML", data = dg)), silent = TRUE)
    if (inherits(g, "try-error")) {
      cat(sprintf("\n  (C) %s: FALHOU -> %s\n", nm, conditionMessage(attr(g, "condition")))); next }
    sg <- summary(g)$p.table
    cat(sprintf("\n  (C) BN ESPACIAL %s (mgcv: family = nb, campo %s, REML)\n", nm,
                ifelse(nm == "ICAR", "ICAR (Besag) sobre a contiguidade rainha", "ICAR + efeito i.i.d. (BYM)")))
    cat(sprintf("      theta = %s | AIC = %s | gl efetivos do campo espacial = %s\n",
                fmtn(g$family$getTheta(TRUE), 3), fmtn(AIC(g), 1),
                paste(fmtn(summary(g)$edf, 1), collapse = " + ")))
    for (v in names(VARS)) {
      bb <- unname(sg[v, 1]); se <- unname(sg[v, 2]); p <- unname(sg[v, 4])
      cat(sprintf("      %-16s beta = %s (IC95%% %s; %s) | RT = %s (%s; %s) | %s\n", VARS[v],
                  fmtn(bb, 3), fmtn(bb - 1.96*se, 3), fmtn(bb + 1.96*se, 3),
                  fmtn(exp(bb), 3), fmtn(exp(bb - 1.96*se), 3), fmtn(exp(bb + 1.96*se), 3), pfx(p)))
      resultado[[paste0(y, "_", nm, "_", v)]] <- c(b = bb, se = se, p = p)
    }
    rg <- residuals(g, type = "pearson")
    set.seed(1); mcg <- moran.mc(rg, W, nsim = 999, zero.policy = TRUE)
    cat(sprintf("      I de MORAN dos residuos de Pearson: I = %s | p = %s\n",
                fmtn(unname(mcg$statistic), 3), pfv(mcg$p.value)))
    resultado[[paste0(y, "_", nm, "_moran")]] <- c(I = unname(mcg$statistic), p = mcg$p.value)
  }
}
saveRDS(resultado, "Bancos_rds/espacial_binomial_negativa.rds")
cat("\nBN_ESPACIAL_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 31_lisa_bh.R]
# -----------------------------------------------------------------------------
# =============================================================================
# BENJAMINI-HOCHBERG NO LISA UNIVARIADO E BIVARIADO
#   O bloco 23 aplica o FDR ao LISA univariado analitico. Aqui o controle de
#   multiplas comparacoes e estendido ao LISA BIVARIADO (incidencia confirmada x
#   precipitacao), que e o mapa da Figura 3, e comparam-se as especificacoes de
#   valor-p disponiveis:
#     univariado : localmoran() analitico (uso atual) x localmoran_perm()
#                  (999 e 99.999 permutacoes, bilateral e dobrado/GeoDa);
#     bivariado  : permutacao global de zy (metodo do script 03, o da Figura 3)
#                  x spdep::localmoran_bv() (permutacao condicional).
#   ATENCAO AO PISO DO PSEUDO-p: com B permutacoes o menor valor-p possivel e
#   1/(B+1); o BH sobre m = 853 testes exige, no posto 1, p <= 0,05/853 = 5,9e-5.
#   Com B = 999 nenhum teste alcanca esse limiar, de modo que "0 significativos"
#   e em parte artefato de resolucao — por isso tudo e repetido com B = 99.999.
#   Semente 1 em todas as permutacoes. Bloco demorado (cerca de 1,5 min).
# Entrada: Bancos_rds/dados_espaciais_municipal.rds
# Saidas : console + Bancos_rds/lisa_bh.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(sf); library(spdep)})
w0 <- getOption("width"); options(width = 240)   # tabelas largas; largura restaurada ao fim
fmtn <- function(x, d = 1) formatC(unname(x), format = "f", digits = d, decimal.mark = ",", big.mark = ".")

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds"); sf_use_s2(FALSE)
nb <- poly2nb(muni, queen = TRUE); W <- nb2listw(nb, style = "W", zero.policy = TRUE)
M  <- nrow(muni); NS <- "Não significativo"
LV <- c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", NS)
quad <- function(zx, lagz) ifelse(zx > 0 & lagz > 0, "Alto-Alto",
                          ifelse(zx < 0 & lagz < 0, "Baixo-Baixo",
                          ifelse(zx > 0 & lagz < 0, "Alto-Baixo", "Baixo-Alto")))
classif <- function(q, p, a = 0.05) factor(ifelse(p < a, q, NS), levels = LV)
lista   <- function(v) paste(sprintf("%s = %d", names(v), as.integer(v)), collapse = " | ")

relata <- function(cl, rot) {
  n <- sum(cl != NS)
  cat(sprintf("\n  >> %s : %d municipios significativos\n", rot, n))
  if (n == 0) return(invisible(0))
  cat("     por quadrante   : ", lista(table(droplevels(cl[cl != NS]))), "\n", sep = "")
  tm <- sort(table(muni$name_meso[cl != NS]), decreasing = TRUE); tm <- tm[tm > 0]
  cat("     por mesorregiao : ", lista(tm), "\n", sep = "")
  if (any(cl == "Alto-Alto")) {
    ta <- sort(table(muni$name_meso[cl == "Alto-Alto"]), decreasing = TRUE); ta <- ta[ta > 0]
    cat("     ALTO-ALTO       : ", lista(ta), "\n", sep = "")
  }
  invisible(n)
}
resumo_bh <- function(p, rot, qd) {
  pbh <- p.adjust(p, "BH")
  cat(sprintf("\n--- %s ---\n", rot))
  cat(sprintf("    menor p observado = %s | p<0,05 bruto: %d | apos BH: %d (de %d)\n",
              format(min(p, na.rm = TRUE), scientific = TRUE, digits = 3),
              sum(p < 0.05, na.rm = TRUE), sum(pbh < 0.05, na.rm = TRUE), M))
  relata(classif(qd, p),   "SEM correcao")
  relata(classif(qd, pbh), "COM Benjamini-Hochberg")
  invisible(c(bruto = sum(p < 0.05, na.rm = TRUE), bh = sum(pbh < 0.05, na.rm = TRUE),
              AA_bruto = sum(classif(qd, p) == "Alto-Alto"), AA_bh = sum(classif(qd, pbh) == "Alto-Alto")))
}

# =========================== UNIVARIADO ======================================
cat("=============================================================\n")
cat(" (A) LISA UNIVARIADO - incidencia confirmada\n")
cat("=============================================================\n")
cat("Piso do pseudo-p: B=999 -> 1,0e-03 | B=99.999 -> 1,0e-05\n")
cat(sprintf("Limiar BH no posto 1 (m=%d): %s\n", M, format(0.05/M, scientific = TRUE, digits = 3)))

x  <- muni$inc_conf
zx <- as.numeric(scale(x)); lagz <- lag.listw(W, zx, zero.policy = TRUE); qd <- quad(zx, lagz)
p_a <- localmoran(x, W, zero.policy = TRUE)[, "Pr(z != E(Ii))"]
set.seed(1); l999 <- localmoran_perm(x, W, nsim = 999,   zero.policy = TRUE, iseed = 1)
set.seed(1); lBIG <- localmoran_perm(x, W, nsim = 99999, zero.policy = TRUE, iseed = 1)
cat(sprintf("Spearman(p analitico, pseudo-p bilateral B=999) = %s\n",
            fmtn(cor(p_a, l999[, "Pr(z != E(Ii)) Sim"], method = "spearman"), 3)))

u_a  <- resumo_bh(p_a,                          "ANALITICO (localmoran) - USO ATUAL (bloco 23)", qd)
u_9  <- resumo_bh(l999[, "Pr(z != E(Ii)) Sim"], "PERMUTACIONAL bilateral, B=999 (localmoran_perm)", qd)
u_9f <- resumo_bh(l999[, "Pr(folded) Sim"],     "PERMUTACIONAL dobrado/GeoDa, B=999", qd)
u_B  <- resumo_bh(lBIG[, "Pr(z != E(Ii)) Sim"], "PERMUTACIONAL bilateral, B=99.999", qd)
u_Bf <- resumo_bh(lBIG[, "Pr(folded) Sim"],     "PERMUTACIONAL dobrado/GeoDa, B=99.999", qd)

cat("\n--- O aglomerado Alto-Alto encolhe? (Zona da Mata x Sul/Sudoeste) ---\n")
for (cfg in list(list(p_a, "analitico"),
                 list(l999[, "Pr(z != E(Ii)) Sim"], "perm. bilateral B=999"),
                 list(l999[, "Pr(folded) Sim"],     "perm. dobrado B=999"),
                 list(lBIG[, "Pr(z != E(Ii)) Sim"], "perm. bilateral B=99.999"),
                 list(lBIG[, "Pr(folded) Sim"],     "perm. dobrado B=99.999"))) {
  p <- cfg[[1]]; pbh <- p.adjust(p, "BH")
  cat(sprintf("    %-26s Zona da Mata Alto-Alto: %2d (bruto) -> %2d (BH) | Sul/Sudoeste: %2d -> %2d\n", cfg[[2]],
              sum(classif(qd, p)   == "Alto-Alto" & muni$name_meso == "Zona da Mata"),
              sum(classif(qd, pbh) == "Alto-Alto" & muni$name_meso == "Zona da Mata"),
              sum(classif(qd, p)   == "Alto-Alto" & muni$name_meso == "Sul/Sudoeste de Minas"),
              sum(classif(qd, pbh) == "Alto-Alto" & muni$name_meso == "Sul/Sudoeste de Minas")))
}
cat("\n  Conferencia com a coluna lisa_conf gravada no banco:\n"); print(table(muni$lisa_conf))

# =========================== BIVARIADO =======================================
cat("\n\n=============================================================\n")
cat(" (B) LISA BIVARIADO - incidencia confirmada x precipitacao\n")
cat("=============================================================\n")
zy <- as.numeric(scale(muni$precip))
lagy <- lag.listw(W, zy, zero.policy = TRUE); Ii_bv <- zx * lagy; qb <- quad(zx, lagy)

# (B1) metodo do script 03 (o da Figura 3): permutacao GLOBAL de zy
proj_biv <- function(B, seed = 1) {
  set.seed(seed); cnt <- integer(M)
  for (s in 1:B) { lp <- lag.listw(W, sample(zy), zero.policy = TRUE)
                   cnt <- cnt + (abs(zx*lp) >= abs(Ii_bv)) }
  (cnt + 1)/(B + 1)
}
p_b999 <- proj_biv(999); p_bBIG <- proj_biv(99999)

# (B2) spdep::localmoran_bv (permutacao condicional)
set.seed(1); bv999 <- localmoran_bv(zx, zy, W, nsim = 999,   zero.policy = TRUE, iseed = 1)
set.seed(1); bvBIG <- localmoran_bv(zx, zy, W, nsim = 99999, zero.policy = TRUE, iseed = 1)
cat(sprintf("Orientacao conferida: cor(zx*lag(zy) manual, Ibvi do spdep) = %s\n",
            fmtn(cor(Ii_bv, bv999[, "Ibvi"]), 4)))
ctwo <- "Pr(z != E(Ibvi)) Sim"; cfld <- "Pr(folded) Sim"

b_1 <- resumo_bh(p_b999,        "PERMUTACAO GLOBAL de zy, B=999 (metodo da Figura 3)", qb)
b_2 <- resumo_bh(p_bBIG,        "PERMUTACAO GLOBAL de zy, B=99.999", qb)
b_3 <- resumo_bh(bv999[, ctwo], "spdep::localmoran_bv bilateral, B=999", qb)
b_4 <- resumo_bh(bvBIG[, ctwo], "spdep::localmoran_bv bilateral, B=99.999", qb)
b_5 <- resumo_bh(bvBIG[, cfld], "spdep::localmoran_bv dobrado/GeoDa, B=99.999", qb)

cat("\n  Conferencia com a coluna lisa_biv gravada no banco:\n"); print(table(muni$lisa_biv))

# ---- Tabelas por mesorregiao ------------------------------------------------
tabela <- function(qq, p, rot) {
  pbh <- p.adjust(p, "BH")
  tb <- data.frame(name_meso = muni$name_meso, bruto = classif(qq, p), bh = classif(qq, pbh)) %>%
    group_by(name_meso) %>% summarise(
      municipios = n(), sig_bruto = sum(bruto != NS), AA_bruto = sum(bruto == "Alto-Alto"),
      sig_BH = sum(bh != NS), AA_BH = sum(bh == "Alto-Alto"), BB_BH = sum(bh == "Baixo-Baixo"),
      AB_BH = sum(bh == "Alto-Baixo"), BA_BH = sum(bh == "Baixo-Alto"), .groups = "drop") %>%
    arrange(desc(sig_BH), desc(sig_bruto))
  cat(sprintf("\n--- %s ---\n", rot)); print(as.data.frame(tb))
}
tabela(qb, p_b999, "LISA BIVARIADO por mesorregiao - permutacao global, B=999 (o da Figura 3)")
tabela(qb, p_bBIG, "LISA BIVARIADO por mesorregiao - permutacao global, B=99.999")
tabela(qb, bvBIG[, ctwo], "LISA BIVARIADO por mesorregiao - localmoran_bv bilateral, B=99.999")
tabela(qd, p_a,    "LISA UNIVARIADO por mesorregiao - analitico (uso atual)")
tabela(qd, lBIG[, "Pr(z != E(Ii)) Sim"], "LISA UNIVARIADO por mesorregiao - permutacional B=99.999")

cat("\n--- Sobreviventes do LISA UNIVARIADO analitico apos BH ---\n")
s_uni <- classif(qd, p.adjust(p_a, "BH")) != NS
print(data.frame(muni = muni$name_muni[s_uni], meso = muni$name_meso[s_uni],
                 quadrante = as.character(classif(qd, p.adjust(p_a, "BH"))[s_uni]),
                 inc_conf = round(muni$inc_conf[s_uni], 1),
                 p = signif(p_a[s_uni], 3), p_BH = signif(p.adjust(p_a, "BH")[s_uni], 3)) %>%
      arrange(p))

cat("\n--- Sobreviventes do LISA BIVARIADO apos BH (permutacao global, B=99.999) ---\n")
s_biv <- classif(qb, p.adjust(p_bBIG, "BH")) != NS
if (sum(s_biv) == 0) cat("  (nenhum)\n") else
print(data.frame(muni = muni$name_muni[s_biv], meso = muni$name_meso[s_biv],
                 quadrante = as.character(classif(qb, p.adjust(p_bBIG, "BH"))[s_biv]),
                 p = signif(p_bBIG[s_biv], 3), p_BH = signif(p.adjust(p_bBIG, "BH")[s_biv], 3)) %>%
      arrange(p))

saveRDS(list(univariado = list(analitico = u_a, perm999 = u_9, perm999_dobrado = u_9f,
                               permBIG = u_B, permBIG_dobrado = u_Bf),
             bivariado  = list(global999 = b_1, globalBIG = b_2, bv999 = b_3,
                               bvBIG = b_4, bvBIG_dobrado = b_5),
             p_univ_analitico = p_a, p_biv_global999 = p_b999),
        "Bancos_rds/lisa_bh.rds")
options(width = w0)   # restaura a largura de impressao
cat("\nLISA_BH_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 32_estabilidade_ivs.R]
# -----------------------------------------------------------------------------
# =============================================================================
# ESTABILIDADE TEMPORAL DO IVS E COMPARACAO COM INDICE CENSITARIO
#   Responde a critica de que o IVS, calculado sobre o Censo 2010, poderia nao
#   representar o contexto socioeconomico do fim do periodo analisado (2024).
#   (i)  mede a estabilidade do ORDENAMENTO municipal entre os Censos 2010 e
#        2022, em indicadores comparaveis (saneamento e alfabetizacao);
#   (ii) constroi um indice de vulnerabilidade censitario para cada ano e
#        reestima as analises espaciais com cada medida, verificando se a
#        conclusao do estudo depende da escolha.
#   Observacao: usa a API HTTP do SIDRA em vez do pacote sidrar, que falhou
#   ("nao e possivel abrir a conexao") no ambiente de execucao.
#   Ressalva: a taxa de alfabetizacao tem cortes etarios distintos nos dois
#   censos (10+ em 2010, 15+ em 2022); a comparacao e de postos, nao de niveis.
# Entradas: Bancos_rds/dados_espaciais_municipal.rds, saneamento_mg_2022.rds,
#           vulnerabilidade_mg.rds + SIDRA (t/3218 2010, t/1383 2010, t/9543 2022)
# Saidas  : console + Bancos_rds/estabilidade_ivs.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("jsonlite")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(dplyr); library(sf); library(spdep); library(spatialreg)
                  library(MASS); library(jsonlite)})
z <- function(x) as.numeric(scale(x))

sidra <- function(url) {
  d <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)
  cab <- d[1, ]; dados <- d[-1, , drop = FALSE]
  list(cab = cab, dados = dados)
}
pct_cat <- function(url, campo_cat, cats) {   # % das categorias sobre o Total
  r <- sidra(url); d <- r$dados
  out <- d |>
    transmute(cod = as.integer(D1C), cat = .data[[campo_cat]],
              val = suppressWarnings(as.numeric(V))) |>
    filter(!is.na(val))
  tot <- out |> filter(cat == "0") |> dplyr::select(cod, total = val)
  adq <- out |> filter(cat %in% cats) |> group_by(cod) |>
    summarise(adq = sum(val), .groups = "drop")
  inner_join(tot, adq, by = "cod") |>
    mutate(pct = round(100 * adq / total, 2)) |> dplyr::select(cod, pct)
}

B <- "https://apisidra.ibge.gov.br/values/t/3218/n6/in%20n3%2031/p/2010/v/96"
cat("Baixando Censo 2010 (saneamento) e alfabetizacao 2010/2022 do SIDRA...\n")
s10 <- pct_cat(paste0(B, "/c61/0,92853/c67/0/c299/0/c309/0"), "D4C", "92853") |> rename(agua = pct) |>
  inner_join(pct_cat(paste0(B, "/c61/0/c67/0/c299/0,9678,9679/c309/0"), "D6C",
                     c("9678", "9679")) |> rename(esgoto = pct), by = "cod") |>
  inner_join(pct_cat(paste0(B, "/c61/0/c67/0,2520/c299/0/c309/0"), "D5C", "2520") |>
               rename(lixo = pct), by = "cod")
taxa <- function(url) {
  r <- sidra(url)
  r$dados |> transmute(cod = as.integer(D1C), v = suppressWarnings(as.numeric(V))) |> filter(!is.na(v))
}
a10 <- taxa("https://apisidra.ibge.gov.br/values/t/1383/n6/in%20n3%2031/p/2010/v/1646/c2/6794") |> rename(alfab_10 = v)
a22 <- taxa(paste0("https://apisidra.ibge.gov.br/values/t/9543/n6/in%20n3%2031/p/2022/v/2513",
                   "/c2/6794/c86/95251/c287/100362")) |> rename(alfab_22 = v)

muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds")
s22  <- readRDS("Bancos_rds/saneamento_mg_2022.rds")
vul  <- readRDS("Bancos_rds/vulnerabilidade_mg.rds")
idx  <- function(a, e, l, f) -(z(a) + z(e) + z(l) + z(f)) / 4

d <- muni |> st_drop_geometry() |>
  dplyr::select(cod, conf, noti, pop, precip, temp, logdens, ivs) |>
  inner_join(s10, by = "cod") |>
  inner_join(s22 |> rename(agua22 = agua, esgoto22 = esgoto, lixo22 = lixo), by = "cod") |>
  inner_join(a10, by = "cod") |> inner_join(a22, by = "cod") |>
  inner_join(vul[, c("cod", "ivs_iu", "ivs_ch", "ivs_rt")], by = "cod") |>
  mutate(vul10 = idx(agua, esgoto, lixo, alfab_10),
         vul22 = idx(agua22, esgoto22, lixo22, alfab_22),
         san10 = (z(agua) + z(esgoto) + z(lixo)) / 3)

cat("\n== (i) ESTABILIDADE DO ORDENAMENTO ENTRE OS CENSOS ==\n")
cat(sprintf("  indice censitario 2010 x 2022 : Spearman %.3f\n",
            cor(d$vul10, d$vul22, method = "spearman")))
for (v in c("agua", "esgoto", "lixo")) {
  cat(sprintf("  %-7s 2010 x 2022            : Spearman %.3f\n", v,
              cor(d[[v]], d[[paste0(v, "22")]], method = "spearman")))
}
cat(sprintf("  alfabetizacao 2010 x 2022     : Spearman %.3f\n",
            cor(d$alfab_10, d$alfab_22, method = "spearman")))
cat(sprintf("  IVS x indice 2010 = %.3f | IVS x indice 2022 = %.3f  (dif %.3f)\n",
            cor(d$ivs, d$vul10, method = "spearman"),
            cor(d$ivs, d$vul22, method = "spearman"),
            cor(d$ivs, d$vul22, method = "spearman") - cor(d$ivs, d$vul10, method = "spearman")))
q <- function(x) cut(rank(x), quantile(rank(x), 0:5/5), include.lowest = TRUE, labels = 1:5)
tb <- table(q(d$vul10), q(d$vul22))
cat(sprintf("  permanecem no mesmo quintil: %.1f%% | movem-se ate um quintil: %.1f%%\n",
            100 * sum(diag(tb)) / sum(tb),
            100 * sum(tb[abs(row(tb) - col(tb)) <= 1]) / sum(tb)))

cat("\n== (ii) A CONCLUSAO DEPENDE DA MEDIDA? (BN com offset) ==\n")
sf_use_s2(FALSE)
g <- muni[match(d$cod, muni$cod), ]; nb <- poly2nb(g, queen = TRUE)
W <- nb2listw(nb, style = "W", zero.policy = TRUE)
base <- glm.nb(conf ~ z(precip) + z(temp) + z(logdens) + offset(log(pop)), data = d)
cat(sprintf("  %-10s AIC %8.1f  (sem vulnerabilidade)\n", "nenhuma", AIC(base)))
for (m in c("ivs", "ivs_iu", "ivs_ch", "ivs_rt", "vul10", "vul22")) {
  f <- as.formula(sprintf("conf ~ z(precip)+z(temp)+z(%s)+z(logdens)+offset(log(pop))", m))
  fit <- glm.nb(f, data = d); co <- summary(fit)$coefficients
  i <- grep("^z\\(", rownames(co))[3]
  cat(sprintf("  %-10s AIC %8.1f  beta %+.3f (IC95%% %+.3f; %+.3f) p-valor %s\n",
              m, AIC(fit), co[i, 1], co[i, 1] - 1.96 * co[i, 2], co[i, 1] + 1.96 * co[i, 2],
              ifelse(co[i, 4] < 0.001, "<0,001", sprintf("%.3f", co[i, 4]))))
}
cat("\n  -- IVS e saneamento no mesmo modelo --\n")
f2 <- glm.nb(conf ~ z(precip) + z(temp) + z(ivs) + z(san10) + z(logdens) + offset(log(pop)), data = d)
co <- summary(f2)$coefficients
for (t in c("z(ivs)", "z(san10)")) {
  i <- which(rownames(co) == t)
  cat(sprintf("  %-10s beta %+.3f (IC95%% %+.3f; %+.3f) p-valor %s\n", t, co[i, 1],
              co[i, 1] - 1.96 * co[i, 2], co[i, 1] + 1.96 * co[i, 2],
              ifelse(co[i, 4] < 0.001, "<0,001", sprintf("%.3f", co[i, 4]))))
}
cat(sprintf("  AIC do modelo conjunto: %.1f\n", AIC(f2)))
saveRDS(d, "Bancos_rds/estabilidade_ivs.rds")
cat("\nSalvo: Bancos_rds/estabilidade_ivs.rds\n")
