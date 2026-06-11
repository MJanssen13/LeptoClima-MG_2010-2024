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
