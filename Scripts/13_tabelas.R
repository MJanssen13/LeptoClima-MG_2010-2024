# =============================================================================
# TABELAS 1 e 2 do artigo (reprodutiveis)
#  T1: casos, incidencia acumulada e Kendall (precip; semanal e 4 SE) por mesorregiao
#  T2: IRR (Binomial Negativa, precip+temp) e Moran bivariado (incidencia x covariavel)
# Saidas: Bancos_rds/tabela1.csv, Bancos_rds/tabela2.csv
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(MASS); library(sf); library(spdep)})

panel <- readRDS("Bancos_rds/dados_temporais.rds")
popm  <- readRDS("Bancos_rds/pop_mesorregiao_2010_2024.rds")
st <- function(p) ifelse(is.na(p), "", ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns"))))
ktau <- function(x,y){ ok<-is.finite(x)&is.finite(y); x<-x[ok]; y<-y[ok]
  if(length(x)<10) return(c(NA,NA)); r<-suppressWarnings(cor.test(x,y,method="kendall")); c(unname(r$estimate),r$p.value) }

# ---- TABELA 1 ----
pm <- popm |> group_by(name_meso) |> summarise(pop=mean(pop), .groups="drop")
t1 <- panel |> group_by(name_meso) |> group_modify(function(d,...){
  a<-ktau(d$inc_conf,d$pr_l2); b<-ktau(d$r_inc_conf,d$r_pr_l2)
  tibble(Casos=sum(d$conf), tauP=a[1],pP=a[2], tauP4=b[1],pP4=b[2]) }) |> ungroup() |>
  left_join(pm, by="name_meso") |>
  transmute(Mesorregiao=name_meso, Casos,
            `Incidencia_100mil`=round(Casos/pop*1e5,1),
            `Kendall_semanal`=paste0(sprintf("%.2f",tauP), st(pP)),
            `Kendall_4SE`=paste0(sprintf("%.2f",tauP4), st(pP4))) |>
  arrange(desc(Incidencia_100mil))
write.csv(t1, "Bancos_rds/tabela1.csv", row.names=FALSE)
cat("=== TABELA 1 ===\n"); print(as.data.frame(t1))

# ---- TABELA 2A: regressao Binomial Negativa (IRR por desvio-padrao) ----
d <- panel |> filter(!is.na(pr_l2),!is.na(t_l2),pop>0) |>
  mutate(pr_z=as.numeric(scale(pr_l2)), t_z=as.numeric(scale(t_l2)), meso=factor(name_meso), yr=ano-2017)
irr <- function(m,v){ s<-summary(m)$coefficients[v,]; sprintf("%.2f (%.2f-%.2f)%s",
  exp(s[1]), exp(s[1]-1.96*s[2]), exp(s[1]+1.96*s[2]), st(s[4])) }
mf <- suppressWarnings(glm.nb(conf~pr_z+t_z+meso+yr+offset(log(pop)), data=d))
mc <- suppressWarnings(glm.nb(noti~pr_z+t_z+meso+yr+offset(log(pop)), data=d))

# ---- TABELA 2B: Moran bivariado (incidencia confirmados x covariavel) ----
muni <- readRDS("Bancos_rds/dados_espaciais_municipal.rds"); sf_use_s2(FALSE)
nb <- poly2nb(muni, queen=TRUE); W <- nb2listw(nb, style="W", zero.policy=TRUE)
bim <- function(x,y,nsim=999){ zx<-as.numeric(scale(x)); zy<-as.numeric(scale(y))
  Ixy<-function(zy){ly<-lag.listw(W,zy,zero.policy=TRUE); sum(zx*ly)/sum(zx^2)}; I<-Ixy(zy)
  perm<-replicate(nsim,Ixy(sample(zy))); c(I,(sum(abs(perm)>=abs(I))+1)/(nsim+1)) }
labs <- c(precip="Precipitacao",temp="Temperatura",ivs="IVS",esgoto="Esgoto",agua="Agua",lixo="Lixo")
mb <- sapply(names(labs), function(v){ r<-bim(muni$inc_conf, muni[[v]]); sprintf("%.2f%s", r[1], st(r[2])) })

t2 <- data.frame(
  Variavel = c("Precipitacao","Temperatura","IVS","Esgoto","Agua","Lixo"),
  IRR_confirmados = c(irr(mf,"pr_z"), irr(mf,"t_z"), NA,NA,NA,NA),
  IRR_notificados = c(irr(mc,"pr_z"), irr(mc,"t_z"), NA,NA,NA,NA),
  Moran_bivariado = unname(mb))
write.csv(t2, "Bancos_rds/tabela2.csv", row.names=FALSE)
cat("\n=== TABELA 2 ===\n"); print(t2)
