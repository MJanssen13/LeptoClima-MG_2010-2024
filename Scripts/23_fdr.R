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
