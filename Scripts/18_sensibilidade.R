# =============================================================================
# ANALISES DE SENSIBILIDADE (compoem o artigo; nao alteram o texto principal)
#   A) Regionalizacao: mesorregiao IBGE (atual) x MACRORREGIAO DE SAUDE (PDR/SES-MG)
#   B) Defasagem temporal: 2 SE (atual) x 0 SE (contemporanea)
# Saidas (console + rds): comparacao de incidencia por regiao, Kendall e IRR (BN).
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(lubridate); library(MASS); library(geobr); library(sf)})
setwd("D:/Pesquisas/Leptospirose")
st <- function(p) ifelse(is.na(p),"", ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns"))))
ktau <- function(x,y){ ok<-is.finite(x)&is.finite(y); if(sum(ok)<10) return(c(NA,NA))
  r<-suppressWarnings(cor.test(x[ok],y[ok],method="kendall")); c(unname(r$estimate), r$p.value) }
irr <- function(m,v){ s<-summary(m)$coefficients[v,]; sprintf("%.2f (%.2f-%.2f)%s",
  exp(s[1]), exp(s[1]-1.96*s[2]), exp(s[1]+1.96*s[2]), st(s[4])) }

# ---- Lookup municipio -> macrorregiao de saude (geobr / PDR) ----
hr <- st_drop_geometry(read_health_region(year = 2013, simplified = TRUE, showProgress = FALSE))
hr <- hr[hr$abbrev_state == "MG", ]
macro_lk <- transmute(hr, cod = as.integer(code_muni), cod6 = as.integer(code_muni6),
                      macro = as.character(name_health_macroregion))
saveRDS(macro_lk, "Bancos_rds/lookup_macrorregiao_saude.rds")
cat("== Macrorregioes de saude (MG):", length(unique(macro_lk$macro)), "==\n")
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
saveRDS(list(incA=incA, kA=kA, est=est), "Bancos_rds/sensibilidade.rds")
cat("\nSENS_OK\n")
