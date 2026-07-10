# =============================================================================
# 04 - CARACTERIZACAO E TABELAS
#   Caracterizacao descritiva dos casos, geracao das Tabelas 1 e 2 (CSV) e
#   montagem do manuscrito em .docx (uso local; a saida .docx nao e versionada).
#   Reune os antigos scripts 17, 13 e 14.
# =============================================================================

# -----------------------------------------------------------------------------
# [bloco: 17_caracterizacao_casos.R]
# -----------------------------------------------------------------------------
# =============================================================================
# CARACTERIZACAO DESCRITIVA dos casos confirmados de leptospirose (MG, 2010-2024)
#   sexo, idade, raca/cor, evolucao/letalidade, hospitalizacao, antecedentes de
#   exposicao, tendencia anual (Mann-Kendall) e sazonalidade. Subsidia o 1o
#   paragrafo de Resultados (participantes) exigido pelo modelo da RESS.
# Entrada: Bancos_rds/base_MG_2010_2024.rds, pop_municipal_2010_2024.rds
# Saida (console): estatisticas para o texto
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(lubridate)})

b   <- readRDS("Bancos_rds/base_MG_2010_2024.rds")
pop <- readRDS("Bancos_rds/pop_municipal_2010_2024.rds")
cf  <- b[b$confirmado %in% TRUE, ]
n   <- nrow(cf)
pct <- function(x) sprintf("%.1f%%", 100 * x)
cat("== Notificados:", nrow(b), "| Confirmados:", n, "(", pct(n/nrow(b)), ")\n")

# ---- Sexo ----
sx <- table(cf$CS_SEXO)
cat("\nSEXO: M =", sx["M"], pct(sx["M"]/n), "| F =", sx["F"], "| razao M:F =",
    sprintf("%.1f", sx["M"]/sx["F"]), "\n")

# ---- Idade (NU_IDADE_N: 1o digito = unidade 1h/2d/3m/4ano; demais = valor) ----
id <- suppressWarnings(as.integer(as.character(cf$NU_IDADE_N)))
un <- id %/% 1000; vl <- id %% 1000
idade <- ifelse(un == 4, vl, ifelse(un %in% 1:3, 0, NA))
cat("IDADE (anos): mediana", median(idade, na.rm=TRUE), "| IIQ",
    paste(quantile(idade, c(.25,.75), na.rm=TRUE), collapse="-"),
    "| media", round(mean(idade, na.rm=TRUE),1), "| faltantes", sum(is.na(idade)), "\n")
fx <- cut(idade, c(-1,19,39,59,Inf), labels=c("0-19","20-39","40-59","60+"))
cat("  Faixas:"); print(round(100*prop.table(table(fx)),1))
cat("  20-59 anos (idade produtiva):", pct(mean(idade>=20 & idade<=59, na.rm=TRUE)), "\n")

# ---- Raca/cor (1 branca 2 preta 3 amarela 4 parda 5 indigena 9 ign) ----
rc <- cf$CS_RACA; rc[rc %in% c("9","")] <- NA
lab <- c("1"="Branca","2"="Preta","3"="Amarela","4"="Parda","5"="Indigena")
rck <- lab[as.character(rc)]; valid <- sum(!is.na(rck))
cat("\nRACA/COR (entre", valid, "validos): Preta+Parda =",
    pct(mean(rck %in% c("Preta","Parda"), na.rm=TRUE)),
    "| Branca =", pct(mean(rck=="Branca", na.rm=TRUE)), "\n")

# ---- Evolucao / letalidade (1 cura 2 obito agravo 3 obito outra 9 ign) ----
ev <- cf$EVOLUCAO
conhec <- sum(ev %in% c("1","2","3"))
obito_ag <- sum(ev %in% "2")
cat("\nEVOLUCAO: obitos pelo agravo =", obito_ag, "| evolucao conhecida =", conhec, "\n")
cat("  LETALIDADE =", pct(obito_ag/conhec), "(", obito_ag, "/", conhec, ")\n")

# ---- Hospitalizacao (ATE_HOSP: 1 sim 2 nao 9 ign) ----
if ("ATE_HOSP" %in% names(cf)) { hp <- cf$ATE_HOSP; ok <- hp %in% c("1","2")
  cat("HOSPITALIZACAO:", pct(mean(hp[ok]=="1")), "preenchidos", pct(mean(ok)), "\n") }

# ---- Antecedentes de exposicao (ANT_CB_*: 1 = sim) ----
ant <- c(ANT_CB_ROE="contato com roedores", ANT_CB_LIX="lixo/entulho",
         ANT_CB_LAM="lama/enchente", ANT_CB_CAI="aguas/caixa d'agua")
for (a in names(ant)) if (a %in% names(cf)) {
  v <- cf[[a]]; ok <- v %in% c("1","2")
  if (sum(ok) > n*0.3) cat("  ", ant[a], "=", pct(mean(v[ok]=="1")), "(preench.", pct(mean(ok)), ")\n")
}

# ---- Tendencia anual (incidencia) ----
cf$ano <- year(as.Date(cf$DT_SIN_PRI))
ac <- cf %>% filter(ano>=2010, ano<=2024) %>% count(ano)
pe <- pop %>% group_by(ano) %>% summarise(p=sum(pop), .groups="drop")
ac <- left_join(ac, pe, by="ano") %>% mutate(inc=n/p*1e5)
mk <- suppressWarnings(cor.test(ac$ano, ac$inc, method="kendall"))
cat("\nTENDENCIA ANUAL: casos", ac$n[ac$ano==2010], "(2010) ->", max(ac$n), "(", ac$ano[which.max(ac$n)], ")",
    "| inc", round(ac$inc[ac$ano==2010],1), "->", round(max(ac$inc),1), "\n")
cat("  Mann-Kendall (ano x incidencia): tau =", round(mk$estimate,2), "| p =", signif(mk$p.value,3), "\n")

# ---- Sazonalidade ----
cf$mes <- month(as.Date(cf$DT_SIN_PRI))
verao <- mean(cf$mes %in% 1:3, na.rm=TRUE)
cat("SAZONALIDADE: jan-mar =", pct(verao), "| pico mes", which.max(table(cf$mes)),
    "| menor mes", which.min(table(cf$mes)), "\n")

# -----------------------------------------------------------------------------
# [bloco: 13_tabelas.R]
# -----------------------------------------------------------------------------
# =============================================================================
# TABELAS 1 e 2 do artigo (reprodutiveis)
#  T1: casos, incidencia acumulada e Kendall (precip; semanal e 4 SE) por mesorregiao
#  T2: IRR (Binomial Negativa, precip+temp) e Moran bivariado (incidencia x covariavel)
# Saidas: Bancos_rds/tabela1.csv, Bancos_rds/tabela2.csv
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(MASS); library(sf); library(spdep)})
set.seed(1)  # reprodutibilidade do Moran por permutacao

panel <- readRDS("Bancos_rds/dados_temporais.rds")
popm  <- readRDS("Bancos_rds/pop_mesorregiao_2010_2024.rds")
st <- function(p) ifelse(is.na(p), "", ifelse(p<0.001,"***", ifelse(p<0.01,"**", ifelse(p<0.05,"*","ns"))))
ktau <- function(x,y){ ok<-is.finite(x)&is.finite(y); x<-x[ok]; y<-y[ok]
  if(length(x)<10) return(c(NA,NA)); r<-suppressWarnings(cor.test(x,y,method="kendall")); c(unname(r$estimate),r$p.value) }

# ---- TABELA 1 ----
pm <- popm |> group_by(name_meso) |> summarise(pop=mean(pop), .groups="drop")
t1 <- panel |> group_by(name_meso) |> group_modify(function(d,...){
  a<-ktau(d$inc_conf,d$pr_l2); b<-ktau(d$r_inc_conf,d$r_pr_l2)
  tibble(Casos=sum(d$conf), Notif=sum(d$noti), tauP=a[1],pP=a[2], tauP4=b[1],pP4=b[2]) }) |> ungroup() |>
  left_join(pm, by="name_meso") |>
  transmute(Mesorregiao=name_meso, Casos,                                   # confirmados (co-primario)
            `Incidencia_100mil`=round(Casos/pop*1e5,1),
            Notificados=Notif,                                             # notificados (co-primario)
            `Incidencia_notif_100mil`=round(Notif/pop*1e5,1),
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
labs <- c(precip="Precipitação", temp="Temperatura", rh="Umidade relativa", rs="Radiação solar",
          u2="Velocidade do vento", eto="Evapotranspiração", logdens="Densidade demográfica (log)",
          ivs="IVS", esgoto="Esgoto", agua="Água", lixo="Lixo")
mb  <- sapply(names(labs), function(v){ r<-bim(muni$inc_conf, muni[[v]]); sprintf("%.2f%s", r[1], st(r[2])) })
mbn <- sapply(names(labs), function(v){ r<-bim(muni$inc_noti, muni[[v]]); sprintf("%.2f%s", r[1], st(r[2])) })  # notificados (co-primario)
irrc <- rep(NA_character_, length(labs)); irrn <- rep(NA_character_, length(labs))  # IRR so para clima precip/temp
irrc[1] <- irr(mf,"pr_z"); irrc[2] <- irr(mf,"t_z")
irrn[1] <- irr(mc,"pr_z"); irrn[2] <- irr(mc,"t_z")

t2 <- data.frame(
  Variavel = unname(labs),
  IRR_confirmados = irrc,
  IRR_notificados = irrn,
  Moran_confirmados = unname(mb),
  Moran_notificados = unname(mbn))
write.csv(t2, "Bancos_rds/tabela2.csv", row.names=FALSE)
cat("\n=== TABELA 2 ===\n"); print(t2)

# ---- TABELA SUPLEMENTAR: modelo de erro espacial por especificacao (CRITICOS 1 e 2) ----
# Reune as saidas dos scripts 19 (log x bruta) e 20 (+ taxa de confirmacao; notificados).
ac <- tryCatch(readRDS("Bancos_rds/analises_complementares.rds"),  error=function(e) NULL)
cc <- tryCatch(readRDS("Bancos_rds/confirmacao_covariavel.rds"),   error=function(e) NULL)
if (!is.null(ac) && !is.null(cc)) {
  fb <- function(cf,v) if (v %in% rownames(cf)) sprintf("%+.3f (%.3f)%s", cf[v,1], cf[v,2], st(cf[v,4])) else "—"
  vars <- c(precip_z="Precipitação", temp_z="Temperatura", ivs_z="IVS",
            dens_z="Densidade (log)", pconf_z="Taxa de confirmação")
  ts <- data.frame(
    Variavel = unname(vars),
    `Confirmados log (primário)`  = sapply(names(vars), function(v) fb(ac$sem_log, v)),
    `Confirmados escala bruta`    = sapply(names(vars), function(v) fb(ac$sem_raw, v)),
    `Confirmados log + confirm.`  = sapply(names(vars), function(v) fb(cc$sem1,    v)),
    `Notificados log + confirm.`  = sapply(names(vars), function(v) fb(cc$semN,    v)),
    check.names = FALSE)
  write.csv(ts, "Bancos_rds/tabela_sensibilidade_espacial.csv", row.names=FALSE)
  cat("\n=== TABELA SUPLEMENTAR (erro espacial, coef. por DP; EP entre parenteses) ===\n"); print(ts)
  cat("Moran global da incidencia: confirmados", cc$moran["conf"], "| notificados", cc$moran["noti"],
      "| confirmados ajust. p/ confirmacao", cc$moran["conf_adj"], "\n")
}

# -----------------------------------------------------------------------------
# [bloco: 14_gerar_docx.R]
# -----------------------------------------------------------------------------
# =============================================================================
# GERA O MANUSCRITO EM .DOCX (Times New Roman 12, espacamento simples)
#   Le ARTIGO_REVISADO.md, converte em .docx, insere Tabelas 1-2 (flextable) e
#   embute as Figuras 1-3. Citacoes [n]/[n,m] no corpo sao renderizadas como
#   numeros SOBRESCRITOS (sem colchetes), conforme a norma da CSP.
# Saida: ARTIGO_REVISADO.docx
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("officer","flextable","png")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(officer); library(flextable)})

md <- readLines("ARTIGO_REVISADO.md", warn = FALSE, encoding = "UTF-8")
t1 <- read.csv("Bancos_rds/tabela1.csv", check.names = FALSE, fileEncoding = "UTF-8")
t2 <- read.csv("Bancos_rds/tabela2.csv", check.names = FALSE, fileEncoding = "UTF-8")
t2[is.na(t2)] <- "—"

clean   <- function(s){ s <- gsub("\\*","",s); s <- gsub("`","",s); s <- gsub("\\(a gerar\\)","",s); trimws(s) }
tnr     <- fp_text(font.family = "Times New Roman", font.size = 12)
tnr_sup <- fp_text(font.family = "Times New Roman", font.size = 12, vertical.align = "superscript")
tnr_b   <- fp_text(font.family = "Times New Roman", font.size = 13, bold = TRUE)
tnr_t   <- fp_text(font.family = "Times New Roman", font.size = 14, bold = TRUE)
ftst    <- function(ft) autofit(fontsize(font(ft, fontname = "Times New Roman", part = "all"), size = 9, part = "all"))
figmap  <- c("Figura 1" = "Resultados/Figuras/fig_serie_temporal.png",
             "Figura 2" = "Resultados/Figuras/mapa_incidencia_confirmados.png",
             "Figura 3" = "Resultados/Figuras/mapa_lisa_bivariado_precip.png")

# Paragrafo com citacoes [n]/[n,m] renderizadas em sobrescrito (sem colchetes)
mk_par <- function(txt, base = tnr) {
  txt <- clean(txt)
  m <- gregexpr("\\[[0-9]+(,[0-9]+)*\\]", txt)[[1]]
  if (m[1] == -1) return(fpar(ftext(txt, base)))
  lens <- attr(m, "match.length"); runs <- list(); pos <- 1L
  for (i in seq_along(m)) {
    if (m[i] > pos) runs <- c(runs, list(ftext(substr(txt, pos, m[i] - 1L), base)))
    num  <- gsub("\\[|\\]", "", substr(txt, m[i], m[i] + lens[i] - 1L))
    runs <- c(runs, list(ftext(num, tnr_sup)))
    pos  <- m[i] + lens[i]
  }
  if (pos <= nchar(txt)) runs <- c(runs, list(ftext(substr(txt, pos, nchar(txt)), base)))
  do.call(fpar, runs)
}

doc <- read_docx()
for (l in md) {
  if (trimws(l) == "" || grepl("^---\\s*$", l)) next
  if (grepl("^## ", l))                       doc <- body_add_fpar(doc, fpar(ftext(clean(sub("^#+ ","",l)), tnr_b)))
  else if (grepl("^# ", l))                   doc <- body_add_fpar(doc, fpar(ftext(clean(sub("^#+ ","",l)), tnr_t)))
  else if (grepl("^\\*\\*Tabela 1\\.", l)) { doc <- body_add_fpar(doc, mk_par(l)); doc <- body_add_flextable(doc, ftst(flextable(t1))) }
  else if (grepl("^\\*\\*Tabela 2\\.", l)) { doc <- body_add_fpar(doc, mk_par(l)); doc <- body_add_flextable(doc, ftst(flextable(t2))) }
  else if (grepl("^\\*\\*Figura [123]\\.", l)) {
    doc <- body_add_fpar(doc, mk_par(l))
    fn <- regmatches(l, regexpr("Figura [123]", l))
    if (file.exists(figmap[[fn]])) { d <- dim(png::readPNG(figmap[[fn]]))   # preserva a proporcao
      doc <- body_add_img(doc, figmap[[fn]], width = 6.2, height = round(6.2 * d[1] / d[2], 2)) }
  }
  else doc <- body_add_fpar(doc, mk_par(l))
}
print(doc, target = "ARTIGO_REVISADO.docx")
cat("OK: ARTIGO_REVISADO.docx gerado (citacoes em sobrescrito)\n")
