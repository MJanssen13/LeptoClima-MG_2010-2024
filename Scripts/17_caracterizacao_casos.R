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
