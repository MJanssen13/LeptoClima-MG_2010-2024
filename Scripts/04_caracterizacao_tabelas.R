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
# [bloco: 25_tendencia_notificados.R]
# -----------------------------------------------------------------------------
# =============================================================================
# TENDENCIA ANUAL (MANN-KENDALL): NOTIFICADOS E TAXA DE CONFIRMACAO
#   Estende o Mann-Kendall dos confirmados (acima) aos dois desfechos que o
#   manuscrito tambem reporta: a incidencia NOTIFICADA (desfecho co-primario) e
#   a propria taxa de confirmacao anual (confirmados/notificados) — uma taxa de
#   confirmacao estavel sustenta que a tendencia dos confirmados nao e artefato
#   de melhora do acesso ao diagnostico.
#   Especificacao IDENTICA a do bloco anterior (ano x incidencia estadual,
#   cor.test de Kendall = Mann-Kendall tau-b), acrescida de Kendall::MannKendall
#   (tau-a, estatistica S) e da inclinacao de Theil-Sen com IC95%.
# Entrada: Bancos_rds/base_MG_2010_2024.rds, Bancos_rds/pop_municipal_2010_2024.rds
# Saidas : console + Bancos_rds/tendencia_anual.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("Kendall")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(dplyr); library(lubridate)})
fmtn <- function(x, d = 1) formatC(unname(x), format = "f", digits = d, decimal.mark = ",", big.mark = ".")
pfv  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p))))
pfx  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", pfv(p))))

b   <- readRDS("Bancos_rds/base_MG_2010_2024.rds")
pop <- readRDS("Bancos_rds/pop_municipal_2010_2024.rds")
pe  <- pop %>% group_by(ano) %>% summarise(p = sum(pop), .groups = "drop")
b$ano <- year(as.Date(b$DT_SIN_PRI))
# Serie anual completa (anos sem caso entram com zero, nao ausentes)
serie <- function(sel) b[sel, ] %>% filter(ano >= 2010, ano <= 2024) %>% count(ano) %>%
  right_join(data.frame(ano = 2010:2024), by = "ano") %>%
  mutate(n = ifelse(is.na(n), 0L, n)) %>%
  left_join(pe, by = "ano") %>% mutate(inc = n/p*1e5) %>% arrange(ano)
serC <- serie(b$confirmado %in% TRUE)   # confirmados
serN <- serie(rep(TRUE, nrow(b)))       # notificados (co-primario)

cat("\n=============================================================\n")
cat(" TENDENCIA ANUAL (Mann-Kendall) 2010-2024\n")
cat("=============================================================\n")
print(data.frame(ano = serC$ano, conf = serC$n, inc_conf = round(serC$inc, 2),
                 noti = serN$n, inc_noti = round(serN$inc, 2),
                 pct_conf = round(100*serC$n/serN$n, 1), pop = round(serN$p)))

# tau-b (cor.test, como no bloco anterior) + tau-a/S (Kendall) + Theil-Sen
mkend <- function(x, y, rot, un = "unid./ano") {
  k <- suppressWarnings(cor.test(x, y, method = "kendall"))
  cat(sprintf("\n  %s\n", rot))
  cat(sprintf("    Mann-Kendall (cor.test kendall, tau-b): tau = %s | %s | T = %s\n",
              fmtn(unname(k$estimate), 2), pfx(k$p.value), format(unname(k$statistic))))
  mkk <- Kendall::MannKendall(y)
  cat(sprintf("    Kendall::MannKendall           : tau = %s | %s | S = %s\n",
              fmtn(unname(mkk$tau), 2), pfx(unname(mkk$sl)), format(unname(mkk$S))))
  # Theil-Sen com IC95% classico (baseado na variancia de S de Kendall)
  nn <- length(x); sl <- c()
  for (i in 1:(nn-1)) for (j in (i+1):nn) sl <- c(sl, (y[j]-y[i])/(x[j]-x[i]))
  sl <- sort(sl); Np <- length(sl)
  Cq <- qnorm(0.975) * sqrt(nn*(nn-1)*(2*nn+5)/18)
  lo <- sl[max(1, floor((Np - Cq)/2))]; hi <- sl[min(Np, ceiling((Np + Cq)/2) + 1)]
  cat(sprintf("    Theil-Sen: %s %s [IC95%% %s; %s]\n", fmtn(median(sl), 3), un, fmtn(lo, 3), fmtn(hi, 3)))
  c(tau = unname(k$estimate), p = k$p.value, sen = median(sl))
}
rC <- mkend(serC$ano, serC$inc, "INCIDENCIA CONFIRMADA (casos/100 mil hab.)", "por 100 mil hab./ano")
rN <- mkend(serN$ano, serN$inc, "INCIDENCIA NOTIFICADA (casos/100 mil hab.)", "por 100 mil hab./ano")
cat("\n  [sensibilidade: contagens absolutas, sem denominador populacional]\n")
rCc <- mkend(serC$ano, as.numeric(serC$n), "CASOS CONFIRMADOS (contagem)", "casos/ano")
rNc <- mkend(serN$ano, as.numeric(serN$n), "CASOS NOTIFICADOS (contagem)", "casos/ano")
cat("\n  [tendencia da propria taxa de confirmacao C/N]\n")
rP <- mkend(serC$ano, 100*serC$n/serN$n, "TAXA DE CONFIRMACAO ANUAL (%)", "pontos percentuais/ano")

cat("\n--- Sintese ---\n")
cat(sprintf("  Confirmados: incidencia %s -> %s por 100 mil (2010 -> 2024); pico %s em %d\n",
            fmtn(serC$inc[1], 1), fmtn(serC$inc[15], 1), fmtn(max(serC$inc), 1), serC$ano[which.max(serC$inc)]))
cat(sprintf("  Notificados: incidencia %s -> %s por 100 mil (2010 -> 2024); pico %s em %d\n",
            fmtn(serN$inc[1], 1), fmtn(serN$inc[15], 1), fmtn(max(serN$inc), 1), serN$ano[which.max(serN$inc)]))
cat(sprintf("  Taxa de confirmacao: %s%% (2010) -> %s%% (2024)\n",
            fmtn(100*serC$n[1]/serN$n[1], 1), fmtn(100*serC$n[15]/serN$n[15], 1)))
cat(sprintf("  tau confirmados = %s (%s) | tau notificados = %s (%s) | tau taxa de confirmacao = %s (%s)\n",
            fmtn(rC["tau"], 2), pfx(rC["p"]), fmtn(rN["tau"], 2), pfx(rN["p"]),
            fmtn(rP["tau"], 2), pfx(rP["p"])))
saveRDS(list(serie_conf = serC, serie_noti = serN, mk_inc_conf = rC, mk_inc_noti = rN,
             mk_casos_conf = rCc, mk_casos_noti = rNc, mk_taxa_conf = rP),
        "Bancos_rds/tendencia_anual.rds")
cat("\nTEND_OK\n")

# -----------------------------------------------------------------------------
# [bloco: 26_ocupacao.R]
# -----------------------------------------------------------------------------
# =============================================================================
# OCUPACAO E EXPOSICAO AGROPECUARIA (casos confirmados, MG 2010-2024)
#   Qualifica o perfil de exposicao que o desenho ecologico nao mede, e mostra
#   que o preenchimento do campo de ocupacao e ele proprio um confundidor
#   regional (acompanha o gradiente de incidencia).
#   (a) ID_OCUPA_N (CBO): preenchimento efetivo e grande grupo (1o digito);
#   (b) ANT_CB_CRI (criacao de animais) e ANT_CB_PLA (plantio/colheita);
#   (c) estratificacao por mesorregiao e contraste ALTA vs BAIXA incidencia;
#   (d) nao-preenchimento por mesorregiao (o confundidor);
#   (e) RMBH vs interior, heterogeneidade e gradiente continuo (Spearman).
#   Proporcoes com IC95% de Wilson; diferencas com IC95% hibrido de Newcombe;
#   comparacoes 2x2 por qui-quadrado de Pearson (ou exato de Fisher se algum
#   esperado < 5) e heterogeneidade entre as 12 mesorregioes por qui-quadrado
#   com conferencia por Fisher de Monte Carlo (B = 20.000, semente 1).
# Entrada: Bancos_rds/base_MG_2010_2024.rds, Bancos_rds/pop_mesorregiao_2010_2024.rds
# Saidas : console + Bancos_rds/ocupacao_exposicao.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr)})
set.seed(1)                                  # Fisher de Monte Carlo (reprodutibilidade)
w0 <- getOption("width"); options(width = 200)   # tabelas largas; largura restaurada ao fim
fmtn <- function(x, d = 1) formatC(unname(x), format = "f", digits = d, decimal.mark = ",", big.mark = ".")
pfv  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p))))
pfx  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", pfv(p))))
ic95 <- function(lo, hi, d = 2) sprintf("IC95%% %s; %s", fmtn(lo, d), fmtn(hi, d))
pc   <- function(x, d = 1) paste0(fmtn(100 * x, d), "%")
# IC95% de Wilson (escore) para uma proporcao
wilson <- function(x, n, conf = 0.95) {
  if (n == 0) return(c(NA, NA))
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n; d <- 1 + z^2 / n
  c((p + z^2/(2*n) - z*sqrt(p*(1-p)/n + z^2/(4*n^2))) / d,
    (p + z^2/(2*n) + z*sqrt(p*(1-p)/n + z^2/(4*n^2))) / d)
}
# IC95% hibrido de Newcombe (1998) para a DIFERENCA de duas proporcoes
newcombe <- function(x1, n1, x2, n2, conf = 0.95) {
  p1 <- x1/n1; p2 <- x2/n2; l1 <- wilson(x1, n1, conf); l2 <- wilson(x2, n2, conf)
  c(dif = p1 - p2,
    lo  = (p1 - p2) - sqrt((p1 - l1[1])^2 + (l2[2] - p2)^2),
    hi  = (p1 - p2) + sqrt((l1[2] - p1)^2 + (p2 - l2[1])^2))
}
# 2x2: qui-quadrado sem correcao se todos os esperados >= 5; senao Fisher exato
teste2x2 <- function(x1, n1, x2, n2) {
  M <- matrix(c(x1, n1 - x1, x2, n2 - x2), nrow = 2)
  E <- outer(rowSums(M), colSums(M)) / sum(M)
  if (all(E >= 5)) { tt <- suppressWarnings(chisq.test(M, correct = FALSE))
    list(teste = "qui-quadrado de Pearson", est = unname(tt$statistic), gl = unname(tt$parameter),
         p = tt$p.value, minE = min(E))
  } else { tt <- fisher.test(M)
    list(teste = "exato de Fisher", est = NA, gl = NA, p = tt$p.value, minE = min(E)) }
}
ALTA  <- c("Sul/Sudoeste de Minas", "Zona da Mata")                                    # maior incidencia
BAIXA <- c("Norte de Minas", "Jequitinhonha", "Vale do Mucuri", "Noroeste de Minas")   # menor incidencia

b  <- readRDS("Bancos_rds/base_MG_2010_2024.rds")
cf <- b[b$confirmado %in% TRUE, ]
N  <- nrow(cf)
cat("\n=============================================================\n")
cat(" OCUPACAO E EXPOSICAO AGROPECUARIA | confirmados n =", N, "\n")
cat("=============================================================\n")

# ---------------------------------------------------------------- (a) CBO ----
GG <- c("0" = "Forças armadas, policiais e bombeiros militares",
        "1" = "Dirigentes e gerentes",
        "2" = "Profissionais das ciências e das artes",
        "3" = "Técnicos de nível médio",
        "4" = "Trabalhadores de serviços administrativos",
        "5" = "Trabalhadores dos serviços e vendedores do comércio",
        "6" = "Trabalhadores agropecuários, florestais e da pesca",
        "7" = "Trabalhadores da produção de bens e serviços industriais",
        "8" = "Trabalhadores da produção de bens e serviços industriais",
        "9" = "Trabalhadores de manutenção e reparação")
ESPECIAIS <- c("999991", "999992", "999993", "999994", "999995", "998999")  # nao-ocupacionais

o <- cf$ID_OCUPA_N
o[is.na(o)] <- ""
cf$cbo_preenchido <- o != "" & o != "XXX"              # criterio "preenchido" bruto
cf$cbo_valido     <- cf$cbo_preenchido & nchar(o) == 6 & o != "000000" & !(o %in% ESPECIAIS)
cf$cbo_gg         <- ifelse(cf$cbo_valido, substr(o, 1, 1), NA)
cf$agro           <- ifelse(cf$cbo_valido, cf$cbo_gg == "6", NA)

cat("\n--- (a) Preenchimento de ID_OCUPA_N ---\n")
cat(sprintf("  Registros com algum conteudo (incl. 'XXX')  : %d (%s)\n", sum(o != ""), pc(mean(o != ""))))
cat(sprintf("  'XXX' (placeholder invalido)                : %d\n", sum(o == "XXX")))
cat(sprintf("  '000000' (nao informado)                    : %d\n", sum(o == "000000")))
cat(sprintf("  Codigos especiais nao-ocupacionais          : %d\n", sum(o %in% ESPECIAIS)))
print(table(o[o %in% ESPECIAIS]))
cat(sprintf("  ==> COM OCUPACAO EFETIVA (CBO valida)       : %d (%s de %d)\n\n",
            sum(cf$cbo_valido), pc(mean(cf$cbo_valido)), N))

cat("--- (a) Distribuicao por grande grupo CBO (denominador = CBO valida) ---\n")
tg <- as.data.frame(table(cf$cbo_gg)); names(tg) <- c("gg", "n")
tg$pct <- 100 * tg$n / sum(tg$n); tg$grupo <- GG[as.character(tg$gg)]
tg <- tg[order(-tg$n), ]
for (i in seq_len(nrow(tg)))
  cat(sprintf("  %s  %-58s %4d  %6s\n", tg$gg[i], substr(tg$grupo[i], 1, 58), tg$n[i], pc(tg$pct[i]/100)))
cat(sprintf("  TOTAL %66d\n", sum(tg$n)))
n6 <- sum(cf$agro, na.rm = TRUE); nv <- sum(cf$cbo_valido); w6 <- wilson(n6, nv)
cat(sprintf("\n  GRUPO 6 (agropecuarios): %d/%d = %s  [%s]\n", n6, nv, pc(n6/nv), ic95(100*w6[1], 100*w6[2], 1)))
cat(sprintf("  (sobre TODOS os preenchidos, incl. especiais: %d/%d = %s)\n",
            n6, sum(cf$cbo_preenchido), pc(n6/sum(cf$cbo_preenchido))))
cat(sprintf("  (sobre TODOS os confirmados n=%d: %s)\n", N, pc(n6/N)))
cat("\n  Codigos CBO mais frequentes dentro do grupo 6:\n")
print(head(sort(table(o[cf$cbo_valido & cf$cbo_gg == "6"]), decreasing = TRUE), 8))

# ------------------------------------------------------- (b) ANT_CB_CRI/PLA --
sn <- function(v) { x <- cf[[v]]; x[is.na(x)] <- ""; ifelse(x == "1", TRUE, ifelse(x == "2", FALSE, NA)) }
cf$cri <- sn("ANT_CB_CRI"); cf$pla <- sn("ANT_CB_PLA")
cat("\n--- (b) Antecedentes de exposicao (sim/nao; 9 e branco = nao preenchido) ---\n")
for (v in c("cri", "pla")) {
  x <- cf[[v]]; k <- sum(x, na.rm = TRUE); nn <- sum(!is.na(x)); w <- wilson(k, nn)
  cat(sprintf("  %-28s sim %d/%d = %s [%s] | preenchimento %d/%d = %s\n",
              ifelse(v == "cri", "ANT_CB_CRI (criacao)", "ANT_CB_PLA (plantio/colheita)"),
              k, nn, pc(k/nn), ic95(100*w[1], 100*w[2], 1), nn, N, pc(nn/N)))
}
cf$agroexp <- ifelse(is.na(cf$cri) & is.na(cf$pla), NA, (cf$cri %in% TRUE) | (cf$pla %in% TRUE))
k <- sum(cf$agroexp, na.rm = TRUE); nn <- sum(!is.na(cf$agroexp)); w <- wilson(k, nn)
cat(sprintf("  %-28s sim %d/%d = %s [%s]\n", "CRI ou PLA (composto)", k, nn, pc(k/nn), ic95(100*w[1], 100*w[2], 1)))

# ------------------------------------------------- (c) por mesorregiao -------
cf$grp <- ifelse(cf$name_meso %in% ALTA, "ALTA", ifelse(cf$name_meso %in% BAIXA, "BAIXA", "outras"))
tab_meso <- cf %>% group_by(name_meso) %>% summarise(
  conf      = n(),
  cbo_pre   = sum(cbo_preenchido), cbo_pre_p = 100*mean(cbo_preenchido),
  cbo_val   = sum(cbo_valido),     cbo_val_p = 100*mean(cbo_valido),
  agro_n    = sum(agro, na.rm = TRUE), agro_d = sum(!is.na(agro)),
  agro_p    = 100*agro_n/pmax(agro_d, 1),
  cri_n     = sum(cri, na.rm = TRUE), cri_d = sum(!is.na(cri)), cri_p = 100*cri_n/pmax(cri_d, 1),
  pla_n     = sum(pla, na.rm = TRUE), pla_d = sum(!is.na(pla)), pla_p = 100*pla_n/pmax(pla_d, 1),
  cri_pre_p = 100*mean(!is.na(cri)), pla_pre_p = 100*mean(!is.na(pla)),
  .groups = "drop") %>% arrange(desc(conf))
cat("\n--- (c) Por mesorregiao (confirmados) ---\n")
print(as.data.frame(tab_meso %>% mutate(across(where(is.numeric), ~round(.x, 1)))))

cat("\n--- (c) ALTA (Sul/Sudoeste + Zona da Mata) vs BAIXA (Norte, Jequitinhonha, Mucuri, Noroeste) ---\n")
comp <- function(varn, rotulo) {
  a <- cf[cf$grp == "ALTA", ]; bb <- cf[cf$grp == "BAIXA", ]
  x1 <- sum(a[[varn]],  na.rm = TRUE); n1 <- sum(!is.na(a[[varn]]))
  x2 <- sum(bb[[varn]], na.rm = TRUE); n2 <- sum(!is.na(bb[[varn]]))
  d  <- newcombe(x1, n1, x2, n2); tt <- teste2x2(x1, n1, x2, n2)
  rp <- (x1/n1)/(x2/n2)                                  # razao de prevalencias
  se <- sqrt(1/x1 - 1/n1 + 1/x2 - 1/n2)
  cat(sprintf("\n  [%s]\n", rotulo))
  cat(sprintf("    ALTA : %d/%d = %s\n", x1, n1, pc(x1/n1)))
  cat(sprintf("    BAIXA: %d/%d = %s\n", x2, n2, pc(x2/n2)))
  cat(sprintf("    diferenca (ALTA-BAIXA) = %s pontos percentuais [IC95%% %s; %s] (Newcombe)\n",
              fmtn(100*d["dif"], 1), fmtn(100*d["lo"], 1), fmtn(100*d["hi"], 1)))
  cat(sprintf("    RP = %s [IC95%% %s; %s]\n", fmtn(rp, 2),
              fmtn(exp(log(rp) - 1.96*se), 2), fmtn(exp(log(rp) + 1.96*se), 2)))
  cat(sprintf("    teste: %s%s, %s\n", tt$teste,
              ifelse(is.na(tt$est), "", sprintf(" (X2 = %s, gl = %d)", fmtn(tt$est, 2), tt$gl)), pfx(tt$p)))
  invisible(list(x1 = x1, n1 = n1, x2 = x2, n2 = n2, d = d, p = tt$p, teste = tt$teste))
}
r_agro <- comp("agro",    "CBO grande grupo 6 (trabalhador agropecuario) | denominador = CBO valida")
r_cri  <- comp("cri",     "ANT_CB_CRI = sim (contato com criacao de animais)")
r_pla  <- comp("pla",     "ANT_CB_PLA = sim (plantio/colheita)")
r_comp <- comp("agroexp", "CRI ou PLA (exposicao agropecuaria composta)")

# ------------------------------------------------- (d) nao-preenchimento -----
cat("\n--- (d) NAO-PREENCHIMENTO por mesorregiao (confundidor potencial) ---\n")
nao <- tab_meso %>% transmute(name_meso, conf,
        `%CBO nao preenchida`      = round(100 - cbo_pre_p, 1),
        `%sem CBO ocupacional`     = round(100 - cbo_val_p, 1),
        `%ANT_CB_CRI nao preench.` = round(100 - cri_pre_p, 1),
        `%ANT_CB_PLA nao preench.` = round(100 - pla_pre_p, 1)) %>% arrange(desc(conf))
print(as.data.frame(nao))

cat("\n  Heterogeneidade do preenchimento entre as 12 mesorregioes:\n")
het <- function(flag, rot) {
  M  <- table(cf$name_meso, cf[[flag]])
  tt <- suppressWarnings(chisq.test(M))
  cat(sprintf("    %-24s qui-quadrado gl=%d, X2=%s, %s (min esperado %s)\n",
              rot, tt$parameter, fmtn(tt$statistic, 1), pfx(tt$p.value), fmtn(min(tt$expected), 1)))
  ff <- fisher.test(M, simulate.p.value = TRUE, B = 20000)
  cat(sprintf("      Fisher (Monte Carlo, B=20.000): %s\n", pfx(ff$p.value)))
}
cf$cri_pre <- !is.na(cf$cri); cf$pla_pre <- !is.na(cf$pla)
het("cbo_preenchido", "CBO preenchida");  het("cbo_valido", "CBO ocupacional valida")
het("cri_pre", "ANT_CB_CRI preenchido");  het("pla_pre", "ANT_CB_PLA preenchido")

cat("\n  ALTA vs BAIXA - PREENCHIMENTO (o confundidor):\n")
for (fl in c("cbo_preenchido", "cbo_valido", "cri_pre", "pla_pre")) {
  a <- cf[[fl]][cf$grp == "ALTA"]; bb <- cf[[fl]][cf$grp == "BAIXA"]
  x1 <- sum(a); n1 <- length(a); x2 <- sum(bb); n2 <- length(bb)
  d <- newcombe(x1, n1, x2, n2); tt <- teste2x2(x1, n1, x2, n2)
  cat(sprintf("    %-16s ALTA %s (%d/%d) | BAIXA %s (%d/%d) | dif %s pp [%s; %s] | %s, %s\n",
      fl, pc(x1/n1), x1, n1, pc(x2/n2), x2, n2, fmtn(100*d["dif"], 1),
      fmtn(100*d["lo"], 1), fmtn(100*d["hi"], 1), tt$teste, pfx(tt$p)))
}

# ------------------------------------------------- (e) contrastes adicionais -
cat("\n--- (e) O contraste que EXISTE: Metropolitana de BH vs interior ---\n")
cf$rm <- ifelse(cf$name_meso == "Metropolitana de Belo Horizonte", "RMBH", "Interior")
comp2 <- function(varn, rotulo) {
  a <- cf[cf$rm == "Interior", ]; bb <- cf[cf$rm == "RMBH", ]
  x1 <- sum(a[[varn]],  na.rm = TRUE); n1 <- sum(!is.na(a[[varn]]))
  x2 <- sum(bb[[varn]], na.rm = TRUE); n2 <- sum(!is.na(bb[[varn]]))
  d <- newcombe(x1, n1, x2, n2); tt <- teste2x2(x1, n1, x2, n2)
  cat(sprintf("    %-38s Interior %s (%d/%d) | RMBH %s (%d/%d) | dif %s pp [%s; %s] | %s, %s\n",
      rotulo, pc(x1/n1), x1, n1, pc(x2/n2), x2, n2, fmtn(100*d["dif"], 1),
      fmtn(100*d["lo"], 1), fmtn(100*d["hi"], 1), tt$teste, pfx(tt$p)))
}
comp2("agro", "CBO grupo 6"); comp2("cri", "ANT_CB_CRI"); comp2("pla", "ANT_CB_PLA"); comp2("agroexp", "CRI ou PLA")

cat("\n--- (e) Heterogeneidade da exposicao agropecuaria entre as 12 mesorregioes ---\n")
for (v in c("agro", "cri", "pla", "agroexp")) {
  M  <- table(cf$name_meso[!is.na(cf[[v]])], cf[[v]][!is.na(cf[[v]])])
  tt <- suppressWarnings(chisq.test(M))
  ff <- fisher.test(M, simulate.p.value = TRUE, B = 20000)
  cat(sprintf("    %-8s X2=%s gl=%d %s | Fisher MC %s\n", v, fmtn(tt$statistic, 1), tt$parameter,
              pfx(tt$p.value), pfx(ff$p.value)))
}

cat("\n--- (e) Gradiente continuo: incidencia confirmada x %exposicao (12 mesorregioes) ---\n")
popm <- readRDS("Bancos_rds/pop_mesorregiao_2010_2024.rds") %>%
  group_by(name_meso) %>% summarise(pop = mean(pop), .groups = "drop")
inc <- b %>% group_by(name_meso) %>%
  summarise(conf = sum(confirmado %in% TRUE), noti = n(), .groups = "drop") %>%
  filter(!is.na(name_meso)) %>% left_join(popm, by = "name_meso") %>%
  mutate(inc_conf = conf/pop*1e5, inc_noti = noti/pop*1e5)
g <- left_join(inc, tab_meso, by = "name_meso")
print(as.data.frame(g %>% transmute(name_meso, inc_conf = round(inc_conf, 1), inc_noti = round(inc_noti, 1),
        agro_p = round(agro_p, 1), agro_d, cri_p = round(cri_p, 1), pla_p = round(pla_p, 1),
        cbo_pre_p = round(cbo_pre_p, 1)) %>% arrange(desc(inc_conf))))
spear <- list()
for (v in c("agro_p", "cri_p", "pla_p", "cbo_pre_p")) {
  s <- suppressWarnings(cor.test(g$inc_conf, g[[v]], method = "spearman"))
  spear[[v]] <- c(rho = unname(s$estimate), p = s$p.value)
  cat(sprintf("    Spearman(inc_conf, %-10s) rho = %s, %s\n", v, fmtn(unname(s$estimate), 2), pfx(s$p.value)))
}

cat("\n--- Sanidade: n por grupo ---\n")
print(cf %>% count(grp))
saveRDS(list(tab_meso = tab_meso, grupos_cbo = tg, spearman = spear,
             alta_baixa = list(agro = r_agro, cri = r_cri, pla = r_pla, composto = r_comp)),
        "Bancos_rds/ocupacao_exposicao.rds")
options(width = w0)   # restaura a largura de impressao
cat("\nOCUP_OK\n")

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
# Formatacao no padrao da RESS: valor-p exato com 3 casas ("<0,001" quando p<0,001),
# virgula decimal e ponto como separador de milhar (substitui os antigos asteriscos).
pf  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0,001", sub("\\.", ",", sprintf("%.3f", p))))
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = ".")
int <- function(x) formatC(as.integer(x), format = "d", big.mark = ".")
pfx <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", pf(p))))  # evita "p=<0,001"
ktau <- function(x,y){ ok<-is.finite(x)&is.finite(y); x<-x[ok]; y<-y[ok]
  if(length(x)<10) return(c(NA,NA)); r<-suppressWarnings(cor.test(x,y,method="kendall")); c(unname(r$estimate),r$p.value) }

# ---- TABELA 1 ----
pm <- popm |> group_by(name_meso) |> summarise(pop=mean(pop), .groups="drop")
t1 <- panel |> group_by(name_meso) |> group_modify(function(d,...){
  a<-ktau(d$inc_conf,d$pr_l2); b<-ktau(d$r_inc_conf,d$r_pr_l2)
  tibble(Casos=sum(d$conf), Notif=sum(d$noti), tauP=a[1],pP=a[2], tauP4=b[1],pP4=b[2]) }) |> ungroup() |>
  left_join(pm, by="name_meso") |>
  mutate(inc_conf_num = Casos/pop*1e5) |>
  # Contagens ficam SEM separador de milhar no CSV: "1.691" seria lido como 1,691
  # decimal por leitores de CSV. O separador e formatacao de exibicao e pertence
  # a tabela do manuscrito, nao ao artefato de dados.
  transmute(Mesorregiao=name_meso, Casos,                                   # confirmados (co-primario)
            `Incidencia_100mil`=fmt(inc_conf_num, 1),
            Notificados=Notif,                                              # notificados (co-primario)
            `Incidencia_notif_100mil`=fmt(Notif/pop*1e5, 1),
            `Kendall_semanal`=paste0(fmt(tauP), " (", pf(pP), ")"),         # tau (valor-p exato)
            `Kendall_4SE`=paste0(fmt(tauP4), " (", pf(pP4), ")"),
            ord = inc_conf_num) |>
  arrange(desc(ord)) |> dplyr::select(-ord)   # dplyr:: explicito (MASS mascara select)
write.csv(t1, "Bancos_rds/tabela1.csv", row.names=FALSE)
cat("=== TABELA 1 ===\n"); print(as.data.frame(t1))

# ---- TABELA 2A: regressao Binomial Negativa (IRR por desvio-padrao) ----
d <- panel |> filter(!is.na(pr_l2),!is.na(t_l2),pop>0) |>
  mutate(pr_z=as.numeric(scale(pr_l2)), t_z=as.numeric(scale(t_l2)), meso=factor(name_meso), yr=ano-2017)
irr <- function(m,v){ s<-summary(m)$coefficients[v,]                       # IRR (IC95%), sem asteriscos
  sprintf("%s (%s; %s)", fmt(exp(s[1])), fmt(exp(s[1]-1.96*s[2])), fmt(exp(s[1]+1.96*s[2]))) }
irr_p <- function(m,v) pf(summary(m)$coefficients[v,4])                    # p exato p/ a nota de rodape
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
mb_r  <- sapply(names(labs), function(v) bim(muni$inc_conf, muni[[v]]))    # linha 1 = I ; linha 2 = p
mbn_r <- sapply(names(labs), function(v) bim(muni$inc_noti, muni[[v]]))    # notificados (co-primario)
mb  <- fmt(mb_r[1, ]);  mbn <- fmt(mbn_r[1, ])
mb_p <- pf(mb_r[2, ]);  mbn_p <- pf(mbn_r[2, ])                            # p exatos p/ a nota de rodape
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

# Nota de rodape da Tabela 2 (a RESS exige p exato; as celulas ficam sem asteriscos)
cat("\n--- nota de rodape da Tabela 2 (valores-p exatos) ---\n")
cat("IRR: precipitacao ", pfx(summary(mf)$coefficients["pr_z",4]), " / ", pfx(summary(mc)$coefficients["pr_z",4]),
    " | temperatura ", pfx(summary(mf)$coefficients["t_z",4]), " / ", pfx(summary(mc)$coefficients["t_z",4]),
    " (confirmados/notificados)\n", sep="")
for (i in seq_along(labs))
  cat(sprintf("  Moran %-28s confirmados %-9s notificados %s\n",
              labs[i], pfx(mb_r[2, i]), pfx(mbn_r[2, i])))

# ---- TABELA SUPLEMENTAR: modelo de erro espacial por especificacao (CRITICOS 1 e 2) ----
# Reune as saidas dos scripts 19 (log x bruta) e 20 (+ taxa de confirmacao; notificados).
ac <- tryCatch(readRDS("Bancos_rds/analises_complementares.rds"),  error=function(e) NULL)
cc <- tryCatch(readRDS("Bancos_rds/confirmacao_covariavel.rds"),   error=function(e) NULL)
if (!is.null(ac) && !is.null(cc)) {
  fb <- function(cf,v) if (v %in% rownames(cf))
    paste0(sub("\\.", ",", sprintf("%+.3f", cf[v,1])), " (",
           sub("\\.", ",", sprintf("%.3f", cf[v,2])), "); ", pfx(cf[v,4])) else "—"
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
#   numeros SOBRESCRITOS (sem colchetes), formatacao anterior; a submissao atual
#   usa Vancouver entre colchetes, mantida direto no .docx do manuscrito.
# Saida: ARTIGO_REVISADO.docx
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
# O texto-fonte do manuscrito (ARTIGO_REVISADO.md) nao e versionado: e material
# de submissao, mantido fora do repositorio. Sem ele nao ha o que converter, e o
# bloco apenas avisa, para o script rodar de ponta a ponta a partir da raiz.
if (!file.exists("ARTIGO_REVISADO.md")) {
  cat("AVISO: ARTIGO_REVISADO.md ausente (nao versionado); .docx nao gerado.\n")
} else {
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
}
