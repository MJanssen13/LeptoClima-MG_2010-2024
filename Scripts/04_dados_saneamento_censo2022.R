# =============================================================================
# BANCO 04 - SANEAMENTO BASICO (Censo 2022, MG, por municipio)
# Fonte (IBGE/SIDRA, dados do universo do Censo 2022):
#   - tab. 6805  Esgotamento sanitario
#   - tab. 6803  Abastecimento de agua
#   - tab. 6892  Destino do lixo
# Indicadores (% de domicilios particulares permanentes ocupados):
#   - esgoto: "Rede geral, rede pluvial ou fossa ligada a rede"
#   - agua:   "Possui ligacao a rede geral e a utiliza como forma principal"
#   - lixo:   "Coletado"
# Saida: saneamento_mg_2022.rds
# Obs.: requer internet (SIDRA).
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("sidrar","dplyr")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(sidrar); library(dplyr)})

# colunas-padrao do retorno do SIDRA (a coluna de classificacao e o complemento)
std <- c("Nível Territorial (Código)","Nível Territorial","Unidade de Medida (Código)",
         "Unidade de Medida","Valor","Município (Código)","Município","Ano (Código)","Ano",
         "Variável (Código)","Variável")

pct_adeq <- function(tab, label) {                # % da categoria 'label' sobre o Total
  d  <- get_sidra(tab, period = "2022", geo = "City", geo.filter = list("State" = 31))
  cl <- setdiff(names(d), std); cl <- cl[!grepl("Código", cl)][1]   # coluna de categoria
  d2 <- data.frame(cod = as.integer(d[["Município (Código)"]]), cat = d[[cl]],
                   val = as.numeric(d[["Valor"]]))
  tot <- d2 |> filter(cat == "Total") |> select(cod, total = val)
  adq <- d2 |> filter(cat == label)   |> select(cod, adq   = val)
  inner_join(tot, adq, by = "cod") |> mutate(pct = round(100 * adq / total, 2)) |> select(cod, pct)
}

san <- pct_adeq(6805, "Rede geral, rede pluvial ou fossa ligada à rede") |> rename(esgoto = pct) |>
  inner_join(pct_adeq(6803, "Possui ligação à rede geral e a utiliza como forma principal") |>
               rename(agua = pct), by = "cod") |>
  inner_join(pct_adeq(6892, "Coletado") |> rename(lixo = pct), by = "cod")
saveRDS(san, "Bancos_rds/saneamento_mg_2022.rds")

cat("OK saneamento:", nrow(san), "municipios | medias MG -> esgoto",
    round(mean(san$esgoto), 1), "% agua", round(mean(san$agua), 1),
    "% lixo", round(mean(san$lixo), 1), "%\n")
