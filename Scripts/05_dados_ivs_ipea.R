# =============================================================================
# BANCO 05 - IVS: INDICE DE VULNERABILIDADE SOCIAL (Atlas IPEA, MG, 2010)
# Entrada: dados_pop/atlasivs_dadosbrutos_pt_v2 - MG.xlsx
#   (Base Completa do Atlas da Vulnerabilidade Social - ivs.ipea.gov.br,
#    ja filtrada para Minas Gerais; aba "Dados brutos IVS")
# Tratamento:
#   - Recorte do nivel MUNICIPIO (nivel == "regiao,uf,rm,municipio"), ano 2010,
#     e do TOTAL geral (cor/sexo/situacao de domicilio = Total)
#   - IVS e 3 subindices: Infraestrutura Urbana, Capital Humano, Renda e Trabalho
#   - Combina com saneamento (banco 04) -> camada municipal de vulnerabilidade
# Saidas: ivs_mg_2010.rds, vulnerabilidade_mg.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("readxl","dplyr")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(readxl); library(dplyr)})

f <- "Dados_brutos/dados_pop/atlasivs_dadosbrutos_pt_v2 - MG.xlsx"
d <- suppressMessages(read_excel(f, sheet = 1, col_types = "text"))   # tudo texto (codigos/decimais)
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)))))

ivs <- d |>
  filter(nivel == "regiao,uf,rm,municipio", ano == "2010",
         label_cor == "Total Cor", label_sexo == "Total Sexo",
         label_sit_dom == "Total Situação de Domicílio") |>
  transmute(cod    = as.integer(municipio),
            ivs    = num(ivs),
            ivs_iu = num(ivs_infraestrutura_urbana),   # Infraestrutura Urbana
            ivs_ch = num(ivs_capital_humano),          # Capital Humano
            ivs_rt = num(ivs_renda_e_trabalho)) |>     # Renda e Trabalho
  arrange(cod)
saveRDS(ivs, "Bancos_rds/ivs_mg_2010.rds")

# Camada municipal de vulnerabilidade (IVS 2010 + saneamento Censo 2022)
vuln <- full_join(ivs, readRDS("Bancos_rds/saneamento_mg_2022.rds"), by = "cod")
saveRDS(vuln, "Bancos_rds/vulnerabilidade_mg.rds")

cat("OK IVS:", nrow(ivs), "municipios | NA:", sum(is.na(ivs$ivs)),
    "| IVS range:", paste(round(range(ivs$ivs, na.rm = TRUE), 3), collapse = "-"),
    "| vulnerabilidade combinada:", nrow(vuln), "munic.\n")
