# =============================================================================
# BANCO 01 - POPULACAO MUNICIPAL E POR MESORREGIAO (Minas Gerais, 2010-2024)
# Fontes (IBGE):
#   - Estimativas de populacao (SIDRA tab. 6579) ...... 2011-2021, 2024
#   - Censo 2010 (SIDRA tab. 1378, total via API) ..... 2010
#   - Censo 2022 (SIDRA tab. 4709) .................... 2022
#   - 2023 por interpolacao geometrica (2022 <-> 2024)
#   - Malha territorial (municipio -> mesorregiao) via geobr (join espacial)
# Saidas: lookup_muni_meso.rds, pop_municipal_2010_2024.rds, pop_mesorregiao_2010_2024.rds
# Obs.: requer internet (SIDRA/geobr).
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
for (p in c("sidrar","geobr","sf","dplyr","tidyr"))
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressMessages({library(sidrar); library(geobr); library(sf); library(dplyr); library(tidyr)})

# ---- 1. Lookup municipio (7 dig.) -> mesorregiao (join espacial geobr) -------
sf_use_s2(FALSE)
meso <- read_meso_region(code_meso = 31, year = 2020, simplified = TRUE, showProgress = FALSE)
muni <- read_municipality(code_muni = 31, year = 2020, simplified = TRUE, showProgress = FALSE)
cent <- suppressWarnings(st_centroid(muni)) |> st_transform(st_crs(meso))
lookup <- suppressWarnings(st_join(cent, meso[, "name_meso"], join = st_within)) |>
  st_drop_geometry() |>
  transmute(cod = as.integer(code_muni), name_muni, name_meso)
if (any(is.na(lookup$name_meso))) {                       # fallback: centroide fora do poligono
  miss <- which(is.na(lookup$name_meso))
  lookup$name_meso[miss] <- meso$name_meso[st_nearest_feature(cent[miss, ], meso)]
}
saveRDS(lookup, "Bancos_rds/lookup_muni_meso.rds")

# ---- 2. Populacao municipal por ano ----------------------------------------
clean <- function(d) d |>
  transmute(cod = as.integer(`Município (Código)`), pop = as.numeric(Valor)) |>
  filter(!is.na(cod))

# Estimativas oficiais IBGE (validadas contra os arquivos estimativa_dou do FTP)
est <- get_sidra(6579, variable = 9324, period = as.character(c(2011:2021, 2024)),
                 geo = "City", geo.filter = list("State" = 31)) |>
  transmute(cod = as.integer(`Município (Código)`), ano = as.integer(Ano), pop = as.numeric(Valor))
# Censo 2010 - tabela classificada por sexo/idade; total via API (todas classif. = 0)
c10 <- get_sidra(api = "/t/1378/n6/all/v/93/p/2010/c1/0/c2/0/c287/0/c455/0") |>
  clean() |> filter(substr(as.character(cod), 1, 2) == "31") |> mutate(ano = 2010L)
# Censo 2022
c22 <- get_sidra(4709, variable = 93, period = "2022",
                 geo = "City", geo.filter = list("State" = 31)) |> clean() |> mutate(ano = 2022L)

pop_municipal <- bind_rows(est, c10, c22) |>
  distinct(cod, ano, .keep_all = TRUE) |>
  pivot_wider(names_from = ano, values_from = pop, names_prefix = "a") |>
  mutate(a2023 = round(sqrt(a2022 * a2024))) |>              # interpolacao 2023
  pivot_longer(starts_with("a"), names_to = "ano", values_to = "pop") |>
  mutate(ano = as.integer(sub("a", "", ano))) |>
  filter(ano >= 2010, ano <= 2024, !is.na(pop)) |>
  arrange(cod, ano)
saveRDS(pop_municipal, "Bancos_rds/pop_municipal_2010_2024.rds")

# ---- 3. Populacao por mesorregiao ------------------------------------------
pop_meso <- pop_municipal |> left_join(lookup, by = "cod") |>
  filter(!is.na(name_meso)) |>
  group_by(name_meso, ano) |> summarise(pop = sum(pop), .groups = "drop")
saveRDS(pop_meso, "Bancos_rds/pop_mesorregiao_2010_2024.rds")

cat("OK populacao:", n_distinct(pop_municipal$cod), "municipios x",
    n_distinct(pop_municipal$ano), "anos | mesorregioes:", n_distinct(pop_meso$name_meso), "\n")
