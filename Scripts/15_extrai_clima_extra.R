# =============================================================================
# EXTRACAO das variaveis adicionais do BR-DWGD por municipio de MG (2010-2024)
#   RH (umidade relativa, %), Rs (radiacao solar, MJ/m2), u2 (vento, m/s),
#   ETo (evapotranspiracao, mm). Estatistica zonal (media das celulas).
# Entrada: Dados_brutos/dados_clima/raw/{RH,Rs,u2,ETo}_20010101_20251231_*.nc
# Saida:   Dados_brutos/dados_clima/mg_clima_extra_diario.rds
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(terra); library(sf); library(exactextractr); library(geobr)})

muni <- read_municipality(code_muni = 31, year = 2020, simplified = TRUE, showProgress = FALSE)
ex_var <- function(file) {
  r <- rast(file); if (is.na(crs(r, describe = TRUE)$code)) crs(r) <- "EPSG:4326"
  m <- st_transform(muni, crs(r)); tt <- terra::time(r)
  idx <- which(tt >= as.Date("2010-01-01") & tt <= as.Date("2024-12-31"))
  ex <- exact_extract(crop(r[[idx]], vect(m)), m, "mean", progress = FALSE)
  cat(basename(file), "ok", format(Sys.time(), "%H:%M:%S"), "\n")
  list(mat = as.matrix(ex), cods = as.integer(m$code_muni), dates = as.Date(tt[idx]))
}
base <- "Dados_brutos/dados_clima/raw/"
RH  <- ex_var(paste0(base, "RH_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
RS  <- ex_var(paste0(base, "Rs_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
U2  <- ex_var(paste0(base, "u2_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
ETo <- ex_var(paste0(base, "ETo_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))

nd <- length(RH$dates); nc <- length(RH$cods)
extra <- data.frame(cod = rep(RH$cods, times = nd), date = rep(RH$dates, each = nc),
                    rh = as.vector(RH$mat), rs = as.vector(RS$mat),
                    u2 = as.vector(U2$mat), eto = as.vector(ETo$mat))
saveRDS(extra, "Dados_brutos/dados_clima/mg_clima_extra_diario.rds")
cat("Linhas:", nrow(extra), "| munic:", length(unique(extra$cod)),
    "| NA:", sum(is.na(extra$rh) | is.na(extra$rs) | is.na(extra$u2) | is.na(extra$eto)), "\nEXTRA_OK\n")
