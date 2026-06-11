# =============================================================================
# FIGURAS DAS ANALISES COMPLEMENTARES E DE ROBUSTEZ
#   Desenha uma figura para cada analise complementar (scripts 20-23):
#   fig_irr_robustos      (script 22) - IC da IRR robustos a autocorrelacao temporal
#   fig_validacao_alerta  (script 21) - validacao preditiva do alerta (skill x lead)
#   fig_confirmacao       (script 20) - efeito da chuva apos ajuste pela taxa de confirmacao
#   fig_fdr               (script 23) - significancia antes/depois do FDR (Benjamini-Hochberg)
# Entradas: Bancos_rds/{ic_robustos,validacao_alerta,confirmacao_covariavel,fdr}.rds
# Saidas:   Resultados/Figuras/*.png
# =============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)
                  library(forcats); library(patchwork)})
dir.create("Resultados/Figuras", recursive = TRUE, showWarnings = FALSE)

AZUL <- "#4575b4"; VERM <- "#d73027"; CINZA <- "#9e9e9e"; VERDE <- "#1a9850"
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text    = element_text(face = "bold"),
        legend.position = "bottom")

# =============================================================================
# FIGURA 1 - IC da IRR robustos a autocorrelacao temporal (script 22)
# =============================================================================
ic <- readRDS("Bancos_rds/ic_robustos.rds")
# transforma "1.36 (1.27-1.46)***" em (irr, lo, hi)
parse_irr <- function(s) {
  m <- regmatches(s, regexec("([0-9.]+) \\(([0-9.]+)-([0-9.]+)\\)", s))[[1]]
  as.numeric(m[2:4])
}
metodos <- c(naive = "Naive (iid, atual)",
             cl_meso = "Robusto: mesorregião (12)",
             cl_mesoano = "Robusto: mesorreg.-ano (180)",
             gee = "GEE Poisson AR(1)")
d1 <- lapply(names(metodos), function(k) {
  pr <- parse_irr(ic[[k]]["precip"]); te <- parse_irr(ic[[k]]["temp"])
  tibble(metodo = metodos[k],
         var = c("Precipitação", "Temperatura"),
         irr = c(pr[1], te[1]), lo = c(pr[2], te[2]), hi = c(pr[3], te[3]))
}) |> bind_rows() |>
  mutate(metodo = factor(metodo, levels = rev(unname(metodos))),
         var = factor(var, levels = c("Precipitação", "Temperatura")))

g1 <- ggplot(d1, aes(irr, metodo, color = var)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), linewidth = 0.7, fatten = 3) +
  geom_text(aes(label = sprintf("%.2f", irr)), vjust = -0.9, size = 3, show.legend = FALSE) +
  facet_wrap(~ var, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("Precipitação" = AZUL, "Temperatura" = VERM), guide = "none") +
  labs(title = "Razão de taxas (IRR) por desvio-padrão, com IC95%\nrobustos à autocorrelação temporal",
       subtitle = "A estimativa pontual é robusta; os intervalos alargam-se corretamente (α AR(1) = 0,11)",
       x = "IRR (IC95%)", y = NULL) +
  tema
ggsave("Resultados/Figuras/fig_irr_robustos.png", g1, width = 8.5, height = 5.5, dpi = 300)
cat("Salvo: fig_irr_robustos.png\n")

# =============================================================================
# FIGURA 2 - validacao preditiva do alerta (script 21)
# =============================================================================
va <- readRDS("Bancos_rds/validacao_alerta.rds")

# (A) varredura de lead-time
L <- matrix(as.numeric(unlist(va$leads)), ncol = 4)
colnames(L) <- c("lead", "sens", "far", "hss")
dA <- as.data.frame(L) |>
  pivot_longer(c(sens, far, hss), names_to = "metrica", values_to = "valor") |>
  mutate(metrica = recode(metrica,
                          sens = "Sensibilidade", far = "Taxa de falso-alarme", hss = "HSS"),
         metrica = factor(metrica, levels = c("Taxa de falso-alarme", "Sensibilidade", "HSS")))
gA <- ggplot(dA, aes(lead, valor, color = metrica)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  scale_color_manual(values = c("Taxa de falso-alarme" = VERM, "Sensibilidade" = AZUL, "HSS" = VERDE)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous(breaks = 0:4) +
  labs(title = "(A) Desempenho por defasagem (lead)",
       subtitle = "Sensibilidade estável (45–49%): o lead de 2 SE não foi escolhido por maximizar o acerto",
       x = "Defasagem do gatilho (semanas epidemiológicas)", y = NULL, color = NULL) +
  tema

# (B) metricas dos gatilhos p80 vs p90
mtab <- function(x, rotulo) tibble(
  gatilho = rotulo,
  metrica = c("Sensibilidade", "Especificidade", "VPP", "Falso-alarme", "HSS"),
  valor   = c(x$sens, x$spec, x$ppv, x$far, x$hss))
dB <- bind_rows(mtab(va$p80, "Chuva 4 SE > p80"), mtab(va$p90, "Chuva 4 SE > p90")) |>
  mutate(metrica = factor(metrica,
           levels = c("Sensibilidade", "Especificidade", "VPP", "Falso-alarme", "HSS")))
gB <- ggplot(dB, aes(metrica, valor, fill = gatilho)) +
  geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = scales::percent(valor, accuracy = 1)),
            position = position_dodge(0.75), vjust = -0.4, size = 2.7) +
  scale_fill_manual(values = c("Chuva 4 SE > p80" = AZUL, "Chuva 4 SE > p90" = CINZA)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(title = "(B) Métricas no teste (2020–2024), gatilho a 2 SE",
       subtitle = "Sensibilidade moderada, falso-alarme elevado e baixo poder discriminante (HSS ≈ 0,08)",
       x = NULL, y = NULL, fill = NULL) +
  tema + theme(axis.text.x = element_text(angle = 20, hjust = 1))

g2 <- gA / gB +
  plot_annotation(title = "Validação preditiva do alerta climatológico (treino ≤ 2019 | teste 2020–2024)",
                  theme = theme(plot.title = element_text(face = "bold", size = 13)))
ggsave("Resultados/Figuras/fig_validacao_alerta.png", g2, width = 9, height = 8.5, dpi = 300)
cat("Salvo: fig_validacao_alerta.png\n")

# =============================================================================
# FIGURA 3 - o sinal climatico sobrevive ao ajuste por confirmacao (script 20)
# =============================================================================
cc <- readRDS("Bancos_rds/confirmacao_covariavel.rds")
covs <- c(precip_z = "Precipitação", temp_z = "Temperatura", ivs_z = "IVS",
          dens_z = "Densidade (log)", pconf_z = "Taxa de confirmação")
extrai <- function(mat, modelo) {
  v <- intersect(rownames(mat), names(covs))
  tibble(modelo = modelo, termo = covs[v],
         beta = mat[v, 1], se = mat[v, 2]) |>
    mutate(lo = beta - 1.96 * se, hi = beta + 1.96 * se)
}
d3 <- bind_rows(
  extrai(cc$sem0, "Confirmados — sem ajuste"),
  extrai(cc$sem1, "Confirmados — ajustado p/ confirmação"),
  extrai(cc$semN, "Notificados — ajustado p/ confirmação")) |>
  mutate(modelo = factor(modelo, levels = c("Confirmados — sem ajuste",
            "Confirmados — ajustado p/ confirmação", "Notificados — ajustado p/ confirmação")),
         termo = factor(termo, levels = rev(unname(covs))))

g3 <- ggplot(d3, aes(beta, termo, color = modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(0.6), linewidth = 0.6, fatten = 2.5) +
  scale_color_manual(values = c("Confirmados — sem ajuste" = CINZA,
                                "Confirmados — ajustado p/ confirmação" = AZUL,
                                "Notificados — ajustado p/ confirmação" = VERDE)) +
  labs(title = "Modelo de erro espacial: o efeito da chuva\nsobrevive ao ajuste pelo acesso ao diagnóstico",
       subtitle = "Coeficientes (β por desvio-padrão, IC95%); a precipitação permanece positiva ao incluir a taxa de confirmação",
       x = "β (incidência em escala log, por desvio-padrão)", y = NULL, color = NULL) +
  guides(color = guide_legend(nrow = 3)) +
  tema
ggsave("Resultados/Figuras/fig_confirmacao.png", g3, width = 9, height = 5.5, dpi = 300)
cat("Salvo: fig_confirmacao.png\n")

# =============================================================================
# FIGURA 4 - controle de multiplas comparacoes (FDR / Benjamini-Hochberg) (script 23)
# =============================================================================
fd <- readRDS("Bancos_rds/fdr.rds")

# (A) Moran bivariado: I por covariavel, significancia apos FDR
dM <- fd$moran_biv |>
  mutate(sig = ifelse(p_fdr < 0.05, "Significativo (FDR)", "Não significativo (FDR)"),
         Variavel = fct_reorder(Variavel, I))
gMA <- ggplot(dM, aes(I, Variavel, color = sig)) +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_segment(aes(x = 0, xend = I, yend = Variavel), linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Significativo (FDR)" = AZUL, "Não significativo (FDR)" = CINZA)) +
  labs(title = "(A) Moran bivariado (incidência × covariável)",
       subtitle = "10 de 11 permanecem significativos sob FDR (apenas “Água” não)",
       x = "I de Moran bivariado", y = NULL, color = NULL) +
  tema

# (B) Kendall por mesorregiao: tau, significancia apos FDR
dK <- fd$kendall |>
  mutate(sig = ifelse(p_fdr < 0.05, "Significativo (FDR)", "Não significativo (FDR)"),
         name_meso = fct_reorder(name_meso, tau))
gKB <- ggplot(dK, aes(tau, name_meso, color = sig)) +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_segment(aes(x = 0, xend = tau, yend = name_meso), linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Significativo (FDR)" = AZUL, "Não significativo (FDR)" = CINZA)) +
  labs(title = "(B) Kendall por mesorregião (incidência × precipitação, 4 SE)",
       subtitle = "10 de 12 permanecem significativos sob FDR (idêntico ao p bruto)",
       x = "τ de Kendall", y = NULL, color = NULL) +
  tema

g4 <- (gMA / gKB) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Controle de múltiplas comparações (Benjamini-Hochberg, FDR)",
    subtitle = sprintf("LISA (Moran local): municípios significativos caem de %d para %d sob FDR — cautela na leitura dos aglomerados",
                       fd$lisa["bruto"], fd$lisa["fdr"]),
    theme = theme(plot.title = element_text(face = "bold", size = 13),
                  plot.subtitle = element_text(color = "grey30"),
                  legend.position = "bottom")) &
  theme(legend.position = "bottom")
ggsave("Resultados/Figuras/fig_fdr.png", g4, width = 9, height = 9, dpi = 300)
cat("Salvo: fig_fdr.png\n")

cat("\nFIGURAS_OK — 4 figuras geradas em Resultados/Figuras/\n")
