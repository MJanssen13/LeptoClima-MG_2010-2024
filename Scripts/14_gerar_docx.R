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
             "Figura 3" = "Resultados/Figuras/fig3_lisa_biv_scatter.png")

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
