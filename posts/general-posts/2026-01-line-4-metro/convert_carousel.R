# convert_carousel.R
# ─────────────────────────────────────────────────────────────
# Converte PDF de carrossel em PNGs de alta resolução para LinkedIn
#
# Por quê?
# O LinkedIn recomprime PDFs agressivamente, reduzindo a qualidade
# dos gráficos. Subir imagens PNG de alta resolução como carrossel
# de imagens preserva muito mais qualidade.
#
# Uso:
#   source("convert_carousel.R")
#   convert_carousel("carousel_linha4_v6.pdf")
#
# Ou direto no terminal:
#   Rscript convert_carousel.R carousel_linha4_v6.pdf
#
# Requisito: pacote 'pdftools' (que usa poppler internamente)
#   install.packages("pdftools")
# ─────────────────────────────────────────────────────────────

library(pdftools)
library(fs)

convert_carousel <- function(
    pdf_path,
    output_dir = NULL,
    dpi = 300,
    format = "png",
    prefix = "slide"
) {

  stopifnot(file.exists(pdf_path))

  # Diretório de saída: mesmo do PDF, subpasta "slides_png"
  if (is.null(output_dir)) {
    output_dir <- file.path(dirname(pdf_path), "slides_png")
  }
  fs::dir_create(output_dir)

  # Converter cada página
  message(sprintf("Convertendo '%s' a %d DPI...", basename(pdf_path), dpi))

  pages <- pdf_info(pdf_path)$pages

  files <- pdf_convert(
    pdf_path,
    format  = format,
    dpi     = dpi,
    filenames = file.path(
      output_dir,
      sprintf("%s_%02d.%s", prefix, seq_len(pages), format)
    )
  )

  # Reportar
  message(sprintf(
    "\n Done: %d slides convertidos -> %s",
    length(files),
    output_dir
  ))
  message(sprintf("  Tamanho medio: %.0f KB", mean(file.size(files)) / 1024))

  # Verificar dimensoes com magick se disponivel
  if (requireNamespace("magick", quietly = TRUE)) {
    img <- magick::image_read(files[1])
    info <- magick::image_info(img)
    message(sprintf("  Dimensoes: %d x %d px", info$width, info$height))

    if (info$width < 1080) {
      warning(
        "Largura menor que 1080px. Considere aumentar o DPI ou as ",
        "dimensoes do papel no QMD."
      )
    }
  }

  invisible(files)
}

# ── Execucao via linha de comando ──────────────────────────
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    stop("Uso: Rscript convert_carousel.R <arquivo.pdf> [dpi]")
  }
  pdf_file <- args[1]
  dpi <- if (length(args) >= 2) as.integer(args[2]) else 300
  convert_carousel(pdf_file, dpi = dpi)
}
