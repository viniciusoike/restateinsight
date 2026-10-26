# Sync interactive pages from ekio-viz ----------------------------------------
#
# Copies the public files of an interactive page from the ekio-viz repo into
# static/interactive/<slug>/. ekio-viz stays the source of truth; the blog
# keeps a pinned copy that only changes when this script runs.
#
# USAGE (from the project root)
#   Rscript R/sync_interactive.R            # sync every page in `pages`
#   Rscript R/sync_interactive.R congresso  # sync one page
#
# Only index.html and top-level .js files are copied. R scripts, data
# extracts and READMEs stay in ekio-viz.

library(cli)
library(fs)

# Config ---------------------------------------------------------------------

ekio_viz_dir <- path("..", "ekio-viz", "interactive")

# ekio-viz folder name = blog slug under static/interactive/
pages <- c(
  congresso = "congresso-2027",
  `eleicao-br` = "eleicao-br-2026"
)

# Functions ------------------------------------------------------------------

sync_page <- function(name, slug) {
  src <- path(ekio_viz_dir, name)
  if (!dir_exists(src)) {
    cli_abort("Source folder {.path {src}} not found.")
  }

  files <- c(path(src, "index.html"), dir_ls(src, glob = "*.js"))
  dest <- path("static", "interactive", slug)
  dir_create(dest)
  file_copy(files, dest, overwrite = TRUE)

  cli_alert_success("Copied {length(files)} file{?s} to {.path {dest}}.")
  return(invisible(files))
}

# Run ------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
selected <- if (length(args) > 0) args else names(pages)

unknown <- setdiff(selected, names(pages))
if (length(unknown) > 0) {
  cli_abort("Unknown page{?s}: {.val {unknown}}. Add to {.code pages} first.")
}

for (name in selected) {
  sync_page(name, pages[[name]])
}
