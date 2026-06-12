# Gallery Highlight Export Helper ----
#
# PURPOSE
#   Standardized export for gallery highlight figures in new posts.
#   Saves a high-res PNG to static/images/posts/<post-slug>/ with
#   consistent settings (ragg device, 300 dpi, white background), so
#   every new gallery image shares the same quality baseline regardless
#   of the post's chunk options.
#
# USAGE (inside a post's index.qmd, in a chunk with output: false)
#   source(here::here("R/save_highlight.R"))   # or relative: ../../R/...
#   save_highlight(p, "births-by-state")
#   save_highlight(p, "births-by-state", width = 9, height = 6)
#
#   The post slug is auto-detected from the rendering directory
#   (posts/<category>/<slug>/); pass `slug =` to override.
#
# AFTER EXPORTING
#   Rscript R/build_thumbs.R
#   Rscript R/build_gallery.R

# Helpers ----

.find_project_root <- function(start = getwd()) {
  dir <- normalizePath(start, winslash = "/")
  repeat {
    if (length(list.files(dir, pattern = "\\.Rproj$")) > 0) return(dir)
    parent <- dirname(dir)
    if (parent == dir) stop("Project root (.Rproj) not found above ", start)
    dir <- parent
  }
}

# Main ----

save_highlight <- function(plot,
                           name,
                           slug   = basename(getwd()),
                           width  = 8,
                           height = 5,
                           dpi    = 300) {

  stopifnot(
    "name must be kebab-case (e.g. 'births-by-state')" =
      grepl("^[a-z0-9]+(-[a-z0-9]+)*$", name),
    "ragg package required: install.packages('ragg')" =
      requireNamespace("ragg", quietly = TRUE)
  )

  root <- .find_project_root()
  dir  <- file.path(root, "static", "images", "posts", slug)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  path <- file.path(dir, paste0(name, ".png"))

  ggplot2::ggsave(
    filename = path,
    plot     = plot,
    device   = ragg::agg_png,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = dpi,
    bg       = "white"
  )

  message("Saved gallery highlight: ", path)
  invisible(path)
}
