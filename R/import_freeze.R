# Freeze Figure Import Script ----
#
# PURPOSE
#   Migrates rendered figures from the _freeze/ cache into the gallery
#   image tree, one post at a time. Old posts export figures as unnamed
#   chunks (unnamed-chunk-7-1.png); this script shows you a contact sheet
#   of every figure in a post, lets you pick the 1-2 highlights, and
#   copies them to static/images/posts/<post-slug>/ under a descriptive
#   name. The _freeze/ cache is never modified.
#
# USAGE
#   Rscript R/import_freeze.R <post-slug>
#   Rscript R/import_freeze.R 2023-10-wz-weddings
#
#   Run from the project root. A contact sheet opens in the browser;
#   pick images by number in the terminal and give each a descriptive,
#   kebab-case name (it becomes the gallery id and image URL).
#
# AFTER IMPORTING
#   Rscript R/build_thumbs.R      generate WebP thumbnails
#   Rscript R/build_gallery.R     print gallery.json template entries

library(fs)
library(stringr)
library(purrr)

# Config ----

FREEZE_DIR <- "_freeze/posts"
TARGET_DIR <- "static/images/posts"
FIG_EXT    <- c("png", "svg", "jpeg", "jpg")

stopifnot(
  "Run this script from the project root (where restateinsight.Rproj is)." =
    dir_exists(FREEZE_DIR)
)

# Helpers ----

read_input <- local({
  con <- NULL
  function(prompt_text) {
    cat(prompt_text)
    if (interactive()) {
      return(trimws(readline()))
    }
    if (is.null(con)) con <<- file("stdin", open = "r")
    trimws(readLines(con, n = 1))
  }
})

img_dims <- function(path) {
  tryCatch({
    info <- magick::image_info(magick::image_read(path))
    sprintf("%d x %d px", info$width[1], info$height[1])
  }, error = function(e) "")
}

valid_name <- function(x) str_detect(x, "^[a-z0-9]+(-[a-z0-9]+)*$")

# Locate post in _freeze ----

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript R/import_freeze.R <post-slug>", call. = FALSE)
}
slug <- args[1]

freeze_dirs <- dir_ls(FREEZE_DIR, recurse = TRUE, type = "directory")
post_dir    <- freeze_dirs[path_file(freeze_dirs) == slug]

if (length(post_dir) == 0) {
  near <- freeze_dirs[agrepl(slug, path_file(freeze_dirs), max.distance = 0.3)]
  msg  <- sprintf("No _freeze entry found for slug '%s'.", slug)
  if (length(near) > 0) {
    msg <- paste0(
      msg, "\nDid you mean:\n",
      paste0("  ", unique(path_file(near)), collapse = "\n")
    )
  }
  stop(msg, call. = FALSE)
}
if (length(post_dir) > 1) {
  warning("Multiple _freeze matches; using: ", post_dir[1])
  post_dir <- post_dir[1]
}

# Collect figures ----

figs <- dir_ls(post_dir, recurse = TRUE, type = "file", fail = FALSE)
figs <- figs[tolower(path_ext(figs)) %in% FIG_EXT]
figs <- figs[str_detect(figs, "figure-html")]
figs <- sort(figs)

if (length(figs) == 0) {
  stop("No figures found under ", post_dir, call. = FALSE)
}

cat(sprintf("\nFound %d figure(s) for '%s':\n\n", length(figs), slug))
dims <- map_chr(figs, img_dims)
walk(seq_along(figs), function(i) {
  cat(sprintf("  [%2d]  %-28s %s\n", i, path_file(figs[i]), dims[i]))
})

# Contact sheet ----

sheet <- path(tempdir(), paste0("freeze-", slug, ".html"))
cells <- map_chr(seq_along(figs), function(i) {
  sprintf(
    paste0(
      '<figure><img src="file://%s" loading="lazy">',
      "<figcaption><strong>[%d]</strong> %s<br><small>%s</small></figcaption></figure>"
    ),
    path_abs(figs[i]), i, path_file(figs[i]), dims[i]
  )
})
html <- sprintf(
  '<!DOCTYPE html><html><head><meta charset="utf-8"><title>%s</title>
  <style>
    body { font-family: sans-serif; margin: 2rem; background: #fafafa; }
    main { display: grid; grid-template-columns: repeat(auto-fill, minmax(420px, 1fr)); gap: 1.5rem; }
    figure { margin: 0; background: #fff; border: 1px solid #ddd; border-radius: 6px; padding: .75rem; }
    img { width: 100%%; height: auto; display: block; }
    figcaption { margin-top: .5rem; font-size: .85rem; color: #333; }
  </style></head>
  <body><h1>%s &mdash; %d figures</h1><main>%s</main></body></html>',
  slug, slug, length(figs), paste(cells, collapse = "\n")
)
writeLines(html, sheet)
utils::browseURL(paste0("file://", sheet))
cat("\nContact sheet opened in browser:\n  ", sheet, "\n")

# Interactive import loop ----

target <- path(TARGET_DIR, slug)
dir_create(target)
imported <- character(0)

repeat {
  pick <- read_input("\nImage number to import (blank to finish): ")
  if (pick == "") break

  i <- suppressWarnings(as.integer(pick))
  if (is.na(i) || i < 1 || i > length(figs)) {
    cat("Invalid number. Pick 1-", length(figs), "\n", sep = "")
    next
  }

  ext  <- tolower(path_ext(figs[i]))
  name <- read_input(sprintf("Descriptive name for [%d] (kebab-case, no extension): ", i))
  if (!valid_name(name)) {
    cat("Use lowercase kebab-case, e.g. weddings-by-state\n")
    next
  }

  dest <- path(target, paste0(name, ".", ext))
  if (file_exists(dest)) {
    ok <- read_input(sprintf("%s exists. Overwrite? [y/N]: ", path_file(dest)))
    if (tolower(ok) != "y") next
  }

  file_copy(figs[i], dest, overwrite = TRUE)
  imported <- c(imported, dest)
  cat("Copied -> ", dest, "\n")
}

# Summary ----

if (length(imported) == 0) {
  cat("\nNothing imported.\n")
} else {
  cat(sprintf("\nImported %d image(s) to %s/\n", length(imported), target))
  cat("\nNext steps:\n")
  cat("  Rscript R/build_thumbs.R\n")
  cat("  Rscript R/build_gallery.R   # paste template into static/gallery/gallery.json\n")
}
