# Thumbnail Generation Script ----
#
# PURPOSE
#   Generates WebP thumbnails at 1200 px wide for every chart image that does
#   not yet have a thumbnail in the corresponding /thumbs/ subdirectory.
#   Already-existing thumbnails are skipped (safe to re-run).
#
# REQUIREMENTS
#   install.packages("magick")   # R binding for ImageMagick
#   ImageMagick must be installed on the system:
#     macOS:   brew install imagemagick
#     Ubuntu:  sudo apt install imagemagick
#
# USAGE
#   # From the project root:
#   source("R/build_thumbs.R")
#   # or from the terminal:
#   Rscript R/build_thumbs.R
#
# THUMBNAIL SPECS
#   Format  : WebP
#   Width   : 1200 px  (height is proportional)
#   Quality : 85 %
#   SVG     : rasterised at 300 dpi before resizing (preserves crispness)
#
# IMAGE DIRECTORIES SCANNED (each with its own /thumbs/ subfolder)
#   static/images/posts/<post-slug>/         charts tied to a blog post
#   static/images/standalone/                charts without a blog post
#   static/images/chart-challenge/<year>/    30DayChartChallenge

library(magick)
library(fs)
library(stringr)
library(purrr)

# Config ----
THUMB_WIDTH   <- 1200L
THUMB_QUALITY <- 85L
SVG_DENSITY   <- 300L

IMAGE_ROOTS <- c(
  "static/images/posts",
  "static/images/standalone",
  "static/images/chart-challenge"
)

# Helpers ----

thumb_path <- function(img_path) {
  dir  <- path_dir(img_path)
  stem <- path_ext_remove(path_file(img_path))
  path(dir, "thumbs", paste0(stem, ".webp"))
}

make_thumb <- function(img_path) {
  out <- thumb_path(img_path)

  if (file_exists(out)) {
    message("  skip   ", img_path)
    return(invisible(NULL))
  }

  dir_create(path_dir(out), recurse = TRUE)

  tryCatch({
    img <- if (str_ends(img_path, "\\.svg")) {
      image_read_svg(img_path, density = SVG_DENSITY)
    } else {
      image_read(img_path)
    }

    img |>
      image_resize(paste0(THUMB_WIDTH, "x")) |>
      image_write(out, format = "webp", quality = THUMB_QUALITY)

    sz <- round(file_size(out) / 1024)
    message(sprintf("  ok  %-55s -> %s  (%d KB)", img_path, out, sz))

  }, error = function(e) {
    message("  ERROR  ", img_path, "\n         ", conditionMessage(e))
  })
}

# Scan ----

existing_roots <- IMAGE_ROOTS[dir_exists(IMAGE_ROOTS)]

stopifnot(
  "No image roots found. Run from the project root." = length(existing_roots) > 0
)

all_files <- map(existing_roots, \(root) {
  dir_ls(root, regexp = "\\.(png|svg)$", recurse = TRUE)
}) |>
  unlist(use.names = FALSE)

# Drop any files that are themselves inside a thumbs/ dir
all_files <- all_files[!str_detect(all_files, "/thumbs/")]

already   <- sum(map_lgl(all_files, \(f) file_exists(thumb_path(f))))
to_build  <- length(all_files) - already

message(sprintf(
  "Found %d image(s): %d already have thumbnails, %d to generate.\n",
  length(all_files), already, to_build
))

# Generate ----
walk(all_files, make_thumb)

message(sprintf("\nDone. %d thumbnail(s) generated.", to_build))
