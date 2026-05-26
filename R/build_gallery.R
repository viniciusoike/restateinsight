# ── build_gallery.R ──────────────────────────────────────────────────────────
#
# Gallery JSON Maintenance Script
#
# PURPOSE
#   Scans the two image directories, compares against the existing gallery.json,
#   and prints ready-to-paste JSON template entries for any images not yet
#   registered. Fill in the TODO fields before committing.
#
# USAGE
#   # From the project root (where restateinsight.Rproj lives):
#   source("R/build_gallery.R")
#   # or from the terminal:
#   Rscript R/build_gallery.R
#
# WORKFLOW (adding a new chart)
#   1. Drop the image into static/images/charts/  (full-res PNG or SVG)
#   2. Run this script  →  it prints a template JSON entry
#   3. Run R/build_thumbs.R  →  generates the WebP thumbnail
#   4. Paste the template into static/data/gallery.json
#   5. Fill in title, topic, post, code, description
#   6. Quarto rebuild or `quarto preview`
#
# IMAGE DIRECTORIES SCANNED
#   static/images/charts/               ← standalone charts (PNG / SVG)
#   static/images/chart-challenge/      ← 30DayChartChallenge (year subdirs)
#
# ─────────────────────────────────────────────────────────────────────────────

library(jsonlite)
library(fs)
library(stringr)
library(purrr)

# ── Paths (relative to project root) ─────────────────────────────────────────
GALLERY_JSON  <- "static/data/gallery.json"
CHARTS_DIR    <- "static/images/charts"
CHALLENGE_DIR <- "static/images/chart-challenge"

stopifnot(
  "Run this script from the project root (where restateinsight.Rproj is)." =
    file_exists(GALLERY_JSON)
)

# ── Load existing gallery ─────────────────────────────────────────────────────
existing     <- fromJSON(GALLERY_JSON, simplifyVector = FALSE)
existing_ids <- map_chr(existing, "id")
existing_imgs <- map_chr(existing, "img")

# ── Helper: local path → web path ─────────────────────────────────────────────
to_web_path <- function(p) paste0("/", str_remove(as.character(p), "^\\./"))

# ── Scan: standalone charts (top-level PNGs/SVGs only, skip thumbs/) ─────────
chart_files <- dir_ls(CHARTS_DIR, regexp = "\\.(png|svg)$", recurse = FALSE)

# ── Scan: chart-challenge (recurse into year dirs, skip thumbs/ subdirs) ──────
challenge_files <- dir_ls(CHALLENGE_DIR, regexp = "\\.(png|svg)$", recurse = TRUE)
challenge_files  <- challenge_files[!str_detect(challenge_files, "/thumbs/")]

# ── Combine ───────────────────────────────────────────────────────────────────
all_web_paths <- to_web_path(c(chart_files, challenge_files))
new_paths     <- setdiff(all_web_paths, existing_imgs)

if (length(new_paths) == 0) {
  message("✔  gallery.json is up to date — no new images found.")
  invisible(NULL)
} else {
  message(sprintf("→  %d new image(s) found. Template entries printed below.\n", length(new_paths)))

  # ── Template builder ─────────────────────────────────────────────────────────
  make_entry <- function(img_path) {
    filename     <- path_file(img_path)
    stem         <- path_ext_remove(filename)
    is_challenge <- str_detect(img_path, "chart-challenge")

    # Derive year from path (works for both standalone and challenge)
    year <- {
      yr <- str_extract(img_path, "20\\d{2}")
      if (!is.na(yr)) as.integer(yr) else as.integer(format(Sys.Date(), "%Y"))
    }

    # Thumbnail path: same directory + /thumbs/ subdir, always .webp
    thumb <- paste0(path_dir(img_path), "/thumbs/", stem, ".webp")

    # ID slug
    id <- if (is_challenge) {
      day <- str_extract(stem, "^\\d+")
      paste0("challenge-", year, "-", day)
    } else {
      str_replace_all(stem, "[_\\s.]+", "-") |> str_to_lower()
    }

    # Avoid duplicates in id (append suffix if needed)
    if (id %in% existing_ids) {
      id <- paste0(id, "-", str_extract(img_path, "20\\d{2}") %||% "new")
    }

    list(
      id          = id,
      title       = "TODO: chart title",
      topic       = "TODO: e.g. Economics / Demographics / Urban",
      tool        = "ggplot2",
      img         = img_path,
      thumb       = thumb,
      post        = NULL,
      code        = NULL,
      year        = year,
      source      = if (is_challenge) "30DayChartChallenge" else NULL,
      description = "TODO: one sentence describing what the chart shows."
    )
  }

  new_entries <- map(new_paths, make_entry)
  cat(toJSON(new_entries, pretty = TRUE, auto_unbox = TRUE, null = "null"))
  cat("\n")

  message(
    "\n→  Next steps:",
    "\n   1. Run R/build_thumbs.R to generate the missing WebP thumbnails.",
    "\n   2. Paste the above JSON into static/data/gallery.json (inside the top-level array).",
    "\n   3. Fill in every 'TODO' field.",
    "\n   4. Rebuild the site with `quarto render` or `quarto preview`."
  )
}
