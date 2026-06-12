# Gallery JSON Maintenance Script ----
#
# PURPOSE
#   Scans the image directories, compares against the existing gallery.json,
#   and prints ready-to-paste JSON template entries for any images not yet
#   registered. Fill in the TODO fields before committing.
#
# USAGE
#   source("R/build_gallery.R")          # from the project root
#   Rscript R/build_gallery.R            # or from the terminal
#
# WORKFLOW (adding a new chart)
#   1. Drop image into the appropriate folder:
#        static/images/posts/<post-slug>/      charts tied to a blog post
#        static/images/standalone/             charts without a blog post
#        static/images/chart-challenge/<year>/ 30DayChartChallenge
#   2. Rscript R/build_thumbs.R          generate the WebP thumbnail
#   3. Rscript R/build_gallery.R         prints a template JSON entry
#   4. Paste the template into static/gallery/gallery.json
#   5. Fill in title / topic / description (and post if not auto-derived)
#   6. quarto render (or quarto preview)
#
# SMART DEFAULTS
#   For posts/<slug>/ images, the script searches posts/<category>/<slug>/
#   and prefills the `post` URL automatically; year is inferred from a
#   YYYY-MM- prefix when present.

library(jsonlite)
library(fs)
library(stringr)
library(purrr)

# Paths ----

GALLERY_JSON  <- "static/gallery/gallery.json"
POSTS_DIR     <- "static/images/posts"
STANDALONE    <- "static/images/standalone"
CHALLENGE_DIR <- "static/images/chart-challenge"

stopifnot(
  "Run this script from the project root (where restateinsight.Rproj is)." =
    file_exists(GALLERY_JSON)
)

# Load existing gallery ----

existing      <- fromJSON(GALLERY_JSON, simplifyVector = FALSE)
existing_ids  <- map_chr(existing, "id")
existing_imgs <- map_chr(existing, "img")

# Helpers ----

to_web_path <- function(p) paste0("/", str_remove(as.character(p), "^\\./"))

# Search posts/<category>/<slug>/ and return the matching web URL, else NULL
find_post_url <- function(slug) {
  if (!dir_exists("posts")) return(NULL)
  candidates <- dir_ls("posts", recurse = TRUE, type = "directory")
  match <- candidates[path_file(candidates) == slug]
  if (length(match) == 0) return(NULL)
  paste0(to_web_path(match[1]), "/")
}

# Derive year from a YYYY-MM- prefix or YYYY anywhere in the path
derive_year <- function(path) {
  yr <- str_extract(path, "20\\d{2}")
  if (!is.na(yr)) as.integer(yr) else as.integer(format(Sys.Date(), "%Y"))
}

# Scan ----

roots <- c(POSTS_DIR, STANDALONE, CHALLENGE_DIR)
roots <- roots[dir_exists(roots)]

scan_root <- function(root) {
  files <- dir_ls(root, regexp = "\\.(png|svg)$", recurse = TRUE)
  files[!str_detect(files, "/thumbs/")]
}

all_files     <- unlist(map(roots, scan_root), use.names = FALSE)
all_web_paths <- to_web_path(all_files)
new_paths     <- setdiff(all_web_paths, existing_imgs)

if (length(new_paths) == 0) {
  message("OK  gallery.json is up to date - no new images found.")
  invisible(NULL)
} else {
  message(sprintf("->  %d new image(s) found. Template entries printed below.\n",
                  length(new_paths)))

  # Template builder ----

  make_entry <- function(img_path) {
    filename <- path_file(img_path)
    stem     <- path_ext_remove(filename)
    thumb    <- paste0(path_dir(img_path), "/thumbs/", stem, ".webp")
    year     <- derive_year(img_path)

    is_post      <- str_detect(img_path, "^/static/images/posts/")
    is_challenge <- str_detect(img_path, "^/static/images/chart-challenge/")
    is_standalone <- str_detect(img_path, "^/static/images/standalone/")

    if (is_post) {
      slug <- str_extract(img_path, "(?<=^/static/images/posts/)[^/]+")
      post_url <- find_post_url(slug)
      source   <- NULL
      year     <- derive_year(slug)
      id       <- str_replace_all(stem, "[_\\s.]+", "-") |> str_to_lower()
    } else if (is_challenge) {
      day      <- str_extract(stem, "^\\d+")
      id       <- paste0("challenge-", year, "-", day)
      post_url <- NULL
      source   <- "30DayChartChallenge"
    } else {
      id       <- str_replace_all(stem, "[_\\s.]+", "-") |> str_to_lower()
      post_url <- NULL
      source   <- NULL
    }

    if (id %in% existing_ids) {
      id <- paste0(id, "-", year)
    }

    list(
      id          = id,
      title       = "TODO: chart title",
      topic       = "TODO: e.g. Economics / Demographics / Urban",
      tool        = "ggplot2",
      img         = img_path,
      thumb       = thumb,
      post        = post_url,
      code        = NULL,
      year        = year,
      source      = source,
      description = "TODO: one sentence describing what the chart shows."
    )
  }

  new_entries <- map(new_paths, make_entry)
  cat(toJSON(new_entries, pretty = TRUE, auto_unbox = TRUE, null = "null"))
  cat("\n")

  message(
    "\n->  Next steps:",
    "\n   1. Run R/build_thumbs.R to generate the missing WebP thumbnails.",
    "\n   2. Paste the above JSON into static/gallery/gallery.json (inside the top-level array).",
    "\n   3. Fill in every 'TODO' field.",
    "\n   4. Rebuild the site with `quarto render` or `quarto preview`."
  )
}
