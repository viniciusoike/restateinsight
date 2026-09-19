# Build Cache Pruning Script ----
#
# PURPOSE
#   Removes build leftovers that no render will ever read again:
#   - _freeze/ entries whose source .qmd no longer exists (renamed or
#     deleted posts)
#   - .quarto/quarto-session-temp* dirs left behind by killed renders
#
#   Dry run by default: prints what would be removed and exits.
#
# USAGE
#   Rscript R/prune_build_cache.R            # dry run
#   Rscript R/prune_build_cache.R --apply    # delete
#
#   Run from the project root. Orphaned _freeze/ entries are tracked in git,
#   so a deletion can be undone with `git restore _freeze/`. Session temp
#   dirs are gitignored scratch space; they are skipped while any quarto
#   render or preview process is running.

library(cli)
library(fs)

# Config ----------------------------------------------------------------------

FREEZE_ROOT <- "_freeze/posts"
QUARTO_DIR <- ".quarto"

# Helpers ---------------------------------------------------------------------

find_freeze_orphans <- function(freeze_root) {
  markers <- dir_ls(
    freeze_root,
    recurse = TRUE,
    type = "directory",
    regexp = "/(execute-results|figure-html)$"
  )
  doc_dirs <- unique(path_dir(markers))
  sources <- paste0(path_rel(doc_dirs, start = "_freeze"), ".qmd")

  return(doc_dirs[!file_exists(sources)])
}

find_session_temps <- function(quarto_dir) {
  if (!dir_exists(quarto_dir)) {
    return(character(0))
  }
  temps <- dir_ls(
    quarto_dir,
    type = "directory",
    regexp = "quarto-session-temp"
  )

  return(temps)
}

quarto_is_running <- function() {
  status <- suppressWarnings(system2(
    "pgrep",
    c("-f", shQuote("[q]uarto.*(render|preview)")),
    stdout = FALSE,
    stderr = FALSE
  ))

  return(status == 0)
}

remove_empty_parents <- function(dirs, stop_at) {
  parents <- unique(path_dir(dirs))
  while (length(parents) > 0) {
    parents <- parents[parents != stop_at & dir_exists(parents)]
    is_empty <- vapply(
      parents,
      function(d) all(path_file(dir_ls(d, all = TRUE)) == ".DS_Store"),
      logical(1)
    )
    dir_delete(parents[is_empty])
    parents <- unique(path_dir(parents[is_empty]))
  }

  return(invisible(NULL))
}

dir_total_size <- function(dirs) {
  if (length(dirs) == 0) {
    return(as_fs_bytes(0))
  }
  sizes <- vapply(
    dirs,
    function(d) sum(dir_info(d, recurse = TRUE, type = "file")$size),
    numeric(1)
  )

  return(as_fs_bytes(sum(sizes)))
}

# Scan ------------------------------------------------------------------------

if (!file_exists("_quarto.yml") || !dir_exists(FREEZE_ROOT)) {
  cli_abort(
    "Run this script from the project root (where {.file _quarto.yml} is)."
  )
}

apply_changes <- "--apply" %in% commandArgs(trailingOnly = TRUE)

orphans <- find_freeze_orphans(FREEZE_ROOT)
temps <- find_session_temps(QUARTO_DIR)

skip_temps <- length(temps) > 0 && quarto_is_running()
if (skip_temps) {
  cli_alert_warning(
    "A quarto render or preview is running; leaving {.path {QUARTO_DIR}} alone."
  )
  temps <- character(0)
}

cli_h1("Orphaned freeze entries")
if (length(orphans) == 0) {
  cli_alert_success("None.")
} else {
  cli_ul(orphans)
  cli_text("{length(orphans)} entr{?y/ies}, {dir_total_size(orphans)}.")
}

cli_h1("Quarto session temp dirs")
cli_text("{length(temps)} dir{?s}, {dir_total_size(temps)}.")

# Delete ----------------------------------------------------------------------

if (!apply_changes) {
  cli_rule()
  cli_alert_info("Dry run. Re-run with {.code --apply} to delete.")
  quit(save = "no")
}

dir_delete(orphans)
remove_empty_parents(orphans, stop_at = FREEZE_ROOT)
dir_delete(temps)

cli_rule()
cli_alert_success(
  "Removed {length(orphans)} freeze entr{?y/ies} and {length(temps)} temp dir{?s}."
)
cli_alert_info(
  "Review with {.code git status _freeze}; undo with {.code git restore _freeze}."
)
