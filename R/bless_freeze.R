# Freeze Blessing Script ----
#
# PURPOSE
#   Applies front-matter edits to a post's _freeze/ entry without executing
#   the post. With `freeze: true`, a project render replays the frozen
#   markdown, which carries its own copy of the YAML front matter; edits to
#   title, image, categories, and similar keys never reach _site/ otherwise.
#
#   For each post whose source md5 differs from its freeze hash, the script:
#   1. finds the source version the freeze was built from in git history
#   2. confirms the document body is unchanged since then
#   3. confirms only presentation keys changed (see PRESENTATION_KEYS)
#   4. splices the new front matter into the frozen markdown and updates
#      the hash
#
#   Posts that fail any check are reported and left alone. Body edits and
#   execution-relevant keys (execute, knitr, format, ...) need a real render.
#
# USAGE
#   Rscript R/bless_freeze.R                          # dry run, all posts
#   Rscript R/bless_freeze.R posts/<cat>/<slug>/index.qmd
#   Rscript R/bless_freeze.R --apply                  # write changes
#
#   Run from the project root. Changes are tracked in git; undo with
#   `git restore _freeze/`.
#
# AFTER BLESSING
#   Push blessed posts into _site/ without executing them, then re-render
#   the listing pages that show them:
#     quarto render <post.qmd> --use-freezer
#     quarto render blog.qmd

library(cli)
library(fs)

# Config ----------------------------------------------------------------------

FREEZE_ROOT <- "_freeze"

PRESENTATION_KEYS <- c(
  "title",
  "subtitle",
  "description",
  "abstract",
  "date",
  "date-modified",
  "author",
  "categories",
  "keywords",
  "image",
  "image-alt",
  "draft",
  "toc",
  "toc-depth",
  "toc-title",
  "title-block-banner",
  "page-layout",
  "comments",
  "citation",
  "aliases"
)

MARKDOWN_MARKER <- "\"markdown\": \""

# Helpers ---------------------------------------------------------------------

read_text <- function(path) {
  text <- rawToChar(readBin(path, "raw", file_size(path)))
  Encoding(text) <- "UTF-8"

  return(text)
}

md5_text <- function(text) {
  tmp <- file_temp()
  on.exit(file_delete(tmp))
  writeBin(charToRaw(text), tmp)

  return(unname(tools::md5sum(tmp)))
}

# Splits "---\n<yaml>\n---\n<body>" into the delimited block and the body.
split_front_matter <- function(text) {
  pattern <- "(?s)\\A---[ \\t]*\\r?\\n.*?\\r?\\n(---|\\.\\.\\.)[ \\t]*(\\r?\\n|\\z)"
  match <- regexpr(pattern, text, perl = TRUE)
  if (match == -1) {
    return(NULL)
  }
  block <- regmatches(text, match)
  body <- substring(text, nchar(block) + 1, nchar(text))

  return(list(block = block, body = body))
}

parse_front_matter <- function(block) {
  yaml_text <- sub(
    "(?s)\\A---[^\\n]*\\n(.*)\\n[^\\n]*\\n?\\z",
    "\\1",
    block,
    perl = TRUE
  )

  return(yaml::yaml.load(yaml_text))
}

json_escape <- function(text) {
  quoted <- as.character(jsonlite::toJSON(text, auto_unbox = TRUE))

  return(substr(quoted, 2, nchar(quoted) - 1))
}

# Finds the source text whose md5 matches the freeze hash: the staged
# version first, then every committed version, following renames.
find_frozen_source <- function(source, hash) {
  tmp <- file_temp()
  on.exit(if (file_exists(tmp)) file_delete(tmp))

  log <- suppressWarnings(system2(
    "git",
    c(
      "log",
      "--follow",
      shQuote("--format=commit %H"),
      "--name-only",
      "--",
      shQuote(source)
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  log <- log[nzchar(log)]
  is_commit <- startsWith(log, "commit ")
  # Merge commits list no files; keep only commits followed by a path.
  has_path <- is_commit & c(!is_commit[-1], FALSE)
  shas <- sub("^commit ", "", log[has_path])
  paths <- log[which(has_path) + 1]

  revisions <- c(paste0(":", source), paste0(shas, ":", paths))
  for (revision in revisions) {
    status <- system2(
      "git",
      c("show", shQuote(revision)),
      stdout = tmp,
      stderr = FALSE
    )
    if (status == 0 && unname(tools::md5sum(tmp)) == hash) {
      return(read_text(tmp))
    }
  }

  return(NULL)
}

changed_keys <- function(old, new) {
  keys <- union(names(old), names(new))
  changed <- keys[
    !vapply(keys, function(k) identical(old[[k]], new[[k]]), logical(1))
  ]

  return(changed)
}

# Returns list(status, detail, json) for one freeze entry. `json` holds the
# rewritten file text when the post can be blessed.
check_post <- function(source, freeze_json) {
  outcome <- function(status, detail = "", json = NULL) {
    return(list(status = status, detail = detail, json = json))
  }

  json_text <- read_text(freeze_json)
  frozen <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  new_source <- read_text(source)
  new_hash <- md5_text(new_source)

  if (identical(frozen$hash, new_hash)) {
    return(outcome("current"))
  }

  old_source <- find_frozen_source(source, frozen$hash)
  if (is.null(old_source)) {
    return(outcome("no-base", "no git version matches the freeze hash"))
  }

  old_split <- split_front_matter(old_source)
  new_split <- split_front_matter(new_source)
  frozen_split <- split_front_matter(frozen$result$markdown)
  if (is.null(old_split) || is.null(new_split) || is.null(frozen_split)) {
    return(outcome("no-front-matter"))
  }
  if (!identical(old_split$body, new_split$body)) {
    return(outcome("body-changed", "needs a real render"))
  }
  if (!identical(frozen_split$block, old_split$block)) {
    return(outcome("mismatch", "frozen front matter differs from its source"))
  }

  old_yaml <- tryCatch(
    parse_front_matter(old_split$block),
    error = function(e) NULL
  )
  new_yaml <- tryCatch(
    parse_front_matter(new_split$block),
    error = function(e) NULL
  )
  if (is.null(old_yaml) || is.null(new_yaml)) {
    return(outcome("yaml-error", "front matter does not parse"))
  }
  keys <- changed_keys(old_yaml, new_yaml)
  blocked <- setdiff(keys, PRESENTATION_KEYS)
  if (length(blocked) > 0) {
    return(outcome("execution-keys", paste(blocked, collapse = ", ")))
  }

  new_json <- splice_json(
    json_text,
    frozen,
    old_split$block,
    new_split$block,
    new_hash
  )
  if (is.null(new_json)) {
    return(outcome(
      "splice-failed",
      "JSON escaping did not match; bless by hand"
    ))
  }

  detail <- if (length(keys) == 0) {
    "formatting only"
  } else {
    paste(keys, collapse = ", ")
  }

  return(outcome("blessable", detail, new_json))
}

# Edits the raw JSON text so every other byte of the file is preserved, then
# parses the result back and checks it against the intended content.
splice_json <- function(json_text, frozen, old_block, new_block, new_hash) {
  replace_once <- function(text, from, to) {
    parts <- strsplit(text, from, fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      return(NULL)
    }

    return(paste0(parts[1], to, parts[2]))
  }

  out <- replace_once(
    json_text,
    paste0(MARKDOWN_MARKER, json_escape(old_block)),
    paste0(MARKDOWN_MARKER, json_escape(new_block))
  )
  if (is.null(out)) {
    return(NULL)
  }
  out <- replace_once(
    out,
    paste0("\"hash\": \"", frozen$hash, "\""),
    paste0("\"hash\": \"", new_hash, "\"")
  )
  if (is.null(out)) {
    return(NULL)
  }

  expected <- frozen
  expected$hash <- new_hash
  expected$result$markdown <- paste0(
    new_block,
    substring(
      frozen$result$markdown,
      nchar(old_block) + 1,
      nchar(frozen$result$markdown)
    )
  )
  if (!identical(jsonlite::fromJSON(out, simplifyVector = FALSE), expected)) {
    return(NULL)
  }

  return(out)
}

# Arguments -------------------------------------------------------------------

if (!file_exists("_quarto.yml") || !dir_exists(FREEZE_ROOT)) {
  cli_abort(
    "Run this script from the project root (where {.file _quarto.yml} is)."
  )
}

args <- commandArgs(trailingOnly = TRUE)
apply_changes <- "--apply" %in% args
requested <- setdiff(args, "--apply")

freeze_jsons <- dir_ls(
  FREEZE_ROOT,
  recurse = TRUE,
  type = "file",
  regexp = "/execute-results/html\\.json$"
)
sources <- paste0(
  path_rel(path_dir(path_dir(freeze_jsons)), start = FREEZE_ROOT),
  ".qmd"
)
names(freeze_jsons) <- sources

if (length(requested) > 0) {
  unknown <- setdiff(requested, sources)
  if (length(unknown) > 0) {
    cli_abort(c(
      "No freeze entry for {.file {unknown}}.",
      "i" = "Pass project-relative paths such as {.file posts/general-posts/<slug>/index.qmd}."
    ))
  }
  freeze_jsons <- freeze_jsons[requested]
}

freeze_jsons <- freeze_jsons[file_exists(names(freeze_jsons))]

# Check -----------------------------------------------------------------------

results <- Map(check_post, names(freeze_jsons), freeze_jsons)
statuses <- vapply(results, `[[`, character(1), "status")

stale <- results[statuses != "current"]
cli_h1("Freeze entries out of date: {length(stale)}")

for (status in unique(statuses[statuses != "current"])) {
  cli_h2(status)
  posts <- names(results)[statuses == status]
  for (post in posts) {
    detail <- results[[post]]$detail
    cli_li(
      if (nzchar(detail)) {
        "{.file {post}} {.emph ({detail})}"
      } else {
        "{.file {post}}"
      }
    )
  }
}

blessable <- names(results)[statuses == "blessable"]

# Write -----------------------------------------------------------------------

cli_rule()
if (length(blessable) == 0) {
  cli_alert_info("Nothing to bless.")
  quit(save = "no")
}

if (!apply_changes) {
  cli_alert_info(
    "Dry run. Re-run with {.code --apply} to bless {length(blessable)} post{?s}."
  )
  quit(save = "no")
}

for (post in blessable) {
  writeBin(charToRaw(results[[post]]$json), freeze_jsons[[post]])
}

cli_alert_success("Blessed {length(blessable)} post{?s}.")
cli_text("Push them into {.path _site/} without executing:")
cli_code(c(
  paste(
    "quarto render",
    paste(blessable, collapse = " \\\n  "),
    "--use-freezer"
  ),
  "quarto render blog.qmd"
))
