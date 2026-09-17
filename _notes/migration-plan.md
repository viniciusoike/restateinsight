# Render migration plan

Goal: a project render runs no R code and makes no network calls. Every post
either replays from `_freeze/` or reads a tracked data snapshot. Ingestion
lives in scripts. Posts keep analysis and visualization, because in tutorials
the code is the content.

## Diagnosis

Measured on 2026-09-12 with Quarto 1.9.37.

### How `freeze` behaves

Tested in a scratch project:

| `freeze` | project render (`quarto render`) | single file (`quarto render <file>`) |
|---|---|---|
| `true` (current) | never executes a frozen post; **ignores source edits** | always executes |
| `auto` | executes only posts whose source changed | always executes |
| any | `quarto render <file> --use-freezer` replays the freeze, no execution | |

- `execute-results/html.json` stores `hash = md5(source file)` plus the full
  knitted markdown, **including the YAML front matter**.
- Under `freeze: true`, a project render uses the frozen markdown and skips
  the hash check. Prose and metadata edits to a published post never reach
  `_site/`.
- Quarto writes freeze entries only for documents with code. Every post
  with an R chunk has one, so a project render executes no R at all.

### Execution risk

- 153 `.qmd` under `posts/`, 2208 R chunks, 137 files with at least one chunk.
- Only R chunks exist; no other engines.
- 86 freeze entries were stale: the source changed after the last render.
  Under `freeze: true` those edits are invisible on the site. See Phase 1
  for the breakdown.
- 63 posts call an API or scrape at render time (`sidrar`, `geobr`,
  `GetBCBData`, `osmdata`, `rvest`, remote files).
- 48 posts read `static/data/`, which is mostly gitignored.
- 9 posts read paths outside the repo (`~/Documents/GitHub/...`).
- 3 posts call `renv::use(lockfile = )` in their first chunk (one commented
  out).

### Weight

- `_freeze/` 893 MB, with 70 orphaned entries (168 MB) left by renamed or
  deleted posts.
- `.quarto/` 1.0 GB: 670 MB of Quarto's internal freeze copy and 360 MB of
  803 leftover `quarto-session-temp*` dirs.
- `_site/` 1.0 GB, 1842 tracked files. `.git/` 924 MB.

## Phase 0 — housekeeping

- [x] `R/prune_build_cache.R`: removes orphaned `_freeze/` entries and stale
  `.quarto/quarto-session-temp*` dirs. Dry run by default.
  Removed 70 freeze entries (`_freeze/` 893 MB to 725 MB) and 803 temp
  dirs (360 MB).
- [x] Baseline: a full render of HEAD (166 documents) took 112 s with no R
  execution and wrote 1.0 GB of output. Taken in a throwaway `git worktree`
  with `--output-dir` outside the tree.

Render time is modest. The cost of a bare `quarto render` is the 1 GB rewrite
of the tracked `_site/`, not execution.

## Phase 1 — make the freeze cache honest

- [x] `R/bless_freeze.R`: when a post's only change is presentation-only front
  matter, splice the new front matter into the frozen markdown and update the
  hash. No execution. Blessed 48 posts on 2026-09-12; `_site/` updates after
  `quarto render <post> --use-freezer` on each, then `quarto render blog.qmd`.
- [ ] 26 posts are `body-changed`: the body was edited after the last render.
  Several come from the June 2026 image reorganization, which moved images
  and updated body paths without re-rendering. The tracked `_site/` still
  points to the old paths, and those images are broken (for example
  `2023-09-happiness` requests `/static/images/owid_happiness.png`). They
  need a real render, which executes the post; fragile posts should go
  through Phase 3 first.
- [ ] 11 posts are `no-base`: the freeze was built from a version never
  committed, so the script cannot prove the body is unchanged. Compare by
  hand or re-render.
- [ ] `2024-04-wz-rdt-brasil` has a `cateogories:` typo in its front matter.

Limit: the script handles front matter only. Body edits need a real render,
because the frozen markdown is knitted output.

## Phase 2 — inventory and tiering

`R/audit_posts.R` writes `_notes/post-inventory.csv` with path, chunk count,
network calls, `static/data` reads, out-of-repo paths, `renv::use`, freeze
present, and freeze age. Each post gets one tier, recorded in its YAML.

- **archive**: output is correct, inputs are gone or unreproducible. Code stays
  visible and never re-executes. Keeps `freeze: true`. The 3 `renv` posts and
  the 9 out-of-repo-path posts start here.
- **sourced**: ingestion moved to a script; the post reads a tracked snapshot.
  Target for most network posts.
- **live**: self-contained, data from CRAN packages only, safe to execute.
  Most of `tutorial-ggplot2/` and `tutorial-tidyverse/`.

## Phase 3 — decouple ingestion

Extend the pattern in `static/data/README.md`. Per post:

- `static/data-raw/R/<slug>.R` fetches data and writes a small snapshot to
  `static/data/`, whitelisted in `.gitignore`.
- `index.qmd` starts from `read_*()` and holds analysis and visualization.

Order by risk: out-of-repo paths, then `renv` posts, then network posts.
Tutorials follow a rule instead of a script: package-bundled data only, no
network. Snapshots need a size budget; LFS already covers
`.csv/.rds/.xlsx/.zip/.gpkg`.

## Phase 4 — flip `freeze` per tier

When a directory is clean, set `freeze: auto` there so edits propagate and
only changed posts execute. `archive` posts keep `freeze: true` in their own
YAML.

## Phase 5 — build hygiene

- `R/render-changed.sh`: render only posts whose source differs from HEAD.
- Optional pre-commit hook: warn when a post's md5 diverges from its freeze
  hash.

## Open decisions

1. Keep `_site/` in git, or build on Netlify? Dropping it shrinks the working
   tree but not `.git/` without a history rewrite.
2. Accept the unreproducible posts as `archive`, or restore their inputs?
3. Phase 3 scope: all network posts, or only those worth re-running?
