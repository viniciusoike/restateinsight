# restateinsight

Quarto-based personal blog deployed to Netlify at restateinsight.com.
Focus: economics, urban analytics, and data visualization (primarily Brazil).

- **Never run a bare `quarto render` to "sync", "rebuild", or "regenerate" `_site/`.**
  It re-renders all ~165 documents (slow) and rewrites the whole tracked `_site/`
  tree, producing thousands of spurious diffs plus stray intermediate `.html` /
  `index_files/` next to every source `.qmd`. `freeze: true` only caches R
  *compute*, not HTML rendering — it does **not** make a full render cheap.

## Commands

```bash
quarto preview                 # local dev server on port 4200
quarto render <path/to/file>   # render ONE page only (see warning below)
quarto publish netlify         # deploy to production
```

> **Never run a bare `quarto render`.** It rebuilds all ~165 documents from
> scratch (minutes long) and rewrites the entire tracked `_site/` tree. Always
> scope renders to the file(s) you changed: `quarto render posts/<cat>/<slug>/index.qmd`.
> See the Gotchas section.

## Architecture

```
posts/<category>/<slug>/                  # blog posts as index.qmd + assets
  brazil-in-charts/                       # Brazil-focused analysis
  general-posts/                          # general topics
  shiny-apps/                             # interactive app posts
  tutorial-ggplot2/
  tutorial-tidyverse/
static/images/posts/<post-slug>/          # charts highlighted from a blog post
static/images/standalone/                 # charts without a blog post
static/images/chart-challenge/<year>/     # 30DayChartChallenge images
static/gallery/gallery.json               # gallery metadata registry
_freeze/                                  # cached computation results
```

- **`static/data/README.md`** — dataset → producing-script → source manifest,
  the gitignore/whitelist policy, and the raw-input path coupling. Read it
  before touching anything under `static/data/`.
- **`static/images/README.md`** — image-directory taxonomy and the two distinct
  "thumbnail" systems (auto-generated gallery `thumbs/` vs hand-made post-card
  `thumbnails/`). Read it before reorganizing images.
- **Data-extraction scripts are canonical at `static/data-raw/R/`** (git-tracked).
  There is no second copy — the old untracked `static/data/raw/R/` was removed.

## Gallery Workflow

When adding new charts to the gallery:

1. Drop image into the matching folder:
   - `static/images/posts/<post-slug>/` — charts from a blog post (1–2 highlights per post; `<post-slug>` must match the post's folder name under `posts/<category>/<slug>/`)
   - `static/images/standalone/` — charts without an associated post
   - `static/images/chart-challenge/<year>/` — 30DayChartChallenge submissions
2. `Rscript R/build_thumbs.R` — generates WebP thumbnails (1200px, 85% quality)
3. `Rscript R/build_gallery.R` — prints JSON template entries for unregistered images (auto-fills `post` URL when the slug matches an existing post)
4. Paste template into `static/gallery/gallery.json`, fill in all `TODO` fields
5. `quarto preview` (or `quarto render gallery.qmd`) to rebuild — never a bare `quarto render`

Thumbnails live at `<image-dir>/thumbs/<stem>.webp`. Requires ImageMagick (`brew install imagemagick`).

Gallery images must be PNG, not SVG — choropleth/map SVGs run 2–30 MB and
are served full-size on card click. Convert any SVG to a 2400px-wide PNG
before registering it.

### Sourcing gallery images

- **Old posts** (figures cached as unnamed chunks under `_freeze/`):
  `Rscript R/import_freeze.R <post-slug>` — opens a contact sheet of that
  post's rendered figures, prompts for picks, and copies them with a
  descriptive kebab-case name into `static/images/posts/<slug>/`. Never
  re-render old posts just to export figures.
- **New posts**: export the 1–2 highlight figures explicitly with
  `save_highlight()` from `R/save_highlight.R` (ragg PNG, 300 dpi, white
  background) in a chunk with `output: false`. Slug is auto-detected from
  the post directory.

## New Blog Posts

Create `posts/<category>/<YYYY-MM-slug>/index.qmd`. The `_metadata.yml` in `posts/` sets shared defaults.

## Gotchas

- **Never run a bare `quarto render` to "sync", "rebuild", or "regenerate" `_site/`.**
  It re-renders all ~165 documents (slow) and rewrites the whole tracked `_site/`
  tree, producing thousands of spurious diffs plus stray intermediate `.html` /
  `index_files/` next to every source `.qmd`. `freeze: true` only caches R
  *compute*, not HTML rendering — it does **not** make a full render cheap.
  - To rebuild specific pages: `quarto render posts/<cat>/<slug>/index.qmd` (one
    or more explicit paths), or use `quarto preview` for iterating.
  - **Resolving `_site/` merge conflicts:** do **not** re-render the whole site.
    `_site/` is committed build output; take one side of the conflict (e.g.
    `git checkout --ours -- _site/...`) or render only the handful of changed
    pages, then commit. The author refreshes `_site/` via their own targeted
    renders / `quarto publish netlify`, not a wholesale rebuild.
  - If a full render was started by mistake: kill it, `git restore .` to undo the
    `_site/` churn, then `git clean -fd` (excluding any pre-existing untracked
    dirs) to delete the stray root-level `.html` and `index_files/` intermediates.
- **`_quarto.yml` controls the entire website.** Edit with extreme care — a bad change here can break every page. Test with `quarto preview` before committing.
- **`freeze: true`** is set in `posts/_metadata.yml`, so post compute outputs are cached under `_freeze/`. Don't delete `_freeze/` casually — re-running every post's code is slow. To force a single post to re-execute, delete only its subdir under `_freeze/`.

## Code Style

Use RStudio-style section headers: `# Section ----`, `## Subsection ----`. Never box-style `====` headers.
