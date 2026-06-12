# restateinsight

Quarto-based personal blog deployed to Netlify at restateinsight.com.
Focus: economics, urban analytics, and data visualization (primarily Brazil).

## Commands

```bash
quarto preview          # local dev server on port 4200
quarto render           # full static build → _site/
quarto publish netlify  # deploy to production
```

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

## Gallery Workflow

When adding new charts to the gallery:

1. Drop image into the matching folder:
   - `static/images/posts/<post-slug>/` — charts from a blog post (1–2 highlights per post; `<post-slug>` must match the post's folder name under `posts/<category>/<slug>/`)
   - `static/images/standalone/` — charts without an associated post
   - `static/images/chart-challenge/<year>/` — 30DayChartChallenge submissions
2. `Rscript R/build_thumbs.R` — generates WebP thumbnails (1200px, 85% quality)
3. `Rscript R/build_gallery.R` — prints JSON template entries for unregistered images (auto-fills `post` URL when the slug matches an existing post)
4. Paste template into `static/gallery/gallery.json`, fill in all `TODO` fields
5. `quarto render` or `quarto preview` to rebuild

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

- **`_quarto.yml` controls the entire website.** Edit with extreme care — a bad change here can break every page. Test with `quarto preview` before committing.
- **`freeze: true`** is set in `posts/_metadata.yml`, so post compute outputs are cached under `_freeze/`. Don't delete `_freeze/` casually — re-running every post's code is slow. To force a single post to re-execute, delete only its subdir under `_freeze/`.

## Code Style

Use RStudio-style section headers: `# Section ----`, `## Subsection ----`. Never box-style `====` headers.
