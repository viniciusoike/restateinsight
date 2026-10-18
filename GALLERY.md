# Gallery Maintenance Guide

This document covers the architecture of `gallery.qmd` and the two-script
workflow for adding new charts.

---

## What was built

| File | Purpose |
|------|---------|
| `gallery.qmd` | Quarto page that renders the gallery. All layout, filtering, and card logic lives here as a single `{=html}` block. |
| `static/gallery/gallery.json` | Single source of truth for all chart metadata. The page fetches this at runtime — adding a chart only requires editing this file. |
| `static/images/posts/<post-slug>/` | Charts highlighted from a specific blog post (1–2 per post). Each folder has its own `thumbs/` subdir for WebP thumbnails. |
| `static/images/standalone/` | Charts that don't belong to any blog post. Same `thumbs/` convention. |
| `static/images/chart-challenge/<year>/` | 30DayChartChallenge submissions, one folder per year, each with its own `thumbs/`. |
| `R/build_gallery.R` | Helper script: scans image directories and prints template JSON entries for new images. |
| `R/build_thumbs.R` | Helper script: generates missing WebP thumbnails via the `magick` package. |

### How the gallery page works

- On page load, the page fetches `/static/gallery/gallery.json`.
- Topic and Tool filter chips are built dynamically from the data.
- Cards are rendered in batches of 12 using an `IntersectionObserver` sentinel
  for smooth infinite scroll — no pagination, no extra pages.
- The filter bar is `position: fixed` (not `sticky`) and its `top` offset is
  measured from the Quarto navbar at runtime, so it works correctly regardless
  of theme or window size.

---

## gallery.json schema

```json
{
  "id":          "unique-slug",           // string, kebab-case
  "title":       "Chart Title",           // string
  "topic":       "Economics",             // string — drives Topic filter chips
  "tool":        "ggplot2",               // string — drives Tool filter chips
  "img":         "/static/images/...",    // string, web path to full-res image
  "thumb":       "/static/images/.../thumbs/....webp",  // string | null
  "post":        "/posts/.../",           // string | null — link to blog post
  "code":        "https://github.com/...", // string | null — link to source
  "year":        2025,                    // integer
  "source":      "30DayChartChallenge",   // string | null — shown as a tag
  "description": "One sentence.",        // string — shown under the title on the card
  "type":        "interactive",           // optional — omit for static charts
  "url":         "/posts/.../"            // interactive only — page the card opens
}
```

Topics currently in use (extend freely — chips are generated from the data):
`Economics`, `Urbanism`, `Demographics`, `Time Series`, `Comparisons`,
`Culture`, `Distributions`, `Uncertainties`, `Relationships`, `Climate`

Note the taxonomy mixes subject matter (`Economics`, `Demographics`) with
chart type (`Distributions`, `Comparisons` — inherited from
30DayChartChallenge category names). Prefer subject-matter topics for new
entries; the chart-type topics should eventually be remapped.

Tools currently in use (extend freely):
`ggplot2`, `sf`, `echarts4r`, `D3`

Interactive entries (`"type": "interactive"`) carry an "Interactive" badge,
open `url` on click instead of the lightbox, and are skipped by the
lightbox's prev/next. The "Interactive only" toggle in the filter bar shows
only these entries; it is hidden when the data has none.

---

## Adding a new chart

### 1. Drop the image

For a **chart tied to a blog post** (the common case — 1–2 highlights per post):
```
static/images/posts/<post-slug>/<filename>.png
```
The `<post-slug>` must match the post's folder name under `posts/<category>/<slug>/`.
`build_gallery.R` searches for it and auto-fills the `post` URL in the template.

For a **standalone chart** (no associated post):
```
static/images/standalone/<filename>.png
```

For a **30DayChartChallenge** submission:
```
static/images/chart-challenge/<year>/<day>_<theme>.png
```

### 2. Generate the thumbnail

```r
# From the project root in R / RStudio:
source("R/build_thumbs.R")
```

This creates `…/thumbs/<filename>.webp` for every image that doesn't already
have one. Safe to re-run — existing thumbnails are skipped.

Requires the `magick` package and ImageMagick on the system:
```bash
brew install imagemagick   # macOS
Rscript -e 'install.packages("magick")'
```

### 3. Add the entry to gallery.json

```r
source("R/build_gallery.R")
```

The script prints a ready-to-paste JSON template for every image not yet
registered in `gallery.json`. Copy it into the file and fill in the `TODO`
fields (title, topic, post, description).

### 4. Preview

```bash
quarto preview
```

The gallery page fetches the JSON at runtime, so no full rebuild is needed —
just save the JSON file and refresh the browser.

---

## Adding an interactive page

Interactive pages are built in the `ekio-viz` repo (`interactive/<name>/`)
and copied here as static files.

1. Add the page to `pages` in `R/sync_interactive.R` (ekio-viz folder name =
   slug under `static/interactive/`), then run
   `Rscript R/sync_interactive.R <name>`. Re-run it whenever the page
   changes in ekio-viz.
2. Create a wrapper post that embeds `/static/interactive/<slug>/` in an
   iframe. Copy the iframe and resize script from
   `posts/general-posts/2026-10-congresso-2027/index.qmd`; the script grows
   the iframe to fit the page, so no nested scrollbar appears.
3. Screenshot the page as a 2400px-wide PNG into `static/images/standalone/`,
   run `Rscript R/build_thumbs.R`, and add a `gallery.json` entry with
   `"type": "interactive"` and `"url"` pointing to the wrapper post.
4. Render the post and `gallery.qmd` with separate `quarto render` calls.
   Rendering both in one call wrote the post's title into `gallery.html`.
   Rendering a single post also rebuilds `archive.html`, `blog.html` and
   `blog.xml` from a partial index, which drops older entries. Check
   `git diff --stat _site/` before committing.

## Updating an existing entry

Edit `static/gallery/gallery.json` directly. Common updates:

- **Add a post link** once the blog post is published: set `"post": "/posts/..."`.
- **Add a code link**: set `"code": "https://github.com/..."`.
- **Change the topic or tool**: edit the relevant field — filter chips
  regenerate automatically from the data.

---

## Folder layout

```
restateinsight/
├── gallery.qmd                          ← gallery page
├── GALLERY.md                           ← this guide
├── R/
│   ├── build_gallery.R                  ← generate gallery.json entries
│   └── build_thumbs.R                   ← generate WebP thumbnails
└── static/
    ├── gallery/
    │   └── gallery.json                 ← chart metadata (source of truth)
    └── images/
        ├── posts/                       ← charts tied to a blog post
        │   └── <post-slug>/
        │       ├── <chart>.png          ← full-res image
        │       └── thumbs/
        │           └── <chart>.webp     ← generated WebP thumbnail
        ├── standalone/                  ← charts without a blog post
        │   └── thumbs/
        └── chart-challenge/
            ├── 2025/
            │   └── thumbs/
            └── 2026/
                └── thumbs/
```

`_quarto.yml` has `resources` entries that copy `static/gallery/`,
`static/images/posts/`, `static/images/standalone/`, and
`static/images/chart-challenge/` into `_site/` during build, since Quarto only
copies files that are directly referenced in `.qmd` source.
