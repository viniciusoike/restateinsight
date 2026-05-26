# Gallery Maintenance Guide

This document covers the architecture of `gallery.qmd` and the two-script
workflow for adding new charts.

---

## What was built

| File | Purpose |
|------|---------|
| `gallery.qmd` | Quarto page that renders the gallery. All layout, filtering, and card logic lives here as a single `{=html}` block. |
| `static/data/gallery.json` | Single source of truth for all chart metadata. The page fetches this at runtime — adding a chart only requires editing this file. |
| `static/images/charts/thumbs/` | WebP thumbnails (1200 px wide, 85 % quality) for standalone charts. |
| `static/images/chart-challenge/<year>/thumbs/` | WebP thumbnails for 30DayChartChallenge submissions. |
| `R/build_gallery.R` | Helper script: scans image directories and prints template JSON entries for new images. |
| `R/build_thumbs.R` | Helper script: generates missing WebP thumbnails via the `magick` package. |

### How the gallery page works

- On page load, the page fetches `/static/data/gallery.json`.
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
  "source":      "30DayChartChallenge",   // string | null — shown as badge
  "description": "One sentence."         // string
}
```

Valid topics (extend freely — chips are generated from the data):
`Economics`, `Demographics`, `Urban`, `Transport`, `Culture`, `Health`, `Environment`

Valid tools (extend freely):
`ggplot2`, `sf`, `ggplot2 + sf`, `Observable`, `Plotly`

---

## Adding a new chart

### 1. Drop the image

For a **standalone chart** (not part of a challenge):
```
static/images/charts/<filename>.png
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

## Updating an existing entry

Edit `static/data/gallery.json` directly. Common updates:

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
    ├── data/
    │   └── gallery.json                 ← chart metadata (source of truth)
    └── images/
        ├── charts/                      ← full-res standalone chart images
        │   └── thumbs/                  ← generated WebP thumbnails
        └── chart-challenge/
            ├── 2025/
            │   └── thumbs/
            └── 2026/
                └── thumbs/
```

`_quarto.yml` has `resources` entries that copy `static/data/` and
`static/images/charts/thumbs/` into `_site/` during build, since Quarto only
copies files that are directly referenced in `.qmd` source.
