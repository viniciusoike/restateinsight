# `static/images/` — image taxonomy

All published imagery. Referenced from posts as absolute paths
(`/static/images/...`) and from the gallery via `static/gallery/gallery.json`.

## Two distinct "thumbnail" systems

The word "thumb" appears in two unrelated places. Don't conflate them:

1. **Gallery thumbnails** — `<image-dir>/thumbs/<stem>.webp`
   Small WebP previews shown on gallery cards. **Auto-generated** by
   `Rscript R/build_thumbs.R` (1200px, 85% quality). Referenced by the
   `thumb` field in `gallery.json`. Live in `thumbs/` subdirs under
   `posts/<slug>/`, `standalone/`, and `chart-challenge/<year>/`.
   Never hand-edit these — regenerate them.

2. **Post-card cover images** — `thumbnails/<name>.png`
   The hand-made featured image set in a post's YAML `image:` field, shown on
   the blog listing. PNG/SVG, one per post, **not** auto-generated and **not**
   part of the gallery pipeline. ~20 posts reference these.

(A clearer name for #2 would be `post-cards/`, but renaming it means editing
the `image:` field in ~20 posts and re-rendering each `_site` page — deferred
with the rest of the casing cleanup below.)

## Directories

| Dir                  | Holds                                                      | Referenced by              |
| -------------------- | --------------------------------------------------------- | -------------------------- |
| `posts/<slug>/`      | Figures highlighted from a post (+ `thumbs/` for gallery) | `gallery.json`, the post   |
| `standalone/`        | Gallery charts with no associated post (+ `thumbs/`)      | `gallery.json`             |
| `chart-challenge/<year>/` | 30DayChartChallenge entries (+ `thumbs/`)            | `gallery.json`             |
| `thumbnails/`        | Post-card cover images (YAML `image:` field)              | post front matter          |
| `maps/`              | Map/choropleth figures embedded in posts                  | posts                      |
| `spo_metro/`         | São Paulo metro study figures + `webscrape/` screenshots  | posts (7)                  |
| `r_tutorial/`        | R-tutorial figures and screenshots                        | posts (12)                 |
| `ggplot2_tutorial/`  | ggplot2-tutorial figures                                  | posts                      |
| `bad_charts/`        | "fixing bad charts" before/after examples                 | posts                      |
| `setup_r/`           | R-setup tutorial screenshots                              | posts                      |
| `replications/`      | Chart-replication studies (+ `nexo_imdb.R`)               | posts                      |
| `coffeeshops/`       | Coffee-shop location viz                                   | posts                      |
| `apps/`              | Shiny-app screenshots                                      | posts                      |
| `icons/`             | Small assets used as *inputs* to map scripts (e.g. `subway.png`) | `posts/.../maps.R` |
| `profile.jpg`        | Author photo (site `image:` in `_quarto.yml`)             | `_quarto.yml`              |

Only `gallery/`, `images/posts/`, `images/standalone/`, and
`images/chart-challenge/` are whitelisted under `project: resources:` in
`_quarto.yml`. Every other dir ships to `_site/` **only because a `.qmd`
references its files inline** — if you add images that no page links, they
won't be published. Prefer the whitelisted dirs for gallery content.

## Gallery images must be PNG

Choropleth/map SVGs run 2–30 MB and are served full-size on card click.
Convert any SVG to a 2400px-wide PNG before registering it in `gallery.json`.

## Known inconsistencies (deferred)

These are cosmetic and intentionally **not** fixed yet, because each rename
forces editing inline references in many `.qmd` files and re-rendering the
corresponding committed `_site/` pages (a bare `quarto render` is forbidden —
see root `CLAUDE.md`):

- Mixed casing: `bad_charts`, `ggplot2_tutorial`, `r_tutorial`, `setup_r`,
  `spo_metro` use `snake_case`; post folders and `chart-challenge`,
  `tutorial-showtext` use `kebab-case`. Target is all-kebab.
- `tutorial-showtext/` is currently unreferenced — verify it isn't tied to a
  draft post before removing.

Revisit alongside the `_site/`-in-git decision.
