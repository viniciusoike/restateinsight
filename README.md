# Real Estate Insights

**[Visit the website →](https://www.restateinsight.com)**

A data analysis and visualization blog focused on economics, urban development, real estate markets, and socioeconomic trends in Brazil and beyond.

## About

This repository hosts the source code for [restateinsight.com](https://www.restateinsight.com), a personal blog featuring data-driven analysis and visualizations. Content covers a range of topics including:

- **Economic Analysis** - Labor markets, unemployment trends, inflation, and macroeconomic indicators
- **Urban Analytics** - Public transportation, city development, and demographic patterns
- **Real Estate Markets** - Housing affordability, price dynamics, and market trends
- **Data Visualization** - Tutorials and examples using ggplot2, R, and other tools
- **Brazilian Insights** - In-depth analysis of Brazil's economic and social landscape

## Technical Stack

Built with [Quarto](https://quarto.org), the site leverages R for data analysis and visualization, with extensive use of ggplot2, tidyverse, and spatial analysis packages. All posts are written in Quarto Markdown and rendered to static HTML.

## Running locally

Requires [Quarto](https://quarto.org) and R (with the tidyverse and the packages used by individual posts).

```bash
quarto preview          # local dev server on port 4200
quarto render           # full static build → _site/
quarto publish netlify  # deploy to production
```

Post computations are cached under `_freeze/`, so a render reuses prior results instead of re-running every post. To force a single post to re-execute, delete only its subdirectory under `_freeze/`.

## Repository Structure

- `posts/` - Blog post source files organized by category (`brazil-in-charts/`, `general-posts/`, `shiny-apps/`, `tutorial-ggplot2/`, `tutorial-tidyverse/`)
- `static/` - Images, data files, and the chart gallery registry (`static/gallery/gallery.json`)
- `_freeze/` - Cached computation results for faster rendering
- `_site/` - Generated static website output
- `R/` - Helper scripts (thumbnail generation, gallery build, figure export)

See [`GALLERY.md`](GALLERY.md) for the chart-gallery workflow and [`CLAUDE.md`](CLAUDE.md) for the full contributor guide.

## Author

Maintained by Vinicius Oike Reginatto - [GitHub](https://github.com/viniciusoike) | [LinkedIn](https://www.linkedin.com/in/viniciusoike/)
