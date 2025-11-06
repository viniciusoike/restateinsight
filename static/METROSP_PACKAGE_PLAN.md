# metrosp R Package - Development Plan

**Date**: 2025-10-15
**Status**: Planning Phase

---

## Executive Summary

**YES - You have more than enough data and code to create a comprehensive R package!**

The existing codebase contains:
- **8 R scripts** (1,800+ lines of code)
- **Clean datasets** covering 2018-2024 (7,500+ rows)
- **66 metro stations** with geographic data
- **6 metro lines** with comprehensive metrics
- **Web scraping infrastructure** for 3 different data sources

This is an excellent candidate for a data package that solves a real problem: providing easy access to São Paulo Metro data that's otherwise trapped in poorly formatted PDFs and CSVs.

---

## Current Code Analysis

### R Scripts Overview

Located in `static/data-raw/R/`:

| File | Lines | Purpose |
|------|-------|---------|
| `metro_sp_csvs.R` | 821 | Main data processing engine for 2017-2024 |
| `metro_sp_line_5.R` | 411 | Web scraping for Line 5 (Lilac) - ViaMobilidade |
| `metro_sp_line_4.R` | 290 | Web scraping for Line 4 (Yellow) - ViaQuatro |
| `metro_sp_join.R` | 193 | Combines all data sources into unified datasets |
| `metro_sp_pdfs.R` | 185 | PDF extraction using tabulizer/pdftools |
| `geosampa_metro.R` | 116 | Geographic data processing (GeoSampa) |
| `metro_sp_download.R` | 99 | Downloads files from Metro SP transparency portal |
| `metro_sp.R` | 23 | Orchestration script |

### Code Strengths

1. **Well-Organized**: Clear separation between download → clean → join
2. **Comprehensive**: Handles multiple data formats (CSV, PDF, geographic)
3. **Robust**: Includes error handling, encoding fixes, multiple encodings
4. **Maintained**: Recent updates (Dec 2024, Feb 2025)
5. **Documented**: Good inline comments explaining challenges

### Code Challenges

1. **Multiple Data Sources**: 3 different websites with different formats
   - `https://transparencia.metrosp.com.br/` (Lines 1, 2, 3, 15)
   - `https://www.viaquatro.com.br/` (Line 4)
   - `https://www.viamobilidade.com.br/` (Line 5)

2. **Encoding Hell**: Mixed Latin-1, UTF-8, ISO-8859-1
   ```r
   # Example from metro_sp_csvs.R:707
   encoding = if_else(name %in% c("Junho", "Julho"), "UTF-8", "ISO-8859-1")
   ```

3. **Inconsistent Formats**:
   - Some PDFs have tables as low-resolution images
   - CSVs with varying skip rows (6 vs 25)
   - Different column structures by year

4. **Station Name Changes**:
   - "Carrão" → "Carrão-Assaí Atacadista"
   - "Penha" → "Penha-Lojas Besni"
   - "Saúde" → "Saúde-Ultrafarma"
   - "Patriarca" → "Patriarca-Vila Ré"

---

## Available Data

### Cleaned Datasets

Located in `static/data/`:

| File | Rows | Description |
|------|------|-------------|
| `metro_sp.csv` | 4,421 | Unified line-level data 2018-2024 |
| `metro_sp_stations_2021_2024.csv` | 3,103 | Station-level passenger data |
| `metro_sp_2018_2020.csv` | 1,328 | Historical line data |
| `metro_sp_2021_2024.csv` | 1,151 | Recent line data |
| `metro_sp_line_4.csv` | 508 | Line 4 specific data |
| `metro_sp_line_5.csv` | 491 | Line 5 specific data |
| `spo_metro_stations.gpkg` | - | Geographic station locations (sf) |
| `spo_metro_line.gpkg` | - | Current metro lines (sf) |
| `spo_metro_line_future.gpkg` | - | Planned metro lines (sf) |

### Data Structure

**metro_sp.csv columns:**
```
year, date, variable, metric, metric_label, metro_line, metro_line_num, value
```

**metro_sp_stations_2021_2024.csv columns:**
```
date, line_name_pt, line_name, line_number, station_name, station_name_full, avg_passenger
```

### Metrics Available

- **Total** - Total monthly passengers (in thousands)
- **Média dos Dias Úteis** - Average weekday passengers
- **Média dos Sábados** - Average Saturday passengers
- **Média dos Domingos** - Average Sunday passengers
- **Máxima Diária** - Maximum daily passengers

### Variables

- **transport** - Passengers transported
- **entrance** - Passenger entries (may differ from transport due to transfers)

### Coverage

**Metro Lines:**
- Line 1 (Blue/Azul) - 23 stations
- Line 2 (Green/Verde) - 14 stations
- Line 3 (Red/Vermelha) - 18 stations
- Line 4 (Yellow/Amarela) - 11 stations
- Line 5 (Lilac/Lilás) - 17 stations
- Line 15 (Silver/Prata) - 11 stations

**Time Period:** 2017-2024 (some gaps in early years)

**Companies:**
- Metrô (Lines 1, 2, 3, 15)
- ViaQuatro (Line 4)
- ViaMobilidade (Line 5)

---

## Package Feasibility Assessment

### ✅ Strong Case for Package Development

**Value Proposition:**
1. **Solves Real Problem**: Data is publicly available but in terrible formats
2. **Unique Offering**: No existing R package for São Paulo Metro data
3. **Research Value**: Useful for urban planning, transportation, economics research
4. **Educational Value**: Great example of web scraping + data cleaning pipeline
5. **Growing Interest**: São Paulo is Brazil's largest city, metro data is valuable

**Technical Readiness:**
- Comprehensive, working code already exists
- Clean, tidy datasets ready to ship
- Good test cases (multiple years/formats successfully processed)
- Active maintenance pattern established

**Data Quality:**
- 7+ years of historical data
- Multiple dimensions (time, station, line, metric)
- Geographic integration ready
- Consistent structure after cleaning

---

## Recommended Package Architecture

### Package Name: `metrosp`

**Tagline**: "Easy Access to São Paulo Metro Ridership Data"

### Proposed Structure

```
metrosp/
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── README.md
├── NEWS.md
├── .Rbuildignore
├── .gitignore
│
├── R/
│   ├── data.R              # Data documentation (@name metro_passengers, etc.)
│   ├── download.R          # download_metro_*() functions
│   ├── clean-csvs.R        # clean_csv_*() internal functions
│   ├── clean-pdfs.R        # clean_pdf_*() internal functions
│   ├── import.R            # import_metro_*() user-facing update functions
│   ├── utils.R             # Helper functions (str_simplify, etc.)
│   ├── dimensions.R        # Dimension tables (lines, stations, metrics)
│   ├── plot.R              # plot_metro_*() visualization helpers
│   └── zzz.R               # Package startup messages
│
├── data/                   # .rda files for instant loading
│   ├── metro_passengers.rda      # Main line-level time series
│   ├── metro_stations.rda        # Station-level time series
│   ├── metro_lines_geo.rda       # sf object - metro lines
│   ├── metro_stations_geo.rda    # sf object - station locations
│   ├── metro_lines_future.rda    # sf object - planned lines
│   ├── dim_lines.rda             # Dimension table
│   ├── dim_stations.rda          # Dimension table
│   └── dim_metrics.rda           # Dimension table
│
├── data-raw/               # Scripts to recreate data/ (not in package)
│   ├── metro_sp_download.R
│   ├── metro_sp_csvs.R
│   ├── metro_sp_line_4.R
│   ├── metro_sp_line_5.R
│   ├── metro_sp_join.R
│   ├── metro_sp_pdfs.R
│   ├── geosampa_metro.R
│   └── build-package-data.R    # Master script to rebuild everything
│
├── inst/
│   ├── extdata/                 # Sample raw files for examples
│   │   ├── sample_2024.csv
│   │   └── sample_station.csv
│   └── CITATION                 # How to cite the package
│
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── test-data-integrity.R
│       ├── test-import.R
│       └── test-utils.R
│
├── vignettes/
│   ├── intro.Rmd               # Getting started
│   ├── exploring-data.Rmd      # Data exploration examples
│   ├── updating-data.Rmd       # How to update with latest data
│   └── visualization.Rmd       # Plotting examples
│
└── man/                        # Auto-generated documentation
```

---

## Proposed User-Facing API

### 1. Pre-loaded Datasets (Primary Use Case)

```r
library(metrosp)

# Main datasets ready to use
data("metro_passengers")   # Line-level time series 2018-2024
data("metro_stations")     # Station-level time series 2021-2024
data("metro_lines_geo")    # Geographic data (sf object)

# Quick exploration
head(metro_passengers)
summary(metro_stations)
```

### 2. Update Functions (Advanced Users)

```r
# Update with latest data from official sources
passengers <- update_metro_passengers()
stations <- update_metro_stations(year = 2025)

# Download specific line data
line4 <- import_line_4(start_date = "2024-01-01")
```

### 3. Helper Functions

```r
# Get data for specific line
filter_line(metro_passengers, line = "Blue")
filter_line(metro_passengers, line_number = 1)

# Calculate growth rates
calculate_growth(metro_passengers, line = "Blue", metric = "mdu")

# Convert between Portuguese/English names
translate_line_name("Azul")  # Returns "Blue"
translate_metric("Média dos Dias Úteis")  # Returns "Average Weekday"
```

### 4. Visualization Functions

```r
# Quick plots using ggplot2
plot_metro_ridership(line = "Blue", metric = "total")
plot_metro_comparison(lines = c("Blue", "Red", "Green"))
plot_station_ridership(station = "Sé", top_n = 10)

# Geographic visualizations (if sf installed)
map_metro_lines()
map_station_ridership(date = "2024-12-01")
```

### 5. Dimension Tables

```r
# Reference tables
data("dim_lines")      # Line names (PT/EN), numbers, colors, companies
data("dim_stations")   # Station names, line assignments
data("dim_metrics")    # Metric codes and full names
```

---

## Implementation Roadmap

### Phase 1: Package Skeleton (Week 1)

**Tasks:**
1. Create package with `usethis::create_package("metrosp")`
2. Set up Git/GitHub repository
3. Write DESCRIPTION file (title, authors, dependencies)
4. Choose license (suggest MIT)
5. Move existing R code to `data-raw/`
6. Create `.Rbuildignore`, `.gitignore`

**Dependencies to declare:**
```r
Imports:
  dplyr,
  tidyr,
  readr,
  stringr,
  lubridate,
  purrr

Suggests:
  sf,           # For geographic data
  ggplot2,      # For plotting functions
  rvest,        # For web scraping
  pdftools,     # For PDF extraction
  tabulizer,    # For PDF tables
  testthat,     # For testing
  knitr,        # For vignettes
  rmarkdown     # For vignettes
```

### Phase 2: Code Refactoring (Week 1-2)

**Tasks:**
1. Extract reusable functions from scripts
2. Create internal utilities:
   - `str_simplify()` - Text normalization
   - `parse_month_pt()` - Portuguese month parsing
   - `handle_encoding()` - Encoding detection/conversion
3. Separate user-facing from internal functions
4. Add roxygen2 documentation to all functions
5. Create consistent naming convention (snake_case)

**Key Functions to Export:**
- `update_metro_passengers()`
- `update_metro_stations()`
- `import_line_4()`, `import_line_5()`
- `filter_line()`, `filter_station()`
- `plot_metro_*()` functions

### Phase 3: Data Preparation (Week 2)

**Tasks:**
1. Create `data-raw/build-package-data.R` master script
2. Source existing cleaning scripts
3. Generate .rda files with `usethis::use_data()`:
   ```r
   usethis::use_data(metro_passengers, overwrite = TRUE)
   usethis::use_data(metro_stations, overwrite = TRUE)
   usethis::use_data(metro_lines_geo, overwrite = TRUE)
   # etc.
   ```
4. Document datasets with roxygen2 in `R/data.R`
5. Compress data appropriately

### Phase 4: Documentation (Week 2-3)

**Tasks:**
1. Write comprehensive README.md with:
   - Installation instructions
   - Quick start examples
   - Badges (R CMD check, CRAN status)
2. Document all functions with roxygen2:
   - `@param` for all arguments
   - `@return` describing output
   - `@examples` with executable code
   - `@export` for user-facing functions
3. Create pkgdown website configuration
4. Write vignettes:
   - Getting started guide
   - Data exploration examples
   - Visualization examples
   - Updating data tutorial

### Phase 5: Testing (Week 3)

**Tasks:**
1. Set up testthat framework: `usethis::use_testthat()`
2. Write unit tests:
   - Data integrity checks (no NAs where unexpected)
   - Function output types
   - Error handling
   - Date parsing
3. Test against different R versions
4. Run `R CMD check` and fix all NOTEs/WARNINGs
5. Test on different operating systems

**Example Tests:**
```r
test_that("metro_passengers has expected structure", {
  expect_true("data.frame" %in% class(metro_passengers))
  expect_named(metro_passengers, c("year", "date", "variable", ...))
  expect_true(all(metro_passengers$year >= 2018))
})

test_that("filter_line works correctly", {
  blue <- filter_line(metro_passengers, line = "Blue")
  expect_equal(unique(blue$metro_line), "azul")
})
```

### Phase 6: Automation (Week 3)

**Tasks:**
1. Set up GitHub Actions for:
   - R CMD check on push/PR
   - pkgdown website deployment
   - Test coverage reporting
2. Create update schedule (quarterly?)
3. Document update process in vignettes
4. Consider data versioning strategy

### Phase 7: Release Preparation (Week 3-4)

**Tasks:**
1. Write NEWS.md documenting version 0.1.0
2. Create CITATION file
3. Spell check: `usethis::use_spell_check()`
4. Polish README with screenshots/plots
5. Create hex sticker logo (optional but fun!)
6. Submit to CRAN:
   - Run `devtools::check_rhub()`
   - Run `devtools::check_win_devel()`
   - Submit via `devtools::submit_cran()`

---

## Data Update Strategy

### Quarterly Update Cycle

**Suggested Schedule:**
- **January**: Update with full previous year data
- **April**: Q1 update
- **July**: Q2 update
- **October**: Q3 update

### Update Workflow

1. Run data-raw scripts to fetch latest data
2. Update version in DESCRIPTION (0.1.0 → 0.1.1)
3. Rebuild .rda files
4. Update NEWS.md
5. Run tests
6. Tag release on GitHub
7. Submit to CRAN (for major updates)

### Version Policy

- **Major (1.0.0)**: Breaking API changes
- **Minor (0.1.0)**: New features, data fields
- **Patch (0.0.1)**: Bug fixes, data updates

---

## Potential Features (Future Versions)

### v0.2.0 - Enhanced Analytics
- Statistical functions (trend analysis, seasonality)
- Comparison with other world metros
- Holiday effects analysis
- COVID-19 impact quantification

### v0.3.0 - Advanced Visualization
- Interactive plots with plotly
- Shiny dashboard for exploration
- Animated time series
- Network analysis visualizations

### v0.4.0 - Integration
- Integration with GTFS data
- Weather data correlation
- Socioeconomic indicators
- Real-time data API (if available)

### v1.0.0 - Comprehensive Package
- Full test coverage (>90%)
- Comprehensive vignettes
- Published paper in R Journal/JSS
- Established user base
- CRAN submission

---

## Similar Packages for Reference

### Data Packages
- `nycflights13` - Flight data structure
- `gapminder` - Clean datasets approach
- `palmerpenguins` - Documentation style

### Brazilian Packages
- `geobr` - Brazilian geographic data
- `sidrar` - IBGE data access
- `electionsBR` - Brazilian election data

### Transportation Packages
- `stplanr` - Sustainable transport planning
- `gtfsrouter` - GTFS data routing
- `tidytransit` - GTFS manipulation

---

## Documentation Examples

### README.md Preview

```markdown
# metrosp <img src="man/figures/logo.png" align="right" height="139" />

> Easy Access to São Paulo Metro Ridership Data

## Overview

`metrosp` provides clean, ready-to-use datasets of São Paulo Metro ridership from 2018-2024. The package handles the messy work of downloading and cleaning data from multiple sources (Metro SP, ViaQuatro, ViaMobilidade), so you can focus on analysis.

## Installation

```r
# Install from CRAN
install.packages("metrosp")

# Development version from GitHub
# install.packages("devtools")
devtools::install_github("yourusername/metrosp")
```

## Quick Start

```r
library(metrosp)
library(dplyr)
library(ggplot2)

# Load main dataset
data("metro_passengers")

# Filter Blue Line data
blue_line <- metro_passengers %>%
  filter(metro_line == "azul", metric == "mdu")

# Plot trend
ggplot(blue_line, aes(date, value)) +
  geom_line() +
  labs(title = "Blue Line Average Weekday Ridership",
       y = "Passengers (thousands)")
```

## Data Sources

This package aggregates data from:
- [Metro SP Transparency Portal](https://transparencia.metrosp.com.br/)
- [ViaQuatro](https://www.viaquatro.com.br/) (Line 4)
- [ViaMobilidade](https://www.viamobilidade.com.br/) (Line 5)

## Citation

```r
citation("metrosp")
```
```

### Data Documentation Example

```r
#' São Paulo Metro Passenger Data
#'
#' Monthly ridership data for São Paulo Metro lines from 2018 to 2024.
#' Includes total passengers, weekday/weekend averages, and maximum daily values.
#'
#' @format A data frame with 4,421 rows and 8 variables:
#' \describe{
#'   \item{year}{Year (2018-2024)}
#'   \item{date}{First day of month (Date)}
#'   \item{variable}{Either "transport" or "entrance"}
#'   \item{metric}{Metric abbreviation: total, mdu, msa, mdo, max}
#'   \item{metric_label}{Full metric name in Portuguese}
#'   \item{metro_line}{Line name in Portuguese: azul, verde, vermelha, lilas, prata}
#'   \item{metro_line_num}{Line number: 1, 2, 3, 4, 5, 15}
#'   \item{value}{Passenger count in thousands}
#' }
#'
#' @details
#' Metrics:
#' - total: Total monthly passengers
#' - mdu: Average weekday (Média dos Dias Úteis)
#' - msa: Average Saturday (Média dos Sábados)
#' - mdo: Average Sunday (Média dos Domingos)
#' - max: Maximum daily (Máxima Diária)
#'
#' @source
#' Metro SP Transparency Portal: \url{https://transparencia.metrosp.com.br/}
#' ViaQuatro: \url{https://www.viaquatro.com.br/}
#' ViaMobilidade: \url{https://www.viamobilidade.com.br/}
#'
#' @examples
#' data(metro_passengers)
#'
#' # Total ridership by line in 2024
#' library(dplyr)
#' metro_passengers %>%
#'   filter(year == 2024, metric == "total") %>%
#'   group_by(metro_line) %>%
#'   summarise(total = sum(value, na.rm = TRUE))
"metro_passengers"
```

---

## Risks and Mitigation

### Risk 1: Data Source Changes
**Risk**: Websites change structure, breaking scrapers
**Mitigation**:
- Include archived data in package
- Document expected structure
- Provide fallback to manual data files
- Set up monitoring for website changes

### Risk 2: Maintenance Burden
**Risk**: Time required to keep package updated
**Mitigation**:
- Automate as much as possible (GitHub Actions)
- Clear documentation for contributors
- Quarterly update schedule (not monthly)
- Consider community maintenance model

### Risk 3: CRAN Submission
**Risk**: Package rejected from CRAN
**Mitigation**:
- Start with GitHub-only release
- Follow CRAN policies strictly
- Get feedback from R package developers
- Use `rhub` and `R CMD check` extensively

### Risk 4: Data Quality Issues
**Risk**: Errors in source data propagate
**Mitigation**:
- Extensive validation tests
- Document known issues in NEWS.md
- Provide issue reporting guidelines
- Cross-validate against official reports

---

## Next Steps

### Immediate Actions
1. **Decide on package scope**: Data-only vs. data + analysis functions?
2. **Choose license**: MIT (permissive) or GPL-3 (copyleft)?
3. **Set up repository**: GitHub public repo
4. **Create package skeleton**: Run `usethis::create_package("metrosp")`

### Questions to Answer
- Will you maintain this solo or seek contributors?
- Should this include analysis functions or just data?
- Target audience: researchers, students, general public?
- CRAN submission goal or GitHub-only?

### Resources Needed
- ~20-30 hours initial development
- Quarterly maintenance (2-3 hours)
- Optional: Domain name for pkgdown site
- Optional: Hex sticker design

---

## Conclusion

**This is absolutely worth pursuing!** You have:

✅ High-quality, comprehensive code
✅ Clean, valuable datasets
✅ Clear value proposition
✅ No competing packages
✅ Growing interest in urban data

The existing code is already 70% of the way to a package. The remaining 30% is:
- Proper packaging structure
- Documentation
- Testing
- Polish

This could become a valuable resource for researchers, students, urban planners, and data enthusiasts interested in São Paulo's transportation system.

---

**Recommended Timeline**: 3-4 weeks for v0.1.0 release on GitHub, then 1-2 months of refinement before potential CRAN submission.

**Next Document to Create**: `METROSP_IMPLEMENTATION_GUIDE.md` with step-by-step instructions once you're ready to start development.
