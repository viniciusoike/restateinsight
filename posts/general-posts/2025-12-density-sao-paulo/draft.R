library(ggplot2)
library(dplyr)
library(sf)

censo <- readr::read_csv2(
  here::here("static/data/censo2022_basico.csv"),
  locale = readr::locale(encoding = "Windows-1252"),
  na = c(".", "")
)

import::from(stringr, str_wrap)
import::from(readxl, read_excel, excel_sheets)

excel_sheets("static/data/Housing in London 2024 - web.xlsx")

import_excel <- function(path, sheet) {
  readxl::read_excel(
    path = path,
    sheet = sheet,
    skip = 3,
    .name_repair = janitor::make_clean_names
  )
}

get_metadata <- function(path, sheet) {
  readxl::read_excel(
    path = path,
    sheet = sheet,
    range = "A1:B2",
    col_names = c("metadata", "description")
  )
}

path <- "static/data/Housing in London 2024 - web.xlsx"
sheets <- excel_sheets(path)

city_density <- import_excel(path, sheets[3])

# (kind of) replicate plots

tab_density <- city_density |>
  summarise(
    total_dwellings = sum(dwellings),
    total_size = sum(hectares),
    .by = "city"
  ) |>
  mutate(avg_density = total_dwellings / total_size) |>
  arrange(avg_density)

# tab_density

lvls_bins <- unique(city_density$dph_bin)

dwellings_metadata <- get_metadata(path, sheets[3])

density <- city_density |>
  mutate(
    share = dwellings / sum(dwellings),
    .by = "city"
  ) |>
  mutate(dph_bin = factor(dph_bin, levels = rev(lvls_bins)))


get_census_data <- function(city) {
  rename_cols <- c(
    "code_tract" = "CD_SETOR",
    "code_muni" = "CD_MUN",
    "code_state" = "CD_UF",
    "code_district" = "CD_DIST",
    "name_district" = "NM_DIST",
    "code_neighborhood" = "CD_BAIRRO",
    "name_neighborhood" = "NM_BAIRRO",
    "code_subdistrict" = "CD_SUBDIST",
    "name_subdistrict" = "NM_SUBDIST"
  )

  rename_cols_censo <- c(
    "pop" = "v0001",
    "domi_total" = "v0002",
    "domi_parti" = "v0003",
    "domi_colet" = "v0004",
    "media_moradores" = "v0005",
    "percentual_domicilios_imputados" = "v0006",
    "domi_ocup" = "v0007"
  )

  dat <- censo |>
    rename(all_of(rename_cols)) |>
    filter(code_muni == !!city)

  if (city == 3550308) {
    grouping_vars <- c("code_district", "name_district")
  } else if (city == 5300108) {
    grouping_vars <- c("code_subdistrict", "name_subdistrict")
  } else {
    grouping_vars <- c("code_neighborhood", "name_neighborhood")
  }

  dat <- dat |>
    summarise(
      across(c(v0001:v0004, v0007), ~ sum(.x, na.rm = TRUE)),
      .by = all_of(grouping_vars)
    ) |>
    rename(any_of(rename_cols_censo))

  if (nrow(dat) == 1) {
    cli::cli_warn("Summarizing returned a single row. Check results.")
  }

  return(dat)
}

get_district_shapes <- function(city) {
  setores <- geobr::read_census_tract(
    city,
    year = 2022,
    showProgress = FALSE
  )

  if (city == 3550308) {
    grouping_vars <- c("code_district", "name_district")
  } else if (city == 5300108) {
    grouping_vars <- c("code_subdistrict", "name_subdistrict")
  } else {
    grouping_vars <- c("code_neighborhood", "name_neighborhood")
  }

  bairros_tamanhos <- setores |>
    st_drop_geometry() |>
    as_tibble() |>
    summarise(
      area_bairro = sum(area_km2, na.rm = TRUE),
      .by = all_of(c(grouping_vars, "name_muni"))
    )

  return(bairros_tamanhos)
}

format_table <- function(census_data, districts) {
  name_city <- unique(districts$name_muni)

  if (length(name_city) == 0) {
    cli::cli_abort("Missing city name from districts file.")
  }

  density_breaks <- c(0, 25, 50, 100, 200)

  join_variables <- names(districts)[1:2]

  dat <- left_join(
    census_data,
    districts,
    by = join_variables
  )

  check_missing <- sum(is.na(dat$area_bairro))

  if (length(check_missing) > 0 && check_missing > 0) {
    cli::cli_warn("Left-join caused {check_missing} NAs.")
  }

  dat <- dat |>
    mutate(
      across(
        pop:domi_ocup,
        ~ (.x / area_bairro) * 1 / 100,
        .names = "{.col}_ha"
      )
    ) |>
    mutate(density_band = findInterval(domi_total_ha, density_breaks))

  dat <- dat |>
    summarise(
      total_domi = sum(domi_total, na.rm = TRUE),
      total_area = sum(area_bairro, na.rm = TRUE) * 100,
      areas = n(),
      .by = "density_band"
    ) |>
    arrange(density_band)

  bins <- c(
    "0 to <25dph" = 1,
    "25 to <50dph" = 2,
    "50 to <100dph" = 3,
    "100 to <200dph" = 4,
    "200+dph" = 5
  )

  available_bins <- unique(dat$density_band)

  classify_bins <- function(x) {
    names(bins)[x]
  }

  fmt_dat <- dat |>
    mutate(
      dph_bin = classify_bins(density_band),
      dph_bin = factor(dph_bin, levels = names(bins)[available_bins]),
      share = total_domi / sum(total_domi),
      city = local(name_city)
    ) |>
    select(
      city,
      dph_bin,
      dwellings = total_domi,
      hectares = total_area,
      areas,
      share
    )

  return(fmt_dat)
}

process_data <- function(city) {
  cli::cli_alert_info("Importing census data for {city}.")
  census <- get_census_data(city)
  cli::cli_alert_info("Importing district data")
  dstr <- get_district_shapes(city)
  cli::cli_alert_info("Formatting density data")
  dat <- format_table(census, dstr)

  return(dat)
}
# Porto Alegre - 4314902
# Brasília - 5300108
# Rio de Janeiro - 3304557
# São Paulo - 3550308

cities_codes <- c(
  "Porto Alegre" = 4314902,
  "Brasília" = 5300108,
  "Rio de Janeiro" = 3304557,
  "São Paulo" = 3550308
)

brazil_density <- purrr::map(cities_codes, process_data)
tab_brazil_density <- bind_rows(brazil_density)

tab_density <- bind_rows(density, tab_brazil_density)

lvls_cities <- tab_density |>
  filter(dph_bin == "0 to <25dph") |>
  arrange(desc(share)) |>
  pull(city)

tab_density <- tab_density |>
  mutate(
    label_highlight = if_else(
      dph_bin %in% lvls_bins[c(1, 2, 5)],
      scales::number(share, accuracy = 0.1, scale = 100, suffix = "%"),
      NA_character_
    ),
    city = factor(city, levels = lvls_cities)
  )

col_purples <- c(
  "#bfd3e6",
  "#8c96c6",
  "#8c6bb1",
  "#88419d",
  "#810f7c"
)

ggplot(tab_density, aes(x = share, y = city)) +
  geom_col(aes(fill = dph_bin), width = 0.8, color = "white", lwd = 0.1) +
  geom_vline(xintercept = 0, color = "gray70") +
  geom_label(
    aes(label = label_highlight),
    position = position_stack(vjust = 0.5)
  ) +
  geom_label(
    data = tibble(
      city = "New York City",
      share = c(0.15, 0.75),
      label = c("Low Density\nNeighbouhoods", "High Density\nNeighbourhoods")
    ),
    aes(x = share, y = city, label = label),
    hjust = 0,
    nudge_y = 1,
    family = "Avenir",
    size = 3
  ) +
  scale_x_continuous(
    expand = expansion(0),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent,
    limits = c(0, 1.05)
  ) +
  scale_y_discrete(expand = expansion(c(0.06, 0.25))) +
  scale_fill_manual(name = "", values = rev(col_purples)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    subtitle = str_wrap(
      paste(
        dwellings_metadata$description[1],
        "(incl. Brasília, Rio de Janeiro, Porto Alegre, and São Paulo)."
      ),
      91
    ),
    title = "",
    x = NULL,
    y = NULL,
    caption = "Source: GLA (Greater London Authority). IBGE (Demographic Census, 2022)."
  ) +
  theme_minimal(base_family = "Avenir") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    plot.margin = margin(15, 20, 15, 20),
    plot.caption = element_text(size = 8, color = "gray60", hjust = 0),
    plot.title = element_text(size = 12, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0, color = "gray20"),
    axis.text.y = element_text(size = 11, color = "gray30"),
    axis.text.x = element_text(size = 10, color = "gray30"),
    axis.ticks.x = element_line(color = "gray30", linewidth = 0.1),
    plot.caption.position = "plot",
    plot.title.position = "plot"
  )

tab_city_density <- tab_density |>
  summarise(
    total_dwellings = sum(dwellings),
    total_size = sum(hectares),
    .by = "city"
  ) |>
  mutate(avg_density = total_dwellings / total_size) |>
  arrange(avg_density)

# density_breaks <- c(0, 25, 50, 100, 200)
#
# spo_dist <- spo_dist |>
#   left_join(bairros_tamanhos) |>
#   mutate(
#     across(pop:domi_ocup, ~ (.x / area_bairro) * 1 / 100, .names = "{.col}_ha")
#   ) |>
#   mutate(
#     density_band = findInterval(domi_total_ha, density_breaks)
#   )
#
# tab_spo <- spo_dist |>
#   summarise(
#     total_domi = sum(domi_total),
#     total_pop = sum(pop),
#     total_area = sum(area_bairro) * 100,
#     m0 = min(domi_total_ha),
#     m1 = max(domi_total_ha),
#     areas = n(),
#     .by = "density_band"
#   ) |>
#   arrange(density_band)
#
# density_spo <- tab_spo |>
#   mutate(
#     dph_bin = factor(density_band, labels = lvls_bins[1:4]),
#     share = total_domi / sum(total_domi),
#     city = "São Paulo"
#   ) |>
#   select(
#     city,
#     dph_bin,
#     dwellings = total_domi,
#     hectares = total_area,
#     areas,
#     share
#   )
