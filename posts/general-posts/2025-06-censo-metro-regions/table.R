library(dplyr)
library(stringr)
import::from(tidyr, pivot_wider)
import::from(sidrar, get_sidra)

# Import shapefiles to get city/metro-region identifiers

metro_region <- geobr::read_metro_area(showProgress = FALSE)
dim_metro <- as_tibble(sf::st_drop_geometry(metro_region))
dim_metro <- select(dim_metro, code_muni, name_metro)

cities <- geobr::read_municipality(year = 2020, showProgress = FALSE)
states <- geobr::read_state(year = 2020, showProgress = FALSE)
dim_state <- as_tibble(sf::st_drop_geometry(states))
dim_city <- as_tibble(sf::st_drop_geometry(cities))

dim_city <- left_join(dim_city, dim_metro, by = "code_muni")

#> Remove an exception: Murici is mapped into two different metro regions
dim_city <- dim_city |>
  filter(!(code_muni == 2705507 & name_metro == "RM da Zona da Mata"))

# Import population tables from SIDRA

# Census population count (2022)
pop2022 <- get_sidra(4709, variable = 93, geo = "City")

# Previous Census population counts (1991, 2000, 2010)
pop_series <- get_sidra(
  136,
  variable = 93,
  period = "1991-2010",
  geo = "City",
  classific = c("c86"),
  category = list(0)
)

# Generic function to clean SIDRA tables
clean_sidra <- function(df, name_value = "pop") {
  cols <- c("code_muni" = "municipio_codigo", "year" = "ano", x = "valor")
  names(cols)[3] <- name_value

  df |>
    as_tibble() |>
    janitor::clean_names() |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
}

# Clean SIDRA tables
cpop_series <- clean_sidra(pop_series)
cpop2022 <- clean_sidra(pop2022)

# Stack both tables
census_pop <- rbind(cpop_series, cpop2022)
census_pop <- left_join(census_pop, dim_city, by = "code_muni")

shorten_ride_names <- function(x) {
  ride_names <- c(
    "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno" = "RIDE DF",
    "RIDE TERESINA - Região Integrada de Desenvolvimento da Grande Teresina" = "RIDE Teresina",
    "RIDE Petrolina/Juazeiro Região Administrativa Integrada de Desenvolvimento do Polo Petrolina/PE e Juazeiro/BA" = "RIDE Petrolina/Juazeiro"
  )

  unname(ride_names[x])
}

# Clean/simplify the name of the metro regions

tbl_pop_metro <- census_pop |>
  filter(!is.na(name_metro)) |>
  mutate(
    name_metro = str_remove(name_metro, "RM d[a-z]"),
    name_metro = str_remove(name_metro, "RM"),
    name_metro = str_replace(name_metro, "Aglomeração Urbana", "AU"),
    name_metro = ifelse(
      str_detect(name_metro, "^RIDE"),
      shorten_ride_names(name_metro),
      name_metro
    ),
    name_metro = str_trim(name_metro, side = "both")
  )

tbl_pop_metro <- tbl_pop_metro |>
  summarise(
    total = sum(pop, na.rm = TRUE),
    state = paste(unique(abbrev_state), collapse = ", "),
    .by = c("year", "name_metro")
  ) |>
  arrange(year) |>
  arrange(name_metro)

# Get only metro regions that had over 500k inhabitants in 2022
big_metros <- subset(tbl_pop_metro, year == 2022 & total > 5 * 1e5)$name_metro

# Get only 500k cities and compute average geometric growth
tbl_major <- tbl_pop_metro |>
  filter(name_metro %in% big_metros) |>
  # Not so trivial since year intervals are not the same between Census editions
  group_by(name_metro) |>
  mutate(
    tcg = (total / lag(total))^(1 / (year - lag(year) + 1)) - 1,
    total_round = round(total / 1000)
  ) |>
  ungroup()

# Convert to wider and arrange by 2022 population
tbl_major_wide <- tbl_major |>
  pivot_wider(
    id_cols = c("name_metro", "state"),
    names_from = "year",
    values_from = c("total_round", "tcg")
  ) |>
  select(-tcg_1991) |>
  arrange(desc(total_round_2022))

qs::qsave(
  list(metro = tbl_major, metro_wide = tbl_major_wide),
  here::here("posts/general-posts/2025-06-censo-metro-regions/table.qs")
)

# Split the larger metro tables into smaller regions views
# to make the interpretation cleaner

tbl_pop_metro <- left_join(
  tbl_pop_metro,
  dim_state,
  by = c("state" = "abbrev_state")
)

tbl_pop_metro <- tbl_pop_metro %>%
  mutate(
    code_region = case_when(
      name_metro == "RIDE DF" ~ 5,
      name_metro == "RIDE Teresina" ~ 2,
      name_metro == "RIDE Petrolina/Juazeiro" ~ 2,
      TRUE ~ code_region
    ),
    name_region = case_when(
      name_metro == "RIDE DF" ~ "Centro Oeste",
      name_metro == "RIDE Teresina" ~ "Nordeste",
      name_metro == "RIDE Petrolina/Juazeiro" ~ "Nordeste",
      TRUE ~ name_region
    )
  )

tbl_full <- tbl_pop_metro |>
  # Not so trivial since year intervals are not the same between Census editions
  group_by(name_metro) |>
  mutate(
    tcg = (total / lag(total))^(1 / (year - lag(year) + 1)) - 1,
    total_round = round(total / 1000) * 1000
  ) |>
  ungroup()

tbl_full <- tbl_full |>
  mutate(
    name_metro = str_remove(name_metro, " - Rio Grande do Sul")
  )

tbl_full_wide <- tbl_full |>
  pivot_wider(
    id_cols = c(
      "name_metro",
      "state",
      "code_state",
      "name_region",
      "code_region"
    ),
    names_from = "year",
    values_from = c("total_round", "tcg")
  ) |>
  select(-tcg_1991) |>
  arrange(desc(total_round_2022))

sub_region <- tbl_full %>%
  filter(code_region == 4)

sub_region_wide <- tbl_full_wide %>%
  filter(code_region == 4) %>%
  select(name_metro, code_state, total_round_2022, starts_with("tcg"))

timeline <- sub_region %>%
  summarise(
    TCG = list(na.omit(c(tcg * 100))),
    .by = "name_metro"
  )

col_names <- c(
  "Região Metro",
  "Pop. 2022",
  "1991/2000",
  "2000/2010",
  "2010/2022"
)

names(col_names) <- names(sub_region_wide)

gt_theme_simple <- function(dat) {
  fmt_table <- dat |>
    opt_table_font(font = "DIN Alternate") |>
    tab_options(
      column_labels.background.color = "#fefefe",
      data_row.padding = px(4),
      column_labels.font.size = px(16),
      table.font.size = px(14),
      table.font.color = "#374151",

      column_labels.border.top.style = "solid",
      column_labels.border.top.width = px(2),
      #column_labels.border.top.color = "#1B365D",
      column_labels.border.top.color = "#000000",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "#E5E7EB",

      stub_border.style = "solid",
      stub_border.width = px(2),
      stub_border.color = "#E5E7EB",

      # Row styling
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#FAFBFC",

      # Borders
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = px(2),
      heading.border.bottom.color = "#000000",
      table.border.top.style = "solid",
      table.border.top.width = px(2),
      table.border.top.color = "#000000",
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      table.border.bottom.color = "#1B365D",

      # Source notes
      source_notes.font.size = px(10),
      source_notes.border.lr.style = "none",
      source_notes.padding = px(10)
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#fefefe", weight = "bold"),
        cell_fill(color = "#1B365D")
      ),
      locations = cells_column_labels()
    ) %>%
    # Style spanners
    tab_style(
      style = list(
        cell_text(weight = "bold", color = "#fefefe"),
        cell_fill(color = "#1B365D")
      ),
      locations = cells_column_spanners()
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "#E5E7EB",
        weight = px(1)
      ),
      locations = cells_body()
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#6B7280")
      ),
      location = cells_source_notes()
    )
}

table_region <- function(dat, code) {
  subdat <- dat %>%
    filter(code_region %in% code) %>%
    select(name_metro, total_round_2022, starts_with("tcg"))

  q <- quantile(
    subdat$tcg_2022[subdat$tcg_2022 > 0],
    probs = c(0.25, 0.5, 0.75)
  )

  subdat <- subdat %>%
    mutate(
      class = factor(findInterval(tcg_2022, c(0, q)))
    )

  timeline <- tbl_full %>%
    filter(code_region %in% code) %>%
    summarise(
      TCG = list(na.omit(c(tcg * 100))),
      .by = "name_metro"
    )

  subdat %>%
    left_join(timeline) %>%
    gt() %>%
    cols_label(.list = col_names) %>%
    cols_hide(columns = "class") %>%
    tab_spanner("Crescimento (%)", columns = 3:5) %>%
    tab_header(
      title = "Crescimento demográfico",
      subtitle = "Crescimento populacional entre os Censos"
    ) %>%
    fmt_number(starts_with("total"), decimals = 0, sep_mark = ".") %>%
    fmt_percent(starts_with("tcg"), decimals = 2, dec_mark = ",") %>%
    ## Target Timeline column
    gt_plt_sparkline(
      column = TCG,
      palette = c("gray30", "black", "firebrick", "dodgerblue", "lightgrey"),
      fig_dim = c(5, 28)
    ) %>%
    gt_theme_simple() %>%
    data_color(
      columns = class,
      target_columns = tcg_2022,
      palette = c("#E63946", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
    )
}


table_state <- function(dat, code) {
  subdat <- dat %>%
    filter(code_state == code) %>%
    select(name_metro, total_round_2022, starts_with("tcg"))

  timeline <- tbl_full %>%
    filter(code_state %in% code) %>%
    summarise(
      TCG = list(na.omit(c(tcg * 100))),
      .by = "name_metro"
    )

  subdat %>%
    left_join(timeline) %>%
    gt() %>%
    cols_label(.list = col_names) %>%
    tab_spanner("Crescimento (%)", columns = 3:5) %>%
    fmt_number(starts_with("total"), decimals = 0, sep_mark = ".") %>%
    fmt_percent(starts_with("tcg"), decimals = 2, dec_mark = ",") %>%
    ## Target Timeline column
    gt_plt_sparkline(
      column = TCG,
      palette = c("gray30", "black", "firebrick", "dodgerblue", "lightgrey"),
      fig_dim = c(5, 28)
    ) %>%
    gt_theme_538() %>%
    gt_color_rows(
      columns = tcg_2022,
      domain = seq(-0.005, 0.03, 0.005),
      #domain = c(min(metro_wide$tcg_2022), 0, max(metro_wide$tcg_2022)),
      palette = c(
        "#E63946",
        "#F1FAEE",
        "#CDEAE5",
        "#BBE2E1",
        "#A8DADC",
        "#90C3CD",
        "#77ABBD",
        "#457B9D",
        "#31587A",
        "#1D3557"
      )
    )
}

table_state(sub_region_wide, 41)
table_region(tbl_full_wide, 3)


sub_region_wide %>%
  left_join(timeline) %>%
  gt() %>%
  cols_label(.list = col_names) %>%
  tab_spanner("Crescimento (%)", columns = 4:6) %>%
  fmt_number(starts_with("total"), decimals = 0, sep_mark = ".") %>%
  fmt_percent(starts_with("tcg"), decimals = 2) %>%
  ## Target Timeline column
  gt_plt_sparkline(
    column = TCG,
    palette = c("gray30", "black", "firebrick", "dodgerblue", "lightgrey"),
    fig_dim = c(5, 28)
  ) %>%
  gt_theme_538() %>%
  gt_color_rows(
    columns = tcg_2022,
    domain = seq(-0.005, 0.03, 0.005),
    #domain = c(min(metro_wide$tcg_2022), 0, max(metro_wide$tcg_2022)),
    palette = c(
      "#E63946",
      "#F1FAEE",
      "#CDEAE5",
      "#BBE2E1",
      "#A8DADC",
      "#90C3CD",
      "#77ABBD",
      "#457B9D",
      "#31587A",
      "#1D3557"
    )
  )
