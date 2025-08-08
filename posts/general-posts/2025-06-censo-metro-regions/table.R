library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
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

# # Get only metro regions that had over 500k inhabitants in 2022
# big_metros <- subset(tbl_pop_metro, year == 2022 & total > 5 * 1e5)$name_metro

# # Get only 500k cities and compute average geometric growth
# tbl_major <- tbl_pop_metro |>
#   filter(name_metro %in% big_metros) |>
#   # Not so trivial since year intervals are not the same between Census editions
#   group_by(name_metro) |>
#   mutate(
#     tcg = (total / lag(total))^(1 / (year - lag(year) + 1)) - 1,
#     total_round = round(total / 1000)
#   ) |>
#   ungroup()

# # Convert to wider and arrange by 2022 population
# tbl_major_wide <- tbl_major |>
#   pivot_wider(
#     id_cols = c("name_metro", "state"),
#     names_from = "year",
#     values_from = c("total_round", "tcg")
#   ) |>
#   select(-tcg_1991) |>
#   arrange(desc(total_round_2022))

# qs::qsave(
#   list(metro = tbl_major, metro_wide = tbl_major_wide),
#   here::here("posts/general-posts/2025-06-censo-metro-regions/table.qs")
# )

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

qs::qsave(
  list(metro = tbl_full, metro_wide = tbl_full_wide),
  here::here("posts/general-posts/2025-06-censo-metro-regions/table.qs")
)


timeline <- tbl_full %>%
  summarise(
    TCG = list(na.omit(c(tcg * 100))),
    .by = "name_metro"
  )

tbl_big_metros <- tbl_full_wide |>
  filter(total_round_2022 > 1000) |>
  select(name_metro, starts_with("total"), starts_with("tcg")) |>
  select(-total_round_1991) |>
  mutate(across(starts_with("total"), ~ round(.x / 1000)))

vals <- c(
  tbl_big_metros$tcg_2000,
  tbl_big_metros$tcg_2010,
  tbl_big_metros$tcg_2022
)

breaks <- quantile(vals, probs = c(0.2, 0.4, 0.6, 0.8))

tbl_big_metros <- tbl_big_metros |>
  mutate(across(
    tcg_2000:tcg_2022,
    ~ findInterval(.x, breaks),
    .names = "{.col}_class"
  ))

tbl_big_metros <- left_join(tbl_big_metros, timeline)

gt_colnames <- c(
  "name_metro" = "Região Metro",
  "total_round_2000" = "2000",
  "total_round_2010" = "2010",
  "total_round_2022" = "2022",
  "tcg_2000" = "1991/2000",
  "tcg_2010" = "2000/2010",
  "tcg_2022" = "2010/2022",
  "TCG" = "TCG"
)

pal_rdbu <- c("#E63946", as.character(ekioplot::ekio_palette("modern_premium")))

gt(tbl_big_metros) %>%
  cols_hide(columns = ends_with("class")) %>%
  cols_label(.list = gt_colnames) %>%
  tab_spanner("Populacao (Mil)", columns = 2:4) %>%
  tab_spanner("Crescimento (%)", columns = 5:7) %>%
  fmt_number(starts_with("total"), decimals = 0, sep_mark = ".") %>%
  fmt_percent(starts_with("tcg"), decimals = 2, dec_mark = ",") %>%
  gt_plt_sparkline(
    column = TCG,
    palette = c("gray30", "black", "firebrick", "dodgerblue", "lightgrey"),
    fig_dim = c(5, 28)
  ) %>%
  data_color(
    columns = c(tcg_2000_class, tcg_2010_class, tcg_2022_class),
    target_columns = c(tcg_2000, tcg_2010, tcg_2022),
    palette = pal_rdbu,
    domain = c(0, 4)
  ) %>%
  gt_theme_ekio()

table_region <- function(dat, code) {
  subdat <- dat %>%
    filter(code_region %in% code) %>%
    select(name_metro, state, total_round_2022, starts_with("tcg"))

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

  col_names <- c(
    "Região Metro",
    "Estado",
    "Pop. 2022",
    "1991/2000",
    "2000/2010",
    "2010/2022"
  )

  gt_colnames <- col_names
  names(gt_colnames) <- names(subdat)[1:length(col_names)]

  subdat %>%
    left_join(timeline) %>%
    gt() %>%
    cols_label(.list = gt_colnames) %>%
    cols_hide(columns = "class") %>%
    tab_spanner("Crescimento (%)", columns = 4:6) %>%
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
    gt_theme_ekio() %>%
    data_color(
      columns = class,
      target_columns = tcg_2022,
      palette = c("#E63946", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
    )
}

table_state <- function(dat, code) {
  subdat <- dat %>%
    filter(code_state %in% code) %>%
    select(name_metro, total_round_2022, starts_with("tcg"))

  timeline <- tbl_full %>%
    filter(code_state %in% code) %>%
    summarise(
      TCG = list(na.omit(c(tcg * 100))),
      .by = "name_metro"
    )

  gt_colnames <- c(
    "Região Metro",
    "Pop. 2022",
    "1991/2000",
    "2000/2010",
    "2010/2022"
  )

  names(gt_colnames) <- names(subdat)[1:length(gt_colnames)]

  subdat <- left_join(subdat, timeline)

  gt(subdat) %>%
    cols_hide(columns = starts_with("class")) %>%
    cols_label(.list = gt_colnames) %>%
    tab_spanner("Crescimento (%)", columns = 3:5) %>%
    fmt_number(starts_with("total"), decimals = 0, sep_mark = ".") %>%
    fmt_percent(starts_with("tcg"), decimals = 2, dec_mark = ",") %>%
    ## Target Timeline column
    gt_plt_sparkline(
      column = TCG,
      palette = c("gray30", "black", "firebrick", "dodgerblue", "lightgrey"),
      fig_dim = c(5, 28)
    ) %>%
    gt_theme_ekio() %>%
    data_color(
      columns = tcg_2000:tcg_2022,
      palette = pal_rdbu,
      method = "quantile",
      quantiles = 5
    )
}

# Export tables

codes <- list(41, 42, 42, c(31:35))

state_tables <- lapply(codes, \(c) table_state(tbl_full_wide, code = c))

names(state_tables) <- c(
  "parana",
  "santa_catarina",
  "rio_grande_do_sul",
  "sudeste"
)
