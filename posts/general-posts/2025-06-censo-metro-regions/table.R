library(dplyr)
library(stringr)
import::from(tidyr, pivot_wider)
import::from(sidrar, get_sidra)

# Import shapefiles to get city/metro-region identifiers

metro_region <- geobr::read_metro_area(showProgress = FALSE)
dim_metro <- as_tibble(sf::st_drop_geometry(metro_region))
dim_metro <- select(dim_metro, code_muni, name_metro)

cities <- geobr::read_municipality(year = 2020, showProgress = FALSE)
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
    tcg = (total / lag(total))^(1/(year - lag(year) + 1)) - 1,
    total_round = round(total / 1000)) |>
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
