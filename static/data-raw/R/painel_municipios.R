library(sidrar)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)

import_sidra <- function(x, variable, geo) {
  
  tab <- get_sidra(x = x, variable = variable, geo = geo)
  
  clean_tab <- clean_sidra(tab)
  
  return(clean_tab)
  
}

clean_sidra <- function(dat) {
  
  rename_cols <- c(
    "code_muni" = "municipio_codigo",
    "variable" = "variavel",
    "value" = "valor"
  )
  
  sel_cols <- c("code_muni", "name_muni", "abbrev_state", "variable", "value")
  
  dat |> 
    janitor::clean_names() |> 
    dplyr::rename(dplyr::any_of(rename_cols)) |> 
    tidyr::separate(
      municipio,
      into = c("name_muni", "abbrev_state"),
      sep = " - ") |>
    dplyr::select(dplyr::any_of(sel_cols))
  
}

#> Set parametres to import Census tables (2022)
#> Imports population and household data for all cities
params <- tribble(
  ~x, ~variable, ~geo, ~name_simplified,
  4714, 93, "City", "population",
  4709, 5936, "City", "population_growth",
  4709, 10605, "City", "population_growth_rate",
  4714, 6318, "City", "city_area",
  4714, 614, "City", "population_density",
  4712, 381, "City", "households",
  4712, 5930, "City", "dwellers_per_household"
)

#> Imports and cleans data and stores as tibbles
tables <- params |> 
  group_by(name_simplified) |> 
  mutate(tables_sidra = pmap(list(x, variable, geo), import_sidra))

#> Set parameters to import city GDP data (Contas Nacionais)
#> Imports most recent GDP data for all cities
params <- tribble(
  ~x, ~variable, ~geo, ~name_simplified,
  5938, 37, "City", "pib",
  5938, 497, "City", "pib_share_uf",
  5938, 543, "City", "pib_taxes",
  5938, 498, "City", "pib_added_value",
  5938, 513, "City", "pib_agriculture",
  5938, 517, "City", "pib_industrial",
  5938, 6575, "City", "pib_services",
  5938, 525, "City", "pib_govmt_services"
)

#> Imports and cleans data and stores as tibbles
tables_pib <- params |> 
  group_by(name_simplified) |> 
  mutate(tables_sidra = pmap(list(x, variable, geo), import_sidra))

#> Join all tables and pivot wider
#> 

dat <- bind_rows(tables, tables_pib)

dim_state <- geobr::read_state(showProgress = FALSE)
dim_state <- as_tibble(sf::st_drop_geometry(dim_state))

#' Brazilian Cities Table
#'
#' A compilation of demographic and economic information on Brazilian Cities.
#' Data is the most up to date available. All datasets come from the Brazilian
#' Institute of Geography and Statistics (IBGE)
#'
#' @format ## `cities_brazil`
#' A data frame with 5560 rows and 22 columns:
#' \describe{
#'   \item{code_muni}{Country name}
#'   \item{name_muni}{2 & 3 letter ISO country codes}
#'   \item{code_state}{Year}
#'   \item{name_state}{}
#'   \item{abbrev_state}{}
#'   \item{code_region}{}
#'   \item{name_region}{}
#'   \item{population}{Total inhabitants in the city}
#'   \item{population_growth}{}
#'   \item{population_growth_rate}{}
#'   \item{city_area}{Total area of the city in km2}
#'   \item{}{}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
cities_brazil <- dat |> 
  ungroup() |> 
  rename(code_sidra = x, variable_sidra = variable) |> 
  select(-geo) |> 
  unnest(cols = "tables_sidra") |> 
  pivot_wider(
    id_cols = c("code_muni", "name_muni", "abbrev_state"),
    names_from = "name_simplified",
    values_from = "value"
  ) |> 
  left_join(dim_state, by = c("abbrev_state")) |> 
  select(
    code_muni, name_muni, code_state, name_state, abbrev_state,
    code_region, name_region, everything()
  )

readr::write_csv(cities_brazil, here::here("static/data", "cities_brazil.csv"))
