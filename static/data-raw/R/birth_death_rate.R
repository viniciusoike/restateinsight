library(sidrar)
library(sf)
library(geobr)
library(dplyr)
library(stringr)

year_ref <- "2022"

births <- get_sidra(
  2609,
  variable = 217,
  period = year_ref,
  classific = "c2",
  category = list(0),
  geo = "City"
)

deaths <- get_sidra(
  2654,
  variable = 343,
  period = year_ref,
  classific = "c244",
  category = list(0),
  geo = "City"
)

pop <- get_sidra(4709, variable = 93, period = year_ref, geo = "City")

borders <- read_municipality(year = as.numeric(year_ref), showProgress = FALSE)
borders <- mutate(borders, code_muni = as.character(code_muni))

tbl_pop <- pop |> 
  janitor::clean_names() |> 
  as_tibble() |> 
  select(code_muni = municipio_codigo, population = valor)

tbl_deaths <- deaths |> 
  janitor::clean_names() |> 
  as_tibble() |> 
  select(code_muni = municipio_codigo, deaths = valor)

tbl_births <- births |> 
  janitor::clean_names() |> 
  as_tibble() |> 
  select(code_muni = municipio_codigo, births = valor)

br_records <- borders |>
  left_join(tbl_pop, by = "code_muni") |>
  left_join(tbl_deaths, by = "code_muni") |>
  left_join(tbl_births, by = "code_muni") |>
  mutate(
    cbr = births / population * 1000,
    cdr = deaths / population * 1000
  ) |>
  filter(!is.na(cbr) & !is.na(cdr)) |>
  mutate(
    cbr_scaled_country = findInterval(cbr, BAMMtools::getJenksBreaks(cbr, 4), rightmost.closed = TRUE),
    cdr_scaled_country = findInterval(cdr, BAMMtools::getJenksBreaks(cdr, 4), rightmost.closed = TRUE),
    bi_class = factor(str_c(cbr_scaled_country, "-", cdr_scaled_country))
  ) |>
  mutate(
    cbr_scaled_state = findInterval(cbr, BAMMtools::getJenksBreaks(cbr, 4), rightmost.closed = TRUE),
    cdr_scaled_state = findInterval(cdr, BAMMtools::getJenksBreaks(cdr, 4), rightmost.closed = TRUE),
    bi_class_state = factor(str_c(cbr_scaled_state, "-", cdr_scaled_state)),
    .by = "code_state"
  )

qs::qsave(br_records, here::here("static/data/birth_death_rate.qs"))