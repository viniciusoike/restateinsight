library(sidrar)
library(sf)
library(geobr)
library(dplyr)
library(stringr)

births <- get_sidra(
  2609,
  variable = 217,
  period = "2022",
  classific = "c2",
  category = list(0),
  geo = "City"
)

deaths <- get_sidra(
  2654,
  variable = 343,
  period = "2022",
  classific = "c244",
  category = list(0),
  geo = "City"
)

pop <- get_sidra(4709, variable = 93, period = "2022", geo = "City")

borders <- read_municipality(year = 2022, showProgress = FALSE)
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

pop_records <- borders |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  left_join(tbl_pop, by = "code_muni") |> 
  left_join(tbl_deaths, by = "code_muni") |> 
  left_join(tbl_births, by = "code_muni")

pop_records <- pop_records |> 
  mutate(
    cbr = births / population * 1000,
    cdr = deaths / population * 1000
  ) |> 
  filter(!is.na(cbr) | !is.na(cdr)) |> 
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

br_records <- borders |> 
  select(code_muni) |> 
  left_join(pop_records, by = "code_muni")

qs::qsave(br_records, here::here("static/data/birth_death_rate.qs"))