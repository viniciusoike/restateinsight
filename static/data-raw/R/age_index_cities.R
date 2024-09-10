#> Crate a table with the Age Index for all Brazilian cities 1991-2022

library(sidrar)
library(dplyr)
library(tidyr)

#> The Census table for 2022 has the Aging Index directly
census22 <- get_sidra(9515, variable = 10612, geo = "City")

tbl_census <- census22 |> 
  janitor::clean_names() |> 
  as_tibble() |> 
  select(year = ano, code_muni = municipio_codigo, age_index = valor) |> 
  mutate(code_muni = as.numeric(code_muni))

#> Past census don't have this information. Download each census separately

#> Age codes for young and elder
age_codes <- c(1140:1142, 1153:1155, 2503)

get_population <- function(year) {
  
  get_sidra(
    200,
    period = as.character(year),
    variable = 93,
    classific = "c58",
    category = list(age_codes),
    geo = "City"
  ) 
}
#> Download in parallel
pop <- parallel::mclapply(c(1991, 2000, 2010), get_population)

tbl_pop <- bind_rows(pop)

x <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")

tbl_pop <- tbl_pop |> 
  janitor::clean_names() |> 
  as_tibble() |> 
  rename(code_muni = municipio_codigo, year = ano) |> 
  filter(sexo == "Total") |> 
  mutate(
    code_muni = as.numeric(code_muni),
    age_group = if_else(grupo_de_idade %in% x, "young", "elder"),
    age_group = factor(age_group, levels = c('young', 'elder'))
    )

tbl_age_index <- tbl_pop |> 
  summarise(
    population = sum(valor, na.rm = TRUE),
    .by = c("year", "code_muni", "age_group")
    ) |> 
  mutate(
    share = population / sum(population, na.rm = TRUE),
    .by = c("code_muni", "year")
    ) |> 
  mutate(age_index = 1 / share - 1, age_index = age_index * 100) |> 
  filter(age_group == "young") |> 
  select(year, code_muni, age_index)

#> Join both tables

tbl_age_index <- bind_rows(list(tbl_age_index, tbl_census))

tbl_age_index <- tbl_age_index |> 
  pivot_wider(
    id_cols = "code_muni",
    names_from = "year",
    names_prefix = "age_index_",
    values_from = "age_index"
  )

dim_muni <- geobr::read_municipality(year = 2020, showProgress = FALSE)

age_muni <- left_join(dim_muni, tbl_age_index, by = "code_muni")

readr::write_rds(
  age_muni, here::here("static/data/census_aging_index_city.rds")
  )


#> The Census table for 2022 has the Aging Index directly
census22 <- get_sidra(9515, variable = 10612)

tbl_census <- census22 |> 
  janitor::clean_names() |> 
  as_tibble() |> 
  select(year = ano, age_index = valor)

#> Past census don't have this information. Download each census separately

#> Age codes for young and elder
age_codes <- c(1140:1142, 1153:1155, 2503)

get_population <- function(year) {
  
  get_sidra(
    200,
    period = as.character(year),
    variable = 93,
    classific = "c58",
    category = list(age_codes),
    geo = "Brazil"
  ) 
}
#> Download in parallel
pop <- parallel::mclapply(c(1991, 2000, 2010), get_population)

tbl_pop <- bind_rows(pop)

x <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")

tbl_pop <- tbl_pop |> 
  janitor::clean_names() |> 
  as_tibble() |> 
  rename(year = ano) |> 
  filter(sexo == "Total") |> 
  mutate(
    age_group = if_else(grupo_de_idade %in% x, "young", "elder"),
    age_group = factor(age_group, levels = c('young', 'elder'))
  )

tbl_age_index <- tbl_pop |> 
  summarise(
    population = sum(valor, na.rm = TRUE),
    .by = c("year", "age_group")
  ) |> 
  mutate(
    share = population / sum(population, na.rm = TRUE),
    .by = c("year")
  ) |> 
  mutate(age_index = 1 / share - 1, age_index = age_index * 100) |> 
  filter(age_group == "young") |> 
  select(year, age_index)

#> Join both tables

tbl_age_index <- bind_rows(list(tbl_age_index, tbl_census))

dput(tbl_age_index)
