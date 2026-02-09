library(dplyr)
library(sf)
import::from(janitor, clean_names)
import::from(tidyr, pivot_longer)
import::from(sidrar, get_sidra)
import::from(geobr, read_municipality)

code_sidra <- 6579

pop25 <- get_sidra(code_sidra, period = "2025", geo = "City")
pop11 <- get_sidra(code_sidra, period = "2011", geo = "City")

geo_muni10 <- read_municipality(
  year = 2010,
  showProgress = FALSE,
  simplified = FALSE
)
geo_muni24 <- read_municipality(
  year = 2024,
  showProgress = FALSE,
  simplified = FALSE
)

dim_city <- as_tibble(st_drop_geometry(geo_muni24))

tab_area_10 <- geo_muni10 |>
  st_transform(crs = 32722) |>
  mutate(
    area = st_area(geom),
    area = as.numeric(area) / 1e6
  ) |>
  select(code_muni, area)

tab_area_24 <- geo_muni24 |>
  st_transform(crs = 32722) |>
  mutate(
    area = st_area(geom),
    area = as.numeric(area) / 1e6
  ) |>
  select(code_muni, area)

tab_area <- bind_rows(
  list("2011" = tab_area_10, "2025" = tab_area_24),
  .id = "year"
)

tab_area <- tab_area |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(across(everything(), as.numeric))

pop <- bind_rows(pop25, pop11)

pop <- pop |>
  as_tibble() |>
  clean_names() |>
  select(
    code_muni = municipio_codigo,
    year = ano,
    pop = valor
  ) |>
  mutate(across(everything(), as.numeric))

tab <- left_join(pop, tab_area, by = c("code_muni", "year"))

tab <- tab |>
  arrange(year) |>
  mutate(pop_density = pop / area) |>
  mutate(
    across(
      pop:pop_density,
      ~ (.x / lag(.x, order_by = year) - 1) * 100,
      .names = "chg_pct_{.col}"
    ),
    .by = "code_muni"
  ) |>
  mutate(
    across(
      pop:pop_density,
      ~ .x - lag(.x, order_by = year),
      .names = "chg_{.col}"
    ),
    .by = "code_muni"
  )

qs::qsave(tab, "posts/general-posts/2025-11-pop-density/data.qs")
