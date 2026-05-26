library(ggplot2)
library(dplyr)
library(ggiraph)
import::from(janitor, clean_names)
import::from(tidyr, pivot_longer)
import::from(sidrar, get_sidra)
import::from(geobr, read_municipality)
import::from(glue, glue)

code_sidra <- 6579

pop25 <- get_sidra(code_sidra, period = "2025", geo = "City")
pop11 <- get_sidra(code_sidra, period = "2011", geo = "City")

library(sf)

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

big_cities <- tab |>
  filter(year == 2011, pop > 1e5) |>
  pull(code_muni) |>
  unique()

tab <- tab |>
  left_join(dim_city, by = join_by(code_muni)) |>
  relocate(name_muni:name_region, .before = year)

tab_cities <- subset(tab, code_muni %in% big_cities)


library(ggplot2)
library(ggiraph)

tab |>
  slice_max(chg_pct_pop_density, n = 10)

xlabels <- c(1, 2, 5, 10, 50) * 1e5
xbreaks <- log(xlabels)

subdat <- filter(tab_cities, year == 2025, chg_pct_pop_density < 200)

subdat <- subdat %>%
  mutate(
    tooltip_text = glue(
      "<div style='font-family: Avenir, sans-serif; padding: 8px; min-width: 200px;'>
        <strong style='font-size: 14px; color: #2c3e50;'>{name_muni}</strong>
        <hr style='margin: 5px 0; border: none; border-top: 1px solid #ddd;'>
        <table style='width: 100%; font-size: 12px;'>
          <tr>
            <td style='color: #7f8c8d;'>População (2025):</td>
            <td style='text-align: right; font-weight: bold;'>{format(round(pop), big.mark = '.')}</td>
          </tr>
          <tr>
            <td style='color: #7f8c8d;'>Densidade Pop.:</td>
            <td style='text-align: right; font-weight: bold;'>{format(round(pop_density, 1), big.mark = '.', decimal.mark = ',')} hab/km²</td>
          </tr>
          <tr>
            <td style='color: #7f8c8d;'>Cresc. Densidade:</td>
            <td style='text-align: right; font-weight: bold; color: {ifelse(chg_pct_pop_density >= 0, \"#27ae60\", \"#e74c3c\")};'>{ifelse(chg_pct_pop_density >= 0, \"+\", \"\")}{format(round(chg_pct_pop_density, 1), decimal.mark = ',')}%</td>
          </tr>
          <tr>
            <td style='color: #7f8c8d;'>Cresc. População:</td>
            <td style='text-align: right; font-weight: bold; color: {ifelse(chg_pct_pop >= 0, \"#27ae60\", \"#e74c3c\")};'>{ifelse(chg_pct_pop >= 0, \"+\", \"\")}{format(round(chg_pct_pop, 1), decimal.mark = ',')}%</td>
          </tr>
        </table>
      </div>"
    )
  )

p_scatter <-
  ggplot(
    subdat,
    aes(x = log(pop), y = chg_pct_pop_density, fill = name_region)
  ) +
  geom_point_interactive(
    aes(tooltip = tooltip_text),
    size = 2,
    alpha = 0.7,
    shape = 21,
    color = "white"
  ) +
  geom_hline(yintercept = 0, lwd = 0.8) +
  scale_x_continuous(
    breaks = xbreaks,
    labels = format(round(xlabels / 1000), big.mark = ".", scientific = FALSE)
  ) +
  facet_wrap(vars(name_region)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hokusai1", 5)) +
  labs(
    title = "Cidades com maior crescimento populacional",
    subtitle = stringr::str_wrap(
      "Crescimento da densidade populacional (2010/25) nas maiores cidades brasileiras (mais de 100 mil habitantes em 2010).",
      71
    ),
    caption = "Fonte: IBGE (Censo, EstimaPOP). @viniciusoike",
    x = "População em 2025 (mil habitantes, escala log)",
    y = "Crescimento Dens. Pop. (2010/25)"
  ) +
  theme_minimal(base_family = "Avenir") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# Create rich HTML tooltip

girafe(
  ggobj = p_scatter,
  width_svg = 8,
  height_svg = 6,
  options = list(
    opts_hover(
      css = "fill:orange;stroke:black;stroke-width:2px;cursor:pointer;"
    ),
    opts_tooltip(
      css = "background-color: white; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); border: 1px solid #e0e0e0;",
      use_fill = FALSE
    )
  )
)
