library(tidyverse)
library(readxl)
library(here)
library(sidrar)
library(sf)
info_sidra(617)

geo_state <- geobr::read_state()
dim_state <- as_tibble(st_drop_geometry(geo_state))

tab_state <- get_sidra(617, variable = 289, geo = "State")

censo <- read_excel(
  here("static", "data", "censo_migracao.xlsx"),
  skip = 5,
  col_names = FALSE,
  na = "-"
)

col_names <- c(
  "code_muni",
  "municipio",
  "uf_de_nascimento",
  "2000",
  "2010",
  "2022"
)

names(censo) <- col_names

censo <- censo |>
  fill(code_muni) |>
  fill(municipio) |>
  mutate(
    abbrev_state = str_extract(municipio, "(?<=\\()[A-Z]{2}"),
    name_muni = str_remove(municipio, " \\([A-Z]{2}\\)$")
  ) |>
  select(
    code_muni,
    name_muni,
    abbrev_state,
    uf_de_nascimento,
    `2000`,
    `2010`,
    `2022`
  )

censo <- censo |>
  pivot_longer(
    cols = starts_with("20"),
    names_to = "year",
    values_to = "pop"
  ) |>
  mutate(across(c(code_muni, year, pop), as.numeric))

pop_rs <- censo |>
  filter(abbrev_state == "RS") |>
  summarise(pop_rs = sum(pop, na.rm = TRUE), .by = "year")

tab_emigration <- censo |>
  filter(
    uf_de_nascimento == "Rio Grande do Sul",
    abbrev_state != "RS"
  ) |>
  summarise(pop_emigrate = sum(pop, na.rm = TRUE), .by = "year") |>
  left_join(pop_rs, by = "year") |>
  mutate(ratio = pop_emigrate / pop_rs * 100)

ggplot(tab_emigration, aes(x = as.factor(year), y = pop_emigrate)) +
  geom_col()

# gauchos fora do RS no Censo de 2022
gauchos <- censo |>
  filter(
    uf_de_nascimento == "Rio Grande do Sul",
    abbrev_state != "RS",
    year == 2022
  )

pop_muni <- censo |>
  filter(year == 2022) |>
  summarise(pop_muni = sum(pop, na.rm = TRUE), .by = "code_muni")

gauchos <- gauchos |>
  left_join(pop_muni, by = "code_muni") |>
  mutate(share_pop = pop / pop_muni * 100)

library(ekioplot)

dat <- gauchos |>
  slice_max(pop, n = 15) |>
  mutate(
    name_muni = factor(name_muni),
    name_muni = fct_reorder(name_muni, pop),
    col_label = str_glue("{name_muni} ({abbrev_state})")
  )

library(ragg)

plot_rs <- ggplot(dat, aes(x = name_muni, y = pop)) +
  geom_col(fill = ekio_palette()[9], width = 0.8) +
  geom_hline(yintercept = 0, lwd = 0.8) +
  geom_text(
    aes(label = scales::number(pop, accuracy = 1, big.mark = ".")),
    family = "Lato",
    size = 3,
    hjust = 0,
    nudge_y = 1000
  ) +
  geom_text(
    aes(y = 1000, label = col_label),
    family = "Lato",
    size = 3,
    color = "white",
    hjust = 0
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = "."),
    expand = expansion(c(0, 0.1))
  ) +
  labs(
    title = "Catarinense com Sotaque Gaudério",
    subtitle = "Cidades com maior número de gaúchos emigrados* (Censo, 2022)",
    x = NULL,
    y = NULL,
    caption = "Fonte: IBGE (Censo 2022) | EKIO\n(*) Pessoas que moram na cidade, mas que nasceram no Rio Grande do Sul"
  ) +
  coord_flip() +
  theme_ekio(base_family = "Lato") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(color = "#000000")
  )

ggsave(
  "/Users/viniciusreginatto/GitHub/ekioplot/prototypes/refs/censo_migration.png",
  plot = plot_rs,
  width = 8,
  height = 6
)

tab_state <- tab_state |>
  janitor::clean_names() |>
  as_tibble() |>
  select(
    code_state = unidade_da_federacao_codigo,
    name_state = unidade_da_federacao,
    uf_de_nascimento = unidade_da_federacao_de_nascimento,
    age_group_code = grupo_de_idade_codigo,
    age_group = grupo_de_idade,
    pop = valor
  )
