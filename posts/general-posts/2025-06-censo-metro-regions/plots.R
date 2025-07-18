library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(geobr)
library(ragg)
library(patchwork)

import::from(sidrar, get_sidra)
import::from(sf, st_drop_geometry)

clean_sidra <- function(df, name_value = "pop") {
  cols <- c("code_muni" = "municipio_codigo", "year" = "ano", x = "valor")
  names(cols)[3] <- name_value

  df %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
}

# Import data ---------------------------------------------------------------

pop2022 <- get_sidra(4709, variable = 93, geo = "City")

pop_series <- get_sidra(
  136,
  variable = 93,
  period = "1991-2010",
  geo = "City",
  classific = c("c86"),
  category = list(0)
)

# Shape files

# Metro Regions
metro_region <- read_metro_area(showProgress = FALSE)
dim_metro <- as_tibble(st_drop_geometry(metro_region))
dim_metro <- select(dim_metro, code_muni, name_metro)

# Cities
cities <- read_municipality(year = 2020, showProgress = FALSE)
dim_city <- as_tibble(st_drop_geometry(cities))

# Join dimension tables
dim_city <- left_join(dim_city, dim_metro, by = "code_muni")

#> Remove an exception: Murici is mapped into two different metro regions
dim_city <- dim_city |>
  filter(!(code_muni == 2705507 & name_metro == "RM da Zona da Mata"))

## Clean and format ---------------------------------------------------------

cpop_series <- clean_sidra(pop_series)
cpop2022 <- clean_sidra(pop2022)

census_pop <- rbind(cpop_series, cpop2022)
census_pop <- left_join(census_pop, dim_city, by = "code_muni")

## Classify cities ----------------------------------------------------------

breaks <- c(20000, 50000, 100000, 500000, 1000000)

bb <- format(breaks, big.mark = ".", scientific = FALSE)
labels <- paste(bb, bb[-1], sep = "-")
labels <- c(" < 20.000", labels[-length(labels)], "> 1.000.000")
labels <- str_remove_all(labels, " ")

labels <- c(
  "Menos de 20 mil",
  "20 a 50 mil",
  "50 a 100 mil",
  "100 a 500 mil",
  "500 mil a 1 milhão",
  "Mais de 1 milhão"
)

dflabel <- tibble(
  pop_group = 0:5,
  labels = c(
    "Menos de 20 mil",
    "20 a 50 mil",
    "50 a 100 mil",
    "100 a 500 mil",
    "500 mil a 1 milhão",
    "Mais de 1 milhão"
  )
)

cpop2022 <- cpop2022 |>
  mutate(pop_group = findInterval(pop, breaks))

## Compute growth metrics ----------------------------------------------------

tab_cities <- census_pop |>
  filter(year %in% c(2010, 2022)) |>
  mutate(
    chg_abs = pop - lag(pop),
    chg_rel = pop / lag(pop) - 1,
    .by = c("code_muni", "name_muni")
  ) |>
  left_join(select(cpop2022, code_muni, pop_group), by = "code_muni")

tab_growth_cities <- tab_cities |>
  filter(year == 2022) |>
  mutate(is_growth = if_else(chg_abs > 0, 1L, 0L)) |>
  summarise(
    total_growth = sum(chg_abs, na.rm = TRUE),
    total_growth_label = format(round(total_growth, -3), big.mark = "."),
    total_wgt = weighted.mean(chg_rel, pop, na.rm = TRUE),
    total = sum(is_growth, na.rm = TRUE),
    count = n(),
    share = total / count * 100,
    .by = "pop_group"
  ) |>
  arrange(pop_group) |>
  left_join(dflabel)

# Plots ------------------------------------------------------------------

## Plot elements -----------------------------------------------------------

plot_caption <- "Fonte: IBGE | REstateInsight"

theme_plot <- theme_minimal(base_family = "Avenir") +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_blank(),
    plot.title = element_text(family = "Georgia", size = 14),
    plot.subtitle = element_text(size = 12, color = "gray10"),
    plot.caption = element_text(hjust = 0)
  )

## Number of cities growth --------------------------------------------------

plot_cities <-
  ggplot(tab_growth_cities, aes(x = as.factor(pop_group), y = share)) +
  geom_col(fill = "#1d3557") +
  geom_text(
    aes(label = paste0(round(share, 1), "%")),
    color = "white",
    family = "Avenir",
    fontface = "bold",
    nudge_y = -5
  ) +
  scale_x_discrete(labels = str_wrap(paste(labels, "hab."), width = 12)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL, y = NULL)

plot_cities <- plot_cities &
  plot_annotation(
    title = "Cidades de Médio e Médio-Grande Porte foram as que mais cresceram",
    subtitle = "Percentual de cidades que tiveram crescimento populacional.",
    caption = plot_caption
  ) &
  theme_plot

## Average growth ----------------------------------------------------------

plot_growth <- ggplot(
  tab_growth_cities,
  aes(x = as.factor(pop_group), y = total_wgt * 100)
) +
  geom_col(fill = "#1d3557") +
  geom_text(
    aes(label = paste0(round(total_wgt * 100, 1), "%")),
    color = "white",
    family = "Avenir",
    fontface = "bold",
    nudge_y = c(rep(-1, 5), -0.55)
  ) +
  scale_x_discrete(labels = str_wrap(paste(labels, "hab."), width = 12)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL, y = NULL)

plot_growth <- plot_growth &
  plot_annotation(
    title = "Cidades de Médio e Médio-Grande Porte foram as que mais cresceram",
    subtitle = "Crescimento médio da população",
    caption = plot_caption
  ) &
  theme_plot

## Total growth -------------------------------------------------------------

plot_total_growth <-
  ggplot(
    tab_growth_cities,
    aes(x = as.factor(pop_group), y = total_growth / 1000)
  ) +
  geom_col(fill = "#1d3557") +
  geom_text(
    aes(label = total_growth_label),
    color = c("black", "white", "white", "white", "white", "black"),
    family = "Avenir",
    fontface = "bold",
    nudge_y = c(500, -550, -550, -550, -550, 500)
  ) +
  scale_x_discrete(labels = str_wrap(paste(labels, "hab."), width = 12)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL, y = NULL)

plot_total_growth <- plot_total_growth &
  plot_annotation(
    title = "Cidades de Médio e Médio-Grande Porte foram as que mais cresceram",
    subtitle = "Crescimento populacional total",
    caption = plot_caption
  ) &
  theme_plot

## Export plots ------------------------------------------------------------

plots <- list(
  cities = plot_cities,
  growth = plot_growth,
  total_growth = plot_total_growth
)

readr::write_rds(
  plots,
  here::here("posts/general-posts/2025-06-censo-metro-regions/plots.rds")
)

# Other plots -------------------------------------------------------------
# These are for the full report
pib <- get_sidra(
  5938,
  variable = 37,
  period = "2021",
  geo = "City"
)

tab_pib <- pib %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(
    code_muni = municipio_codigo,
    pib = valor
  ) %>%
  mutate(code_muni = as.numeric(code_muni))

tab_cities <- tab_cities %>%
  filter(year == 2022) %>%
  left_join(tab_pib) %>%
  mutate(pibpc = pib / pop * 1000)

sub_cities <- tab_cities |>
  filter(year == 2022, pop > 1e5)

avg_growth <- weighted.mean(sub_cities$chg_rel, sub_cities$pop)

sub_cities %>%
  slice_min(chg_rel, n = 10)

sel_cities <- c(
  "São Gonçalo",
  "Uruguaiana",
  "São Paulo",
  "Porto Alegre",
  "Nova Lima",
  "Belo Horizonte",
  "Brasília",
  "Chapecó",
  "Sinop",
  "Senador Canedo",
  "Fazenda Rio Grande",
  "Palhoça",
  "Camboriú",
  "Maricá",
  "São José De Ribamar",
  "Parauapebas",
  "Paulínia"
)

sub_cities <- sub_cities %>%
  mutate(
    label_city = if_else(name_muni %in% sel_cities, name_muni, "")
  )

ybreaks <- c(10000, 20000, 50000, 100000, 500000)
ylabels <- format(ybreaks, big.mark = ".", scientific = FALSE)
ylabels[length(ylabels)] <- "PIB per capita\nR$ 500.000 (Escala Log)"

df_axis <- tibble(x = -0.45, y = log(ybreaks) + 0.15, label = ylabels)
df_axis <- df_axis %>%
  mutate(y = ifelse(y == max(y), y + 0.15, y))

theme_scatterplot <- theme_minimal(base_family = "Avenir") +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray25"),
    axis.title.x = element_text(color = "gray10"),
    plot.title = element_text(family = "Georgia", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray20"),
    plot.caption = element_text(color = "gray30"),
    legend.position = "top",
    legend.text = element_text(size = 12, color = "gray25")
  )

ggplot(sub_cities, aes(x = chg_rel, y = log(pibpc))) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = avg_growth, linetype = 2, color = "gray10") +
  ggrepel::geom_label_repel(
    aes(label = label_city),
    family = "Avenir",
    size = 3,
    max.overlaps = Inf,
    min.segment.length = 0
  ) +
  geom_text(
    data = df_axis,
    aes(x, y, label = label),
    hjust = 0,
    family = "Avenir",
    color = "gray10"
  ) +
  geom_point(
    aes(fill = name_region, size = pop),
    shape = 21,
    alpha = 0.85
  ) +
  scale_x_continuous(
    labels = scales::label_percent(),
    breaks = seq(-0.1, 0.8, 0.1),
    limits = c(-0.5, NA)
  ) +
  scale_y_continuous(
    breaks = log(ybreaks),
    labels = ylabels,
    limits = c(NA, max(log(ybreaks)) + 1.1)
  ) +
  scale_size(range = c(1, 10)) +
  scale_fill_manual(
    name = "",
    values = c("#A44A3D", "#E09351", "#C0BE84", "#466C6F", "#224B5E")
  ) +
  guides(
    size = "none",
    fill = guide_legend(override.aes = list(size = 3))
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "Crescimento populacional (2022/2010, %)",
    y = "PIB per capita (2022)",
    caption = "Fonte: IBGE (Censo 2022, Contas Nacionais) | REstateInsight",
  ) +
  theme_scatterplot

plot_scatter_region <- function(code_region) {
  region <- code_region

  swap_region <- c(
    "1" = "Norte",
    "2" = "Nordeste",
    "3" = "Sudeste",
    "4" = "Sul",
    "5" = "Centro Oeste"
  )

  name_region <- swap_region[as.character(region)]

  sub_region <- sub_cities %>%
    mutate(
      highlight_region = factor(if_else(code_region == local(region), 1L, 0L))
    )

  avg_growth_region <- sub_region %>%
    filter(code_region == local(region)) %>%
    summarise(x = weighted.mean(chg_rel, pop)) %>%
    pull(x)

  cols_regions <- c(
    "Centro Oeste" = "#A44A3D",
    "Nordeste" = "#E09351",
    "Norte" = "#C0BE84",
    "Sudeste" = "#466C6F",
    "Sul" = "#224B5E"
  )

  cols_highlight <- rep("#bfbfbf", 5)
  names(cols_highlight) <- names(cols_regions)
  cols_highlight[name_region] <- cols_regions[name_region]

  sel_region <- sub_region %>%
    filter(code_region == region) %>%
    slice_max(chg_rel, n = 10) %>%
    pull(name_muni)

  sel_labels <- c(sel_region, sample(sel_cities, 7))

  sub_region <- sub_region %>%
    mutate(
      label_city = if_else(name_muni %in% sel_labels, name_muni, "")
    )

  plot_labs <- list(
    title = c(
      "Cidades de Médio e Médio-Grande Porte foram as que mais cresceram",
      "Cidades do NE tem maior variância. Capitais e cidades grandes sofrem mais do que a média",
      "Rio de Janeiro e cidades grandes sofrem enquanto interior de SP desponta",
      "Cidades do SC tem crescimento forte enquanto RS e fica para trás",
      "Todas cidades do CO cresceram acima da média do país"
    ),
    subtitle = NULL,
    x = "Crescimento populacional (2022/2010, %)",
    y = NULL,
    caption = "Fonte: IBGE (Censo 2022, Contas Nacionais) | REstateInsight"
  )

  ggplot(sub_region, aes(x = chg_rel, y = log(pibpc))) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = avg_growth, linetype = 2, color = "gray10") +
    # Diferenciar esta linha da outra
    geom_vline(xintercept = avg_growth_region, linetype = 2, color = "gray10") +
    # Adicionar label indicando qual linha é qual (média BR x média regiao)
    ggrepel::geom_label_repel(
      aes(label = label_city),
      family = "Avenir",
      size = 3,
      max.overlaps = Inf,
      min.segment.length = 0
    ) +
    geom_text(
      data = df_axis,
      aes(x, y, label = label),
      hjust = 0,
      family = "Avenir",
      color = "gray10"
    ) +
    geom_point(
      data = subset(sub_region, highlight_region == 0L),
      aes(fill = name_region, size = pop, alpha = highlight_region),
      shape = 21
    ) +
    geom_point(
      data = subset(sub_region, highlight_region == 1L),
      aes(fill = name_region, size = pop, alpha = highlight_region),
      shape = 21
    ) +
    scale_x_continuous(
      labels = scales::label_percent(),
      breaks = seq(-0.1, 0.8, 0.1),
      limits = c(-0.5, NA)
    ) +
    scale_y_continuous(
      breaks = log(ybreaks),
      labels = ylabels,
      limits = c(NA, max(log(ybreaks)) + 1.1)
    ) +
    scale_size(range = c(1, 10)) +
    scale_fill_manual(
      name = "",
      values = cols_highlight
    ) +
    scale_alpha_manual(values = c(0.25, 0.85)) +
    guides(
      size = "none",
      alpha = "none",
      fill = guide_legend(override.aes = list(size = 3))
    ) +
    labs(
      title = plot_labs$title[region],
      subtitle = plot_labs$subtitle,
      x = plot_labs$x,
      y = plot_labs$y,
      caption = plot_labs$caption,
    ) +
    theme_minimal(base_family = "Avenir") +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(family = "Georgia", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray10"),
      legend.position = "top",
      legend.text = element_text(size = 12),
      plot.margin = margin(10, 5, 10, 5)
    )
}

plot_scatter_region(5)
plot_scatter_region(2)
plot_scatter_region(1)
plot_scatter_region(3)
plot_scatter_region(4)

region <- 1

swap_region <- c(
  "1" = "Norte",
  "2" = "Nordeste",
  "3" = "Sudeste",
  "4" = "Sul",
  "5" = "Centro Oeste"
)

name_region <- swap_region[as.character(region)]

sub_region <- sub_cities %>%
  mutate(
    highlight_region = factor(if_else(code_region == local(region), 1L, 0L))
  )

avg_growth_region <- sub_region %>%
  filter(code_region == local(region)) %>%
  summarise(x = weighted.mean(chg_rel, pop)) %>%
  pull(x)

cols_regions <- c(
  "Centro Oeste" = "#A44A3D",
  "Nordeste" = "#E09351",
  "Norte" = "#C0BE84",
  "Sudeste" = "#466C6F",
  "Sul" = "#224B5E"
)

cols_highlight <- rep("#bfbfbf", 5)
names(cols_highlight) <- names(cols_regions)
cols_highlight[name_region] <- cols_regions[name_region]

sel_region <- sub_region %>%
  filter(code_region == region) %>%
  slice_max(chg_rel, n = 10) %>%
  pull(name_muni)

sel_labels <- c(sel_region, sample(sel_cities, 7))


sub_region <- sub_region %>%
  mutate(
    label_city = if_else(name_muni %in% sel_labels, name_muni, "")
  )

ggplot(sub_region, aes(x = chg_rel, y = log(pibpc))) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = avg_growth, linetype = 2, color = "gray10") +
  geom_vline(xintercept = avg_growth_region, linetype = 2, color = "gray10") +
  ggrepel::geom_label_repel(
    aes(label = label_city),
    family = "Avenir",
    size = 3,
    max.overlaps = Inf,
    min.segment.length = 0
  ) +
  geom_text(
    data = tibble(x = -0.45, y = log(ybreaks) + 0.15, label = ylabels),
    aes(x, y, label = label),
    hjust = 0,
    family = "Avenir"
  ) +
  geom_point(
    data = subset(sub_region, highlight_region == 0L),
    aes(fill = name_region, size = pop, alpha = highlight_region),
    shape = 21
  ) +
  geom_point(
    data = subset(sub_region, highlight_region == 1L),
    aes(fill = name_region, size = pop, alpha = highlight_region),
    shape = 21
  ) +
  scale_x_continuous(
    labels = scales::label_percent(),
    breaks = seq(-0.1, 0.8, 0.1),
    limits = c(-0.5, NA)
  ) +
  scale_y_continuous(
    breaks = log(ybreaks),
    labels = ylabels
  ) +
  scale_size(range = c(1, 10)) +
  scale_fill_manual(
    name = "",
    values = cols_highlight
  ) +
  scale_alpha_manual(values = c(0.25, 0.85)) +
  guides(
    size = "none",
    alpha = "none",
    fill = guide_legend(override.aes = list(size = 3))
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    caption = "Fonte: IBGE (Censo 2022, Contas Nacionais) | REstateInsight",
  ) +
  theme_minimal(base_family = "Avenir") +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(family = "Georgia", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray10"),
    legend.position = "top",
    legend.text = element_text(size = 12)
  )


as.character(MetBrewer::met.brewer("Hokusai1", n = 20))

sub_cities %>%
  slice_max(chg_rel, n = 15)

sub_cities %>%
  slice_max(pibpc, n = 15)
