library(dplyr)
library(ggplot2)
library(ggtext)
library(sf)
library(showtext)
library(basemaps)

import::from(here, here)
import::from(tidyr, nest, unnest)
import::from(forcats, fct_reorder)
import::from(readr, read_csv)
import::from(lubridate, `%m+%`, month)
import::from(stringr, str_wrap, str_remove)

dat <- read_csv(here("static/data/metro_sp_line_5.csv"))
station <- read_csv(here("static/data/metro_sp_line_5_stations.csv"))

font_add("Helvetica", "Helvetica.ttc")
font_add("Helvetica Neue", "HelveticaNeue.ttc")
font_add("Fira Code", "FiraCode-Regular.ttf")
font_add_google("Fira Sans", "Fira Sans")
showtext_auto()

colors <- c("#e6bb3e")

theme_metro <- theme_minimal(base_family = "Fira Sans", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linetype = 2),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff")
  )


# 1. Total Passenger Flow -------------------------------------------------

# 23 de janeiro de 2018	Inauguração da Estação Higienópolis–Mackenzie[85][86]
# 4 de abril de 2018	Inauguração da Estação Oscar Freire[87][88]
# 27 de outubro de 2018	Inauguração da Estação São Paulo–Morumbi
# 17 de dezembro de 2021	Inauguração da Estação Vila Sônia

## Auxiliar data -----------------------------------------------------------


df_labels <- tibble(
  date = c(
    as.Date("2018-01-01"),
    as.Date("2018-04-01"),
    as.Date("2018-10-01"),
    as.Date("2021-12-01"),
    as.Date("2024-06-01")
  ),
  date_label = c(
    "Jan-18",
    "Abr-18",
    "Out-18",
    "Dez-21",
    "Jun-24"
  ),
  label = c(
    "Inauguração\nHigienópolis\nMackenzie",
    "Inauguração\nOscar Freire\n",
    "Inauguração\nSão Paulo\nMorumbi",
    "Inauguração\nVila Sônia\n",
    "Aumento\nda tarifa\n(4,40 -> 5,00)"
  ),
  xtext = c(
    as.Date("2018-01-01"),
    as.Date("2018-04-01") %m+% months(4),
    as.Date("2018-10-01") %m+% months(4),
    as.Date("2021-12-01"),
    as.Date("2024-06-01")
  ),
  ytext = c(2000, 3000, 2000, 2000, 2000)
)

total_passengers <- dat |> 
  filter(variable == "passenger_transported", metric == "Total") |> 
  trendseries::add_trend()

df_labels <- inner_join(df_labels, total_passengers, by = "date")

## Plot --------------------------------------------------------------------

base_plot <- ggplot() +
  geom_rect(
    aes(xmin = as.Date("2020-03-16"),
        xmax = as.Date("2023-05-12"),
        ymin = -Inf,
        ymax = Inf),
    fill = "gray70",
    alpha = 0.4) +
  geom_line(
    data = total_passengers,
    aes(x = date, y = value),
    color = colors,
    alpha = 0.5,
    lwd = 0.8) +
  geom_point(
    data = total_passengers,
    aes(x = date, y = value),
    color = colors,
    alpha = 0.5,
    size = 2
  ) +
  geom_line(
    data = total_passengers,
    aes(x = date, y = trend_stl),
    color = colors,
    lwd = 1
  )

plot_labels <- base_plot +
  geom_segment(
    data = df_labels,
    aes(x = date, xend = date, y = 1000, yend = value),
    color = "gray20"
  ) +
  geom_segment(
    data = slice(df_labels, 2:3),
    aes(x = date, xend = date %m+% months(4), y = 1000, yend = 1000)
  ) +
  geom_point(
    data = df_labels,
    aes(x = date, y = value),
    shape = 21,
    size = 2,
    color = "gray20",
    fill = colors
  ) +
  geom_label(
    data = df_labels,
    aes(x = date, y = ytext, label = date_label),
    size = 3,
    family = "Fira Code"
  ) +
  geom_text(
    data = df_labels,
    aes(x = xtext, y = 100, label = label),
    size = 2,
    family = "Fira Code",
    hjust = 0.5
  ) +
  geom_text(
    aes(x = as.Date("2021-10-13"),
        y = 21000,
        label = "Pandemia Covid-19\n(mar/20-mai/23)"),
    family = "Fira Sans",
    size = 3)

p_flow_total <- plot_labels +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", scale = 0.001),
    breaks = seq(5000, 20000, 5000)) +
  labs(
    title = "Total de Passageiros (Linha-4, Amarela)",
    subtitle = "Numero total de passageiros transportados na linha-4 Amarela (incluindo baldeações) a cada mês.",
    x = NULL,
    y = "Milhões",
    caption = "Fonte: ViaQuatro. @viniciusoike"
  ) +
  theme_metro

# 2. Flow Pandemic --------------------------------------------------------

## Auxiliar data -----------------------------------------------------------

passenger <- dat |> 
  filter(variable == "passenger_transported")

# Compute pre-pandemic base line (avg. values 2018-2019)
pre_pandemic_baseline <- passenger |> 
  mutate(month = month(date)) |> 
  filter(year < 2020) |> 
  summarise(base_index = mean(value), .by = c("metric", "month"))

# Join baseline with data and compute indexed values
df1 <- passenger |> 
  mutate(month = month(date)) |> 
  left_join(pre_pandemic_baseline, by = c("metric", "month")) |> 
  mutate(
    index = value / base_index * 100,
    cat = case_when(
      metric == "Média dos Dias Úteis" ~ "Workday",
      metric %in% c("Média dos Sábados", "Média dos Domingos") ~ "Weekend",
      TRUE ~ NA_character_
    )) |> 
  filter(!is.na(cat)) |> 
  summarise(
    index_2 = mean(index),
    .by = c("date", "cat")
  )

# Smooth both series using STL
df1 <- df1 |> 
  group_by(cat) |> 
  nest() |> 
  mutate(trend = lapply(data, \(x) trendseries::add_trend(x, value_colname = "index_2", trend = "stl"))) |> 
  unnest(trend) |> 
  ungroup()


## Plot --------------------------------------------------------------------

p_flow <- ggplot(df1, aes(date, trend_stl, color = cat)) +
  geom_hline(yintercept = 100) +
  geom_line(lwd = 1) +
  geom_text(
    data = df1 |> group_by(cat) |> slice_max(date, n = 1),
    aes(x = date + months(3), y = trend_stl, label = round(trend_stl, 1), color = cat)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("#2a9d8f", "#e76f51")) +
  guides(color = "none") +
  labs(
    title = "Workday passenger flow is still down 10% from pre-pandemic values",
    subtitle = "Indexed average Line-4 Subway passenger flow for <b><span style='color:#2a9d8f'>weekdays</span></b> and <b><span style='color:#e76f51'>workdays</span></b>.",
    y = "Index (100 = pre-pandemic average)",
    x = NULL,
  ) +
  theme_metro +
  theme(
    axis.title = element_text(size = 10),
    plot.subtitle = element_textbox_simple(
      size = 10,
      padding = margin(5.5, 5.5, 5.5, 0),
      margin = margin(0, 0, 0, 0))
  )


# 3. Stations Ranking -----------------------------------------------------

## Auxiliar Data -----------------------------------------------------------

rank_station <- station |> 
  mutate(
    # Compute moving sum 12-months by station
    rolling_year = RcppRoll::roll_sumr(value, n = 12),
    .by = "name_station"
  ) |> 
  # Get most recent value (jun-24)
  filter(date == max(date)) |> 
  mutate(
    name_station = factor(name_station),
    name_station = fct_reorder(name_station, rolling_year)
  ) |> 
  arrange(name_station)

## Plot -------------------------------------------------------------------

p_rank_station <- 
  ggplot(rank_station, aes(x = name_station, y = rolling_year)) +
  geom_col(fill = colors) +
  geom_text(
    aes(y = 25, label = str_wrap(str_remove(name_station, " -"), 11)),
    size = 3,
    family = "Fira Sans",
    color = "black",
    hjust = 0
  ) +
  geom_text(
    aes(y = rolling_year + 50,
        label = format(round(rolling_year), big.mark = ".")),
    family = "Fira Sans",
    size = 3
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 1800)) +
  coord_flip() +
  labs(
    title = "Station Ranking",
    subtitle = "Total passengers by station (L12M)"
  ) +
  theme_metro +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

# 4. Station Map ----------------------------------------------------------

# Auxiliar data -----------------------------------------------------------

# Import shapefiles
metro_station <- st_read(here("static/data/spo_metro_stations.gpkg"))
metro_line <- st_read(here("static/data/spo_metro_line.gpkg"))
metro_line_future <- st_read(here("static/data/spo_metro_line_future.gpkg"))

# Filter data for line-4
metro_station <- metro_station |> 
  filter(line_number == 4)

metro_line <- metro_line |> 
  filter(line_number == 4)

metro_line_future <- metro_line_future |> 
  filter(line_number == 4)

# Create a bounding box for the basemap
ext <- metro_station |> 
  # Convert to UTM-Mercator (m)
  st_transform(crs = 32722) |> 
  # Create a 1km buffer
  st_buffer(dist = 1000) |>
  # Convert to Web Mercator
  st_transform(crs = 3857) |>
  # Create the bounding box
  st_bbox() |> 
  st_as_sfc()

p_basemap <- ggplot() +
  # Carto basemap
  basemap_gglayer(ext = ext, map_service = "carto", map_type = "light") +
  scale_fill_identity() + 
  coord_sf()

p_map <- p_basemap +
  geom_sf(data = st_transform(metro_station, 3857), color = colors) +
  geom_sf(data = st_transform(metro_line, 3857), color = colors) +
  geom_sf(data = st_transform(metro_line_future, 3857), linetype = 3, color = colors) +
  geom_sf_label(
    data = st_transform(metro_station, 3857),
    aes(label = station_name),
    size = 2,
    family = "Helvetica",
    nudge_y = c(rep(250, 3), -300, rep(250, 9))) +
  theme_void()

# Export plots ------------------------------------------------------------

showtext_opts(dpi = 300)
showtext_auto()

ggsave(here("static/images/spo_metro/line_4_map.png"), p_map)

cowplot::save_plot(
  here("static/images/spo_metro/line_4_overview.png"),
  p_flow_total,
  base_height = 6)

cowplot::save_plot(
  here("static/images/spo_metro/line_4_pandemic.png"),
  p_flow,
  base_height = 5)

cowplot::save_plot(
  here("static/images/spo_metro/line_4_station_ranking.png"),
  p_rank_station,
  base_height = 6
)

# library(tmap)
# library(tmaptools)
# tmap_mode("view")
# 
# tm_shape(metro_station) +
#   tm_dots() +
#   tm_shape(metro_line) +
#   tm_lines() +
#   tm_shape(metro_line_future) +
#   tm_lines(lty = 3) +
#   tm_basemap(server = "CartoDB.Positron")