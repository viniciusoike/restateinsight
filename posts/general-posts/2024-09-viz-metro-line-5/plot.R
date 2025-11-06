library(dplyr)
library(ggplot2)
library(ggtext)
library(sf)
library(showtext)
library(stringr)
library(trendseries)
# library(basemaps)

import::from(here, here)
import::from(forcats, fct_reorder)
import::from(readr, read_csv)
import::from(lubridate, `%m+%`, `%m-%`, month)

dat <- read_csv(here("static/data/metro_sp.csv"))
station <- read_csv(here("static/data/metro_sp_line_5_stations.csv"))

font_add("Helvetica", "Helvetica.ttc")
font_add("Helvetica Neue", "HelveticaNeue.ttc")
font_add("Fira Code", "FiraCode-Regular.ttf")
font_add_google("Fira Sans", "Fira Sans")
showtext_auto()

colors <- c("#9d4edd")

theme_metro <- theme_minimal(base_family = "Fira Sans", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linetype = 2),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff")
  )


# 1. Total Passenger Flow -------------------------------------------------

# 2018: Hospital Sao Paulo, Santa Cruz, Chacara Klabin
# 4 de abril de 2018	Inauguração da Estação Oscar Freire[87][88]
# 27 de outubro de 2018	Inauguração da Estação São Paulo–Morumbi
# 17 de dezembro de 2021	Inauguração da Estação Vila Sônia

## Auxiliar data -----------------------------------------------------------

df_labels <- tibble(
  date = c(
    as.Date("2018-08-01"),
    as.Date("2018-09-01"),
    as.Date("2019-04-01"),
    as.Date("2019-10-01"),
    as.Date("2024-06-01")
  ),
  date_label = c(
    "Ago-18",
    "Set-18",
    "Abr-19",
    "Out-19",
    "Jun-24"
  ),
  label = c(
    "ViaMobilidade\nassume operação\nda linha-5 Lilás",
    "Inauguração\nHospital São Paulo\nSanta Cruz e\nChácara Klabin",
    "Linha 5-Lilás\n é finalizada\napós entrega da\nestação Campo Belo",
    "16,1 milhão\nde passageiros\ntransportados",
    "Aumento\nda tarifa\n(4,40 -> 5,00)"
  ),
  xtext = c(
    as.Date("2018-08-01") %m+% months(-7),
    as.Date("2018-09-01"),
    as.Date("2019-04-01") %m+% months(5),
    as.Date("2019-10-01") %m+% months(-7),
    as.Date("2024-06-01")
  ),
  ytext = c(18500, 3500, 3500, 18500, 3500),
  ytext_label = c(18500, 250, 250, 18500, 750),
  yend_line = c(20000, 1750, 1750, 20000, 1750)
)

total_passengers <- dat |>
  filter(
    variable == "transport",
    metric == "total",
    metro_line_num == 5
  ) |>
  filter(!is.na(value)) |>
  summarise(
    value = sum(value),
    .by = names(dat)[-8]
  )

total_passengers <- trendseries::augment_trends(
  total_passengers,
  methods = "stl"
)

df_labels <- inner_join(df_labels, total_passengers, by = "date")

## Plot --------------------------------------------------------------------

total_passengers |>
  slice_max(value)

base_plot <- ggplot() +
  geom_rect(
    aes(
      xmin = as.Date("2020-03-16"),
      xmax = as.Date("2023-05-12"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "gray70",
    alpha = 0.4
  ) +
  geom_line(
    data = total_passengers,
    aes(x = date, y = value),
    color = colors,
    alpha = 0.5,
    lwd = 0.8
  ) +
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
  # Straight lines from points
  geom_segment(
    data = df_labels,
    aes(x = date, xend = date, y = yend_line, yend = value),
    color = "gray20"
  ) +
  # Horizontal lines (if needed)
  geom_segment(
    data = slice(df_labels, 3),
    aes(x = date, xend = date %m+% months(6), y = 1750, yend = 1750)
  ) +
  geom_segment(
    data = slice(df_labels, 4),
    aes(x = date, xend = date %m+% months(-6), y = 20000, yend = 20000)
  ) +
  geom_segment(
    data = slice(df_labels, 1),
    aes(x = date, xend = date %m+% months(-6), y = 20000, yend = 20000)
  ) +
  # Points to highlight
  geom_point(
    data = df_labels,
    aes(x = date, y = value),
    shape = 21,
    size = 2,
    color = "gray20",
    fill = colors
  ) +
  # Date labels
  geom_label(
    data = df_labels,
    aes(x = date, y = ytext, label = date_label),
    size = 3,
    family = "Fira Code"
  ) +
  # Text box labels
  geom_label(
    data = df_labels,
    aes(x = xtext, y = ytext_label, label = label),
    size = 2,
    family = "Fira Code",
    label.size = 0,
    hjust = 0.5
  ) +
  # Covid text label
  geom_text(
    aes(
      x = as.Date("2021-10-13"),
      y = 20000,
      label = "Pandemia Covid-19\n(mar/20-mai/23)"
    ),
    family = "Fira Sans",
    size = 3
  )

p_flow_total <- plot_labels +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", scale = 0.001),
    breaks = seq(5000, 20000, 5000),
    limits = c(-1000, NA)
  ) +
  labs(
    title = "Total de Passageiros (Linha-5, Lilás)",
    subtitle = "Numero total de passageiros transportados na linha-5 Lilás (incluindo baldeações) a cada mês.",
    x = NULL,
    y = "Milhões",
    caption = "Fonte: ViaMobilidade. @viniciusoike"
  ) +
  theme_metro

# 2. Flow Pandemic --------------------------------------------------------

## Auxiliar data -----------------------------------------------------------

# Aggregate data (line-5 has info from Metro and ViaMobilidade)
passenger <- dat |>
  filter(
    variable == "transport",
    metro_line_num == 5
  ) |>
  filter(!is.na(value)) |>
  # Aggregate by all variables except 'value'
  summarise(
    value = mean(value, na.rm = TRUE),
    .by = names(dat)[-grep("value", names(dat))]
  )

# Compute pre-pandemic base line (avg. values 2019)
pre_pandemic_baseline <- passenger |>
  mutate(month = month(date)) |>
  filter(year == 2019) |>
  summarise(base_index = mean(value, na.rm = TRUE), .by = c("metric", "month"))

# Join baseline with data and compute indexed values
df1 <- passenger |>
  mutate(month = month(date)) |>
  left_join(pre_pandemic_baseline, by = c("metric", "month")) |>
  mutate(
    index = value / base_index * 100,
    cat = case_when(
      metric == "mdu" ~ "Workday",
      metric %in% c("msa", "mdo") ~ "Weekend",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(cat)) |>
  summarise(
    index_2 = mean(index),
    .by = c("date", "cat")
  )

df1 <- trendseries::augment_trends(
  df1,
  value_col = "index_2",
  group_vars = "cat",
  methods = "stl",
  window = 17
)

## Plot --------------------------------------------------------------------

colors_days <- c("#807dba", "#3f007d")

subtitle <- str_glue(
  "Indexed average Line-5 Subway passenger flow for <b style='color:{colors_days[1]}'>weekdays</b> and <b style='color:{colors_days[2]}'>workdays</b>."
)

p_flow <- ggplot(df1, aes(date, trend_stl, color = cat)) +
  geom_hline(yintercept = 100) +
  geom_line(lwd = 0.8) +
  geom_text(
    data = df1 |> dplyr::group_by(cat) |> dplyr::slice_max(date, n = 1),
    aes(
      x = date + months(3),
      y = trend_stl,
      label = round(trend_stl, 1),
      color = cat
    )
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = colors_days) +
  guides(color = "none") +
  labs(
    title = "Weekend passenger flow has exceeded pre-pandemic values",
    subtitle = subtitle,
    y = "Index (100 = 2019 average)",
    x = NULL,
  ) +
  theme_metro +
  theme(
    axis.title = element_text(size = 10),
    plot.subtitle = element_textbox_simple(
      size = 10,
      padding = margin(5.5, 5.5, 5.5, 0),
      margin = margin(0, 0, 0, 0)
    )
  )


# 3. Stations Ranking -----------------------------------------------------

## Auxiliar Data -----------------------------------------------------------

format_date <- function(x) {
  mes <- lubridate::month(x, label = TRUE, abbr = TRUE)
  ano <- lubridate::year(x)
  return(paste(mes, ano, sep = "/"))
}

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
    name_station = fct_reorder(name_station, rolling_year),
    date_start = date %m+% months(-11),
    date_range = stringr::str_glue(
      "{format_date(date_start)} - {format_date(date)}"
    )
  ) |>
  arrange(name_station)

## Plot -------------------------------------------------------------------

subtitle <- str_glue(
  "Total passengers by station (L12M) — {unique(rank_station$date_range)}"
)

p_rank_station <- ggplot(
  rank_station,
  aes(x = name_station, y = rolling_year)
) +
  geom_col(fill = colors) +
  # Label: station name
  geom_text(
    aes(y = 10, label = str_wrap(str_remove(name_station, " -"), 21)),
    size = 3,
    family = "Fira Sans",
    color = "black",
    hjust = 0
  ) +
  # Label: number of passengers
  geom_text(
    aes(
      y = ifelse(rolling_year < 200, 231, rolling_year + 30),
      label = format(round(rolling_year), big.mark = ".")
    ),
    family = "Fira Sans",
    fontface = "bold",
    size = 3
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 1200)) +
  coord_flip() +
  labs(
    title = "Station Ranking",
    subtitle = subtitle,
    caption = "Source: ViaMobilidade | @viniciusoike"
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
  filter(line_number == 5)

metro_line <- metro_line |>
  filter(line_number == 5)

metro_line_future <- metro_line_future |>
  filter(line_number == 5)

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
  basemaps::basemap_gglayer(
    ext = ext,
    map_service = "carto",
    map_type = "light"
  ) +
  scale_fill_identity() +
  coord_sf()

metro_station <- metro_station |>
  mutate(
    ytext = if_else(
      station_name %in% c("Hospital São Paulo", "Largo Treze"),
      -400,
      300
    )
  )

p_map <- p_basemap +
  geom_sf(data = st_transform(metro_station, 3857), color = colors) +
  geom_sf(data = st_transform(metro_line, 3857), color = colors) +
  geom_sf(
    data = st_transform(metro_line_future, 3857),
    linetype = 3,
    color = colors
  ) +
  geom_sf_label(
    data = st_transform(metro_station, 3857),
    aes(label = str_wrap(station_name, 17)),
    nudge_y = metro_station$ytext,
    size = 2,
    family = "Helvetica"
  ) +
  theme_void()

# Export plots ------------------------------------------------------------

showtext_opts(dpi = 300)
showtext_auto()

ggsave(
  here("static/images/spo_metro/line_5_map.png"),
  p_map,
  device = ragg::agg_png
)

cowplot::save_plot(
  here("static/images/spo_metro/line_5_overview.png"),
  p_flow_total,
  base_height = 6,
  device = ragg::agg_png
)

cowplot::save_plot(
  here("static/images/spo_metro/line_5_pandemic.png"),
  p_flow,
  base_height = 5,
  device = ragg::agg_png
)

cowplot::save_plot(
  here("static/images/spo_metro/line_5_station_ranking.png"),
  p_rank_station,
  base_height = 6,
  device = ragg::agg_png
)
