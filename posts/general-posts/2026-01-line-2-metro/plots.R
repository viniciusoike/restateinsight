library(metrosp)
library(ggplot2)
library(dplyr)
library(trendseries)

stations <- station_daily |>
  filter(line_number == 2)

psg <- passengers_transported |>
  filter(line_number == 2) |>
  mutate(
    value = if_else(date >= as.Date("2020-01-01"), value * 1000, value)
  )

psg_total <- psg |>
  filter(metric_abb == "total")

ggplot(psg_total, aes(x = date, y = value)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(
    labels = scales::label_number(scale = 0.001, suffix = "M")
  ) +
  labs(y = "Passageiros transportados", x = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Theme -------------------------------------------------------------------
main_color <- "#006D2C"
colors_days <- c("#41AB5D", "#00441B")
colors <- c("#00441B", "#006D2C")

text_theme <- "Lato"

theme_metro <- theme_minimal(base_family = text_theme, base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "gray95",
      linetype = 1,
      linewidth = 0.45
    ),
    plot.title = element_text(size = 12, family = "Lora", face = "bold"),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "#ffffff", color = "#ffffff")
  )

fmt_br <- function(x) {
  format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Plot: Total de Passageiros ----------------------------------------------

df_labels <- tibble(
  date = c(
    as.Date("2018-09-01"),
    as.Date("2019-10-01"),
    as.Date("2020-01-01"),
    as.Date("2024-01-01"),
    as.Date("2025-01-01")
  ),
  date_label = c(
    "Set-18",
    "Out-19",
    "Jan-20",
    "Jan-24",
    "Jan-25"
  ),
  label = c(
    "Integração\nChácara Klabin\ncom Linha 5-Lilás",
    "21,6 milhões\nde passageiros\ntransportados",
    "Último aumento\ntarifário\nantes da pandemia\n(4,30 -> 4,40)",
    "Aumento\nda tarifa\n(4,40 -> 5,00)",
    "Aumento\nda tarifa\n(5,00 -> 5,20)"
  ),
  xtext = c(
    as.Date("2018-09-01"),
    as.Date("2019-10-01"),
    as.Date("2020-01-01"),
    as.Date("2024-01-01"),
    as.Date("2025-01-06")
  ),
  ytext_label = c(5000, 8000, 25000, 7000, 5000),
  ytext_date_label = case_when(
    ytext_label < 20000 ~ ytext_label + 2000,
    date_label == "Jan-20" ~ ytext_label - 2000,
    TRUE ~ ytext_label - 2000
  )
)

tab_mes_trend <- psg_total |>
  augment_trends(
    methods = c("stl", "kalman", "ucm"),
    params = list(s.window = 21, robust = TRUE, t.window = 13)
  )

total_passengers <- tab_mes_trend |>
  mutate(
    value = value,
    trend_stl = trend_stl
  )

total_passengers |>
  slice_max(value, n = 1)

df_labels <- left_join(df_labels, total_passengers, by = "date")

base_plot <- ggplot() +
  geom_rect(
    aes(
      xmin = as.Date("2020-02-15"),
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
    color = colors[1],
    alpha = 0.5,
    lwd = 0.5
  ) +
  geom_point(
    data = total_passengers,
    aes(x = date, y = value),
    color = "gray20",
    fill = colors[1],
    alpha = 0.5,
    size = 2,
    shape = 21
  ) +
  geom_line(
    data = total_passengers,
    aes(x = date, y = trend_stl),
    color = colors[1],
    lwd = 0.8
  )

plot_labels <- base_plot +
  # Lies connecting dots and text labels
  geom_segment(
    data = df_labels,
    aes(x = date, xend = date, y = ytext_label, yend = value),
    color = "gray20"
  ) +
  # Highlight individual points
  geom_point(
    data = df_labels,
    aes(x = date, y = value),
    shape = 21,
    size = 2,
    color = "gray20",
    fill = colors[1]
  ) +
  # Date labels above text labels
  geom_label(
    data = df_labels,
    aes(x = date, y = ytext_date_label, label = date_label),
    size = 3,
    family = "Lato"
  ) +
  # Text labels
  geom_label(
    data = df_labels,
    aes(x = xtext, y = ytext_label, label = label),
    size = 2,
    family = "Fira Code",
    border.color = "gray40"
  ) +
  geom_text(
    aes(
      x = as.Date("2021-10-13"),
      y = 18500,
      label = "Pandemia Covid-19\n(mar/20-mai/23)"
    ),
    family = "Lato",
    size = 3
  )

p_flow_total <- plot_labels +
  geom_hline(yintercept = 0, lwd = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", scale = 0.001),
    breaks = seq(0, 25000, 5000),
    limits = c(0, 26000),
    expand = expansion(c(0, 0.05))
  ) +
  labs(
    title = "Total de Passageiros (Linha-2, Verde)",
    subtitle = "Numero total de passageiros transportados na linha-2 Verde (incluindo baldeações) a cada mês.",
    x = NULL,
    y = "Milhões",
    caption = "Fonte: METRÔ. @viniciusoike"
  ) +
  theme_metro +
  theme(
    axis.ticks.x = element_line(linewidth = 0.5)
  )

library(sf)

metro_station <- metro_stations_geo |>
  filter(line_number == 2)

stations_order <- c(
  "Vila Madalena",
  "Santuário N.S. de Fátima-Sumaré",
  "Clínicas",
  "Consolação",
  "Trianon-Masp",
  "Brigadeiro",
  "Paraíso",
  "Ana Rosa",
  "Chácara Klabin",
  "Santos-Imigrantes",
  "Alto do Ipiranga",
  "Sacomã",
  "Tamanduateí",
  "Vila Prudente"
)

station_trends <- station_averages |>
  filter(line_number == 2) |>
  mutate(
    station_names = factor(station_name, levels = stations_order)
  ) |>
  augment_trends(
    value_col = "avg_passenger",
    group_vars = "station_name",
    methods = c("stl", "kalman", "ma"),
    params = list(s.window = 21, robust = TRUE, t.window = 13)
  )

ggplot(
  subset(station_trends, station_name %in% stations_order[1:8]),
  aes(date, trend_stl)
) +
  geom_line() +
  facet_wrap(vars(station_names), ncol = 2)
