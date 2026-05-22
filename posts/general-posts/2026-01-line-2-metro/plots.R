library(metrosp)
library(ggplot2)
library(dplyr)
library(sf)
library(trendseries)
library(leaflet)
import::from(here, here)

stations <- station_daily |>
  filter(line_number == 2)

psg <- passengers_transported |>
  filter(line_number == 2) |>
  mutate(
    value = if_else(date >= as.Date("2020-01-01"), value * 1000, value)
  )

psg <- passengers_transported |>
  filter(line_number == 2)

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
  ytext_label = c(5000, 8000, 26000, 7000, 5000),
  ytext_date_label = case_when(
    ytext_label < 20000 ~ ytext_label + 2000,
    date_label == "Jan-20" ~ ytext_label - 2500,
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
    limits = c(0, 27000),
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


stations_order <- c(
  "Vila Madalena",
  "Sumaré",
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

station_comp <- station_averages |>
  filter(line_number == 2, date == "2019-08-01" | date == "2025-08-01") |>
  mutate(
    period = if_else(date == as.Date("2019-08-01"), "2019", "2025"),
    period = factor(period, level = c("2025", "2019"))
  )

station_comp_order <- station_comp |>
  filter(period == "2019") |>
  arrange(avg_passenger) |>
  pull(station_name)

station_comp <- station_comp |>
  mutate(
    station_name = factor(station_name, levels = station_comp_order)
  )

station_comp_labels <- station_comp |>
  summarise(
    xtext = max(avg_passenger),
    abs_chg = last(avg_passenger) - first(avg_passenger),
    chg = last(avg_passenger) / first(avg_passenger) - 1,
    .by = "station_name"
  ) |>
  mutate(
    num_label = scales::number(chg, accuracy = 0.1, suffix = "%", scale = 100)
  )
library(ggtext)

subtitle <- "Comparação entre fluxo médio de passageiros transportados em dias úteis <span style='color:#41AB5D'><b>antes da pandemia (agosto de 2019)</b></span> e <span style='color:#00441B'><b>após a pandemia (agosto de 2025)</b></span>. Consolação (-40k), Ana Rosa (-22k) e Paraíso (-13k) concentram as maiores quedas no sistema. A consolidação das linhas 5-Lilás e 15-Prata impulsionaram a recuperação e aumento na atividade das estações Chácara Klabin (+11k) e Vila Prudente (+15k). A estação Tamanduateí (+19k) também teve forte recuperação."

plot_station_comp <- ggplot(
  station_comp,
  aes(avg_passenger, station_name, fill = period)
) +
  geom_col(width = 0.8, position = position_dodge(0.8)) +
  geom_label(
    data = station_comp_labels,
    aes(xtext, station_name, label = num_label),
    family = "Lato",
    size = 3,
    nudge_x = 10000,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = rev(colors_days),
    labels = c("Agosto de 2019", "Agosto de 2025")
  ) +
  scale_x_continuous(
    breaks = seq(0, 150000, 25000),
    labels = scales::label_number(scale = 1e-3, suffix = "k"),
    expand = expansion(c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Maiores quedas e ganhos foram nas estações integradoras",
    subtitle = subtitle,
  ) +
  theme_metro +
  theme(
    plot.margin = margin(15, 20, 10, 10),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      size = 9,
      color = "gray30",
      lineheight = 1.1,
      width = unit(1, "npc"),
      margin = margin(b = 10)
    ),
    panel.grid.major.y = element_blank(),
    axis.line.y = element_line(color = "gray20", size = 0.5),
    axis.ticks.y = element_line(color = "gray20", size = 0.25),
    legend.position = "bottom",
    legend.justification.bottom = "left",
    legend.key.height = unit(0.25, "cm")
  )


station_trends <- station_averages |>
  filter(line_number == 2) |>
  mutate(
    station_names = factor(station_name, levels = stations_order)
  ) |>
  augment_trends(
    value_col = "avg_passenger",
    group_cols = "station_name",
    methods = c("stl", "kalman", "ma"),
    params = list(s.window = 21, robust = TRUE, t.window = 13)
  )

stations_green <- station_averages |>
  filter(line_number == 2) |>
  mutate(
    station_name = factor(station_name, levels = stations_order)
  )

base_index <- stations_green |>
  filter(between(date, as.Date("2019-01-01"), as.Date("2019-12-31"))) |>
  summarise(
    base = mean(avg_passenger),
    .by = "station_name"
  )

tab_dias_trend <- stations_green |>
  left_join(base_index, by = "station_name") |>
  mutate(
    index = avg_passenger / base * 100
  ) |>
  augment_trends(
    value_col = "index",
    group_cols = "station_name",
    methods = c("stl", "ma"),
    params = list(s.window = 21, robust = TRUE, t.window = 13)
  )

tab_dias_comp <- tab_dias_trend |>
  filter(date >= as.Date("2019-01-01"))

plot_stations <- ggplot(
  tab_dias_comp,
  aes(date, trend_stl)
) +
  geom_rect(
    data = tibble(
      xmin = as.Date("2020-02-15"),
      xmax = as.Date("2023-05-12"),
      ymin = -Inf,
      ymax = Inf
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray70",
    alpha = 0.4,
    inherit.aes = FALSE
  ) +
  geom_line(lwd = 0.8, color = "#000") +
  geom_hline(yintercept = 100, lwd = 0.8) +
  geom_point(
    data = tab_dias_comp,
    aes(date, index),
    fill = main_color,
    size = 0.8,
    shape = 21,
    alpha = 0.3
  ) +
  facet_wrap(vars(station_name)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(20, 140, 20)) +
  labs(
    title = "Demanda no eixo da Paulista está 20% abaixo do nível pré-pandemia",
    subtitle = "Índice de Embarques nas Estações em Dias Úteis (média de 2019 = 100)",
    x = NULL,
    y = "Índice (100 = média de 2019)"
  ) +
  theme_metro +
  theme(
    strip.background = element_rect(fill = colors[2]),
    strip.text = element_text(
      color = "white",
      size = 9,
      family = "Lato"
    )
  )

base_index <- passengers_transported |>
  filter(
    metric_abb %in% c("mdu", "total"),
    line_number %in% 1:3,
    between(date, as.Date("2019-01-01"), as.Date("2019-12-31"))
  ) |>
  summarise(
    base = mean(value),
    .by = c("line_number", "metric_abb")
  )

psg_trend_lines <- passengers_transported |>
  filter(
    metric_abb %in% c("mdu", "total"),
    line_number %in% 1:3
  ) |>
  left_join(base_index, by = c("line_number", "metric_abb")) |>
  mutate(index = value / base * 100) |>
  augment_trends(
    value_col = "index",
    group_cols = c("line_number", "metric_abb"),
    methods = c("stl", "ma"),
    params = list(s.window = 21, robust = TRUE, t.window = 13)
  )

psg_trend_lines <- psg_trend_lines |>
  mutate(
    metric = case_when(
      metric_abb == "total" ~ "Total",
      metric_abb == "mdu" ~ "Médias Dias Úteis"
    )
  )

plot_recovery <-
  ggplot(
    psg_trend_lines,
    aes(date, trend_stl, color = as.factor(line_number))
  ) +
  geom_line(lwd = 0.8) +
  geom_hline(yintercept = 100) +
  geom_label(
    data = subset(psg_trend_lines, date == max(date)),
    aes(label = scales::number(trend_stl, accuracy = 0.1, suffix = "%")),
    family = "Lato",
    size = 3,
    nudge_x = 250,
    nudge_y = c(2.5, 0, 0, 2.5, 0, 0)
  ) +
  facet_wrap(vars(metric), scales = "free_y") +
  scale_x_date(
    breaks = seq(as.Date("2018-01-01"), as.Date("2026-01-01"), by = "1 years"),
    date_labels = "%Y",
    expand = expansion(c(0.025, 0.1))
  ) +
  scale_y_continuous(breaks = seq(40, 100, 20), limits = c(40, NA)) +
  scale_color_manual(
    name = NULL,
    values = unname(metro_colors[1:3])
  ) +
  labs(
    title = "Linha 2 teve a melhor recuperação entre as linhas do METRÔ",
    subtitle = "Linha 2-Verde opera em média 10% abaixo da sua capacidade pré-pandemia, mas teve melhor recuperação do que linhas 1-Azul e 3-Vermelha.",
    x = NULL,
    y = "Índice (2019 = 100)"
  ) +
  guides(color = "none") +
  theme_metro +
  theme(
    strip.background = element_rect(fill = colors[2]),
    strip.text = element_text(
      color = "white",
      size = 9,
      family = "Lato"
    )
  )


tab_station_embarques <- station_averages |>
  filter(date == "2025-08-01", line_number == 2) |>
  select(station_name, avg_passenger)

library(bizdays)

metro_line <- st_read(here::here("static/data/spo_metro_line.gpkg"))

metro_line <- metro_line |>
  filter(line_name == "Green")

metro_line_future <- st_read(here::here(
  "static/data/spo_metro_line_future.gpkg"
))

metro_line_future <- metro_line_future |>
  filter(line_name == "Green")


metro_station <- st_read(here::here("static/data/spo_metro_stations.gpkg"))

metro_station <- metro_station |>
  filter(line_name == "Green")

tab_stations_embarques <- station_daily |>
  filter(
    line_number == 2,
    date >= as.Date("2025-01-01"),
    date <= as.Date("2025-12-31")
  ) |>
  mutate(
    is_business_day = as.integer(!is.bizday(date, cal = "Brazil/ANBIMA")),
  ) |>
  summarise(
    media_embarques = mean(passengers, na.rm = TRUE),
    pico_embarques = max(passengers, na.rm = TRUE),
    .by = c("station_name", "is_business_day")
  ) |>
  tidyr::pivot_wider(
    id_cols = "station_name",
    names_from = "is_business_day",
    values_from = c("media_embarques", "pico_embarques")
  )

stations_summary <- left_join(
  metro_station,
  tab_stations_embarques,
  by = "station_name"
)

stations_summary <- stations_summary |>
  mutate(
    popup_html = glue::glue(
      "<div style='font-family: sans-serif; min-width: 180px;'>
    <h4 style='margin: 0 0 8px 0; color: #006D2C;'>{station_name}</h4>
    <table style='border-collapse: collapse; width: 100%;'>
     <tr style='border-bottom: 1px solid #eee;'>
      <td style='padding: 4px 8px 4px 0; color: #555;'>Dias úteis</td>
      <td style='padding: 4px 0; font-weight: bold; text-align: right;'>{fmt_br(media_embarques_0)}</td>
     </tr>
     <tr>
      <td style='padding: 4px 8px 4px 0; color: #555;'>Fins de semana</td>
      <td style='padding: 4px 0; font-weight: bold; text-align: right;'>{fmt_br(media_embarques_1)}</td>
     </tr>
    </table>
    <p style='margin: 6px 0 0 0; font-size: 11px; color: #999;'>Média diária de embarques (2025)</p>
   </div>"
    )
  )
library(leaflet)

metro_icon <- makeAwesomeIcon(
  icon = "train",
  library = "fa",
  markerColor = "black",
  iconColor = main_color
)

leaflet_map <- leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolylines(
    data = metro_line,
    color = main_color,
    weight = 4,
    opacity = 0.8
  ) |>
  addPolylines(
    data = metro_line_future,
    color = main_color,
    weight = 3,
    opacity = 0.6,
    dashArray = "5,10"
  ) |>
  addAwesomeMarkers(
    data = stations_summary,
    popup = ~popup_html,
    icon = metro_icon,
    label = ~station_name
  )

# Exportar ----------------------------------------------------------------

dirpath <- here::here("posts/general-posts/2026-01-line-2-metro")
dirpath_export <- here::here("static/images/spo_metro")

saveRDS(leaflet_map, here::here(dirpath, "leaflet_map.rds"))

ggsave(
  here::here(dirpath_export, "line_2_overview.png"),
  p_flow_total,
  device = ragg::agg_png,
  width = 9,
  height = 5
)

ggsave(
  here::here(dirpath_export, "line_2_station_comp.png"),
  plot_station_comp,
  device = ragg::agg_png,
  width = 7,
  height = 6
)

ggsave(
  here::here(dirpath_export, "line_2_stations.png"),
  plot_stations,
  device = ragg::agg_png,
  width = 7.5,
  height = 6.2
)

ggsave(
  here::here(dirpath_export, "line_2_recovery.png"),
  plot_recovery,
  device = ragg::agg_png,
  width = 7.5,
  height = 3.8
)
