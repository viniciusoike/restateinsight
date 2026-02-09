# Setup -------------------------------------------------------------------

library(dataverse)
library(dplyr)
library(ggplot2)
library(trendseries)
library(lubridate)
library(ggtext)
library(stringr)
library(sf)
library(here)
library(leaflet)

# Data Import -------------------------------------------------------------

dv <- "dataverse.datascience.insper.edu.br"
doi_motiva <- "10.60873/FK2/UTGQ0I"
dset <- get_dataset(doi_motiva, server = dv)

embarques <- get_dataframe_by_name(
  "emb_diarios.rds",
  dataset = doi_motiva,
  server = dv,
  original = TRUE,
  .f = readr::read_rds
)

embarques_medias <- get_dataframe_by_name(
  "emb_media.rds",
  dataset = "10.60873/FK2/BPYHFB",
  server = dv,
  original = TRUE,
  .f = readr::read_rds
)

metro_station <- st_read(here("static/data/spo_metro_stations.gpkg"))
metro_line <- st_read(here("static/data/spo_metro_line.gpkg"))
metro_line_future <- st_read(here("static/data/spo_metro_line_future.gpkg"))

metro_station <- metro_station |>
  filter(line_number == 5) |>
  mutate(
    station_name = str_replace(station_name, "Aacd-Servidor", "AACD - Servidor")
  )

metro_line <- metro_line |>
  filter(line_number == 5)

metro_line_future <- metro_line_future |>
  filter(line_number == 5)

# Data Wrangling ----------------------------------------------------------

tab_stations <- embarques |>
  filter(
    business_unit == "ViaMobilidade - Linha 5",
    tipo_embarque == "Bloqueio"
  ) |>
  mutate(anomes = floor_date(data, "month")) |>
  summarise(
    embarques_estacao = sum(embarques, na.rm = TRUE),
    .by = c("anomes", "station_name")
  )

# Sum last 12 months per station
tab_stations <- tab_stations |>
  mutate(
    acum_embarques = RcppRoll::roll_sumr(embarques_estacao, 12, fill = 0L),
    .by = "station_name"
  )

tab_embarques <- embarques |>
  filter(business_unit == "ViaMobilidade - Linha 5") |>
  summarise(
    embarques_linha = sum(embarques, na.rm = TRUE),
    .by = c("data")
  ) |>
  arrange(data) |>
  mutate(
    ano = year(data),
    mes = month(data),
    mesano = floor_date(data, "month"),
    .before = 1
  )

tab_mes <- tab_embarques |>
  rename(date = mesano) |>
  summarise(
    total = sum(embarques_linha, na.rm = TRUE),
    .by = "date"
  )

tab_dias <- tab_embarques |>
  mutate(
    dia_semana = wday(data, label = FALSE, abbr = FALSE),
    is_weekend = if_else(dia_semana %in% c(1, 7), 1L, 0L)
  ) |>
  summarise(
    total = sum(embarques_linha, na.rm = TRUE),
    .by = c("mesano", "is_weekend")
  )

# Trend decomposition
tab_embarques <- tab_embarques |>
  augment_trends(date_col = "data", value_col = "embarques_linha")

trendstl <- stl(
  ts(tab_embarques$embarques_linha, frequency = 360),
  s.window = 51
)
tab_embarques$trend_stl <- as.numeric(trendstl$time.series[, "trend"])

tab_mes_trend <- tab_mes |>
  mutate(ltotal = log(total)) |>
  augment_trends(
    value_col = "ltotal",
    methods = c("ma", "stl"),
    params = list(s.window = 11)
  ) |>
  mutate(total = exp(ltotal))

trendstl <- stl(
  ts(log(tab_mes$total), frequency = 12, start = c(2018, 8)),
  s.window = 11,
  robust = TRUE
)
tab_mes_trend$trend_stl <- as.numeric(exp(trendstl$time.series[, "trend"]))

# Theme -------------------------------------------------------------------

colors <- c("#9B6BB5")
colors_days <- c("#807dba", "#3f007d")
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
    as.Date("2018-08-01"),
    as.Date("2018-09-01"),
    as.Date("2019-04-01"),
    as.Date("2019-10-01"),
    as.Date("2024-06-01"),
    as.Date("2025-01-01")
  ),
  date_label = c(
    "Ago-18",
    "Set-18",
    "Abr-19",
    "Out-19",
    "Jun-24",
    "Jan-25"
  ),
  label = c(
    "ViaMobilidade\nassume operação\nda linha-5 Lilás",
    "Inauguração\nHospital São Paulo\nSanta Cruz e\nChácara Klabin",
    "Linha 5-Lilás\n é finalizada\napós entrega da\nestação Campo Belo",
    "16,1 milhão\nde passageiros\ntransportados",
    "Aumento\nda tarifa\n(4,40 -> 5,00)",
    "Aumento\nda tarifa\n(5,00 -> 5,20)"
  ),
  xtext = c(
    as.Date("2018-08-01") %m+% months(-5),
    as.Date("2018-09-01") %m+% months(-5),
    as.Date("2019-04-01") %m+% months(-1),
    as.Date("2019-10-01") %m+% months(-4),
    as.Date("2024-06-01"),
    as.Date("2025-01-01") %m+% months(-7)
  ),
  ytext = c(15000, 3500, 3500, 17500, 3500, 18500),
  ytext_label = c(18000, 250, 250, 19500, 750, 18500),
  yend_line = c(17000, 1750, 1750, 18500, 1750, 20000)
)

total_passengers <- tab_mes_trend |>
  rename(value = total) |>
  mutate(
    value = value / 1000,
    trend_stl = trend_stl / 1000
  )

df_labels <- inner_join(df_labels, total_passengers, by = "date")

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
    color = colors,
    alpha = 0.5,
    lwd = 0.5
  ) +
  geom_point(
    data = total_passengers,
    aes(x = date, y = value),
    color = "gray20",
    fill = colors,
    alpha = 0.5,
    size = 2,
    shape = 21
  ) +
  geom_line(
    data = total_passengers,
    aes(x = date, y = trend_stl),
    color = colors,
    lwd = 0.8
  )

plot_labels <- base_plot +
  geom_segment(
    data = df_labels,
    aes(x = date, xend = date, y = yend_line, yend = value),
    color = "gray20"
  ) +
  geom_segment(
    data = slice(df_labels, 3),
    aes(x = date, xend = date %m+% months(6), y = 1750, yend = 1750)
  ) +
  geom_segment(
    data = slice(df_labels, 6),
    aes(x = date, xend = date %m+% months(-6), y = 20000, yend = 20000)
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
    family = "Lato"
  ) +
  geom_label(
    data = df_labels,
    aes(x = xtext, y = ytext_label, label = label),
    size = 2,
    family = "Fira Code",
    border.color = "gray40",
    hjust = c(0, 0, 0, 0, 0.5, 0.5)
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

# Plot: Ranking de Estações -----------------------------------------------

date_recent <- max(tab_stations$anomes, na.rm = TRUE) - months(1)

stations_recent <- tab_stations |>
  filter(anomes == date_recent) |>
  mutate(
    station_name = factor(station_name),
    station_name = forcats::fct_reorder(station_name, acum_embarques),
    acum_embarques = acum_embarques / 1e6,
    rank = rank(-acum_embarques, ties.method = "first"),
    station_label = str_glue("{rank}° - {station_name}"),
    ytext = if_else(acum_embarques < 750, 750, acum_embarques + 50),
    num_label = scales::number(
      acum_embarques,
      decimal.mark = ",",
      big.mark = ".",
      accuracy = 1e-2
    ),
    num_label = if_else(
      station_name == "Alto Da Boa Vista",
      paste(num_label, "milhões de passageiros nos últimos 12 meses"),
      num_label
    )
  )

p_station_ranking <- ggplot(
  stations_recent,
  aes(x = acum_embarques, y = station_name)
) +
  geom_col(width = 0.4, fill = colors) +
  geom_vline(xintercept = 0, color = "#000000", lwd = 0.8) +
  geom_text(
    aes(x = 0.1, label = station_label),
    size = 3,
    family = "Lato",
    color = "#000000",
    hjust = 0,
    nudge_y = 0.5,
    fontface = "bold"
  ) +
  geom_text(
    aes(x = acum_embarques, label = num_label),
    size = 3,
    family = "Lato",
    color = "#000000",
    hjust = 0,
    nudge_x = 0.15
  ) +
  scale_y_discrete(expand = expansion(0.075)) +
  labs(
    title = "Estações mais movimentadas (últimos 12 meses)",
    subtitle = "Total de passageiros transportados por estação da linha 5-Lilás (Abr/24 - Mar/25)",
    x = "Milhões de passageiros",
    caption = "Fonte: ONMS (Motiva) | @viniciusoike"
  ) +
  theme_metro +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Plot: Dias Úteis vs Fins de Semana --------------------------------------

base_index <- tab_dias |>
  filter(mesano >= as.Date("2019-01-01"), mesano <= as.Date("2019-12-31")) |>
  summarise(
    base = mean(total, na.rm = TRUE),
    .by = "is_weekend"
  )

tab_dias <- tab_dias |>
  left_join(base_index, by = "is_weekend") |>
  mutate(
    index = total / base * 100,
    is_weekend = factor(is_weekend)
  )

tab_dias_trend <- tab_dias |>
  augment_trends(
    date_col = "mesano",
    value_col = "index",
    group_vars = "is_weekend",
    methods = c("ma", "stl"),
    params = list(s.window = 11)
  )

tab_dias_trend <- tab_dias_trend |>
  mutate(
    weekend_label = if_else(is_weekend == 1, "Finais de Semana", "Dias Úteis")
  )

subtitle <- str_glue(
  "Índice médio de passageiros da Linha 5-Lilás em <b style='color:{colors_days[2]}'>finais de semana</b> e <b style='color:{colors_days[1]}'>dias úteis</b>."
)

p_weekday <- ggplot(
  subset(tab_dias_trend, mesano >= as.Date("2019-01-01")),
  aes(mesano, trend_stl, color = is_weekend)
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
  geom_line(lwd = 0.8) +
  geom_hline(yintercept = 100, lwd = 0.8) +
  geom_point(
    data = subset(tab_dias_trend, mesano >= as.Date("2019-01-01")),
    aes(mesano, index, fill = is_weekend),
    size = 2,
    shape = 21,
    alpha = 0.5
  ) +
  facet_wrap(vars(weekend_label), nrow = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(20, 120, 20)) +
  scale_color_manual(values = colors_days) +
  scale_fill_manual(values = colors_days) +
  guides(color = "none", fill = "none") +
  labs(
    title = "Linha 5-Lilás recuperou o fluxo de passageiros após a pandemia",
    subtitle = subtitle,
    x = NULL,
    y = "Índice (100 = média de 2019)"
  ) +
  theme_metro +
  theme(
    plot.subtitle = element_textbox_simple(
      margin = margin(2.5, 0, 5.5, 0),
      size = 10
    ),
    strip.text = element_text(size = 12)
  )

# Plot: Painel por Estação ------------------------------------------------

base_index_stations <- tab_stations |>
  filter(anomes >= as.Date("2019-01-01"), anomes <= as.Date("2019-12-31")) |>
  summarise(
    base = mean(embarques_estacao, na.rm = TRUE),
    .by = "station_name"
  )

tab_stations_index <- tab_stations |>
  left_join(base_index_stations, by = "station_name") |>
  mutate(
    index = embarques_estacao / base * 100,
    station_label = factor(station_name, levels = metro_station$station_name)
  )

tab_stations_trend <- tab_stations_index |>
  augment_trends(
    date_col = "anomes",
    value_col = "index",
    group_vars = "station_name",
    methods = c("ma", "stl"),
    params = list(s.window = 11, robust = TRUE)
  )

stnames <- unique(tab_stations_trend$station_label)

plot_panel <- function(stations) {
  dat <- subset(tab_stations_trend, station_label %in% stations)

  ggplot(dat, aes(x = anomes, y = trend_stl)) +
    geom_hline(yintercept = 100, lwd = 0.8) +
    geom_point(
      data = dat,
      aes(x = anomes, y = index),
      size = 0.8,
      shape = 21,
      alpha = 0.5,
      fill = "white",
      color = colors
    ) +
    geom_line(lwd = 0.8, color = colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(vars(station_label)) +
    scale_y_continuous(breaks = seq(0, 200, 20)) +
    labs(
      title = "Atividade de embarques por estação",
      subtitle = "Índice de embarques (100 = média de 2019)",
      x = NULL,
      y = NULL,
      caption = "Fonte: ONMS (Motiva) | @viniciusoike"
    ) +
    theme_metro +
    theme(
      strip.background = element_rect(
        fill = RColorBrewer::brewer.pal(9, "Purples")[7]
      ),
      strip.text = element_text(color = "#ffffff", size = 9, family = "Lato"),
      plot.background = element_rect(color = "gray20"),
      panel.background = element_rect(color = "gray20")
    )
}

panel_plots <- list(
  plot_panel(stnames[1:4]),
  plot_panel(stnames[5:8]),
  plot_panel(stnames[9:12]),
  plot_panel(stnames[13:16])
)

# Mapa Interativo ---------------------------------------------------------

tab_station_embarques <- embarques |>
  filter(business_unit == "ViaMobilidade - Linha 5") |>
  filter(data >= as.Date("2025-01-01")) |>
  mutate(
    weekday = wday(data, label = FALSE, abbr = FALSE),
    is_weekend = if_else(weekday %in% c(1, 7), 1L, 0L)
  ) |>
  summarise(
    media_embarques = mean(embarques, na.rm = TRUE),
    pico_embarques = max(embarques, na.rm = TRUE),
    .by = c("station_name", "is_weekend")
  ) |>
  tidyr::pivot_wider(
    id_cols = "station_name",
    names_from = "is_weekend",
    values_from = c("media_embarques", "pico_embarques")
  )

stations_summary <- left_join(
  metro_station,
  tab_station_embarques,
  by = "station_name"
)

stations_summary <- stations_summary |>
  mutate(
    popup_html = glue::glue(
      "<div style='font-family: sans-serif; min-width: 180px;'>
        <h4 style='margin: 0 0 8px 0; color: #9d4edd;'>{station_name}</h4>
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

metro_icon <- makeAwesomeIcon(
  icon = "train",
  library = "fa",
  markerColor = "purple"
)

leaflet_map <- leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolylines(
    data = metro_line,
    color = "#9d4edd",
    weight = 4,
    opacity = 0.8
  ) |>
  addPolylines(
    data = metro_line_future,
    color = "#9d4edd",
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

dirpath <- here("posts/general-posts/2026-01-line-5-metro")

saveRDS(
  list(station_ranking = p_station_ranking, weekday = p_weekday),
  here(dirpath, "plots.rds")
)

saveRDS(panel_plots, here(dirpath, "panel_plots.rds"))

saveRDS(leaflet_map, here(dirpath, "leaflet_map.rds"))

ggsave(
  here("static/images/spo_metro/line_5_v2_station_ranking.png"),
  p_station_ranking,
  device = ragg::agg_png,
  width = 7.5,
  height = 6.2
)

ggsave(
  here("static/images/spo_metro/line_5_v2_pandemic.png"),
  p_weekday,
  device = ragg::agg_png,
  width = 7,
  height = 6
)

ggsave(
  here("static/images/spo_metro/line_5_v2_overview.png"),
  p_flow_total,
  device = ragg::agg_png,
  width = 9,
  height = 5
)
