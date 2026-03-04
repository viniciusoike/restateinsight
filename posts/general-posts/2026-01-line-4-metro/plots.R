# Setup -------------------------------------------------------------------

library(dataverse)
library(dplyr)
library(ggplot2)
library(trendseries)
library(lubridate)
library(ggtext)
library(stringr)
library(leaflet)
library(sf)
library(ggiraph)
library(patchwork)

import::from(here, here)


# Data Import -------------------------------------------------------------

dv <- "dataverse.datascience.insper.edu.br"
doi_motiva <- "10.60873/FK2/UTGQ0I"
dset <- get_dataset(doi_motiva, server = dv)

embarques_hora <- get_dataframe_by_name(
  "emb_horarios.rds",
  dataset = "10.60873/FK2/9MZGJL",
  server = dv,
  original = TRUE,
  .f = readr::read_rds
)

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
  filter(line_number == 4)

metro_line <- metro_line |>
  filter(line_number == 4)

metro_line_future <- metro_line_future |>
  filter(line_number == 4)

# Data Wrangling ----------------------------------------------------------

tab_stations <- embarques |>
  filter(
    business_unit == "ViaQuatro",
    !is.na(tipo_embarque)
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
  filter(business_unit == "ViaQuatro", !is.na(tipo_embarque)) |>
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

tab_dias_stations <- embarques |>
  filter(business_unit == "ViaQuatro", !is.na(tipo_embarque)) |>
  mutate(
    mesano = floor_date(data, "month"),
    dia_semana = wday(data, label = FALSE, abbr = FALSE),
    is_weekend = if_else(dia_semana %in% c(1, 7), 1L, 0L)
  ) |>
  summarise(
    total = sum(embarques, na.rm = TRUE),
    .by = c("mesano", "station_name", "is_weekend")
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
    params = list(s.window = 13, robust = TRUE, t.window = 13)
  ) |>
  mutate(trend_stl = exp(trend_stl))


tab_mes_trend |>
  mutate(
    ano = lubridate::year(date),
  )

# library(bsts)

# metro_ts <- ts(log(tab_mes$total), frequency = 12, start = c(2012, 1))
# ss <- list()
# ss <- AddLocalLinearTrend(ss, metro_ts)
# ss <- AddSeasonal(ss, metro_ts, nseasons = 12)
# fit <- bsts(metro_ts, state.specification = ss, niter = 1000)
# plot(fit)
# plot(fit, "components")

# fit <- StructTS(metro_ts, type = "BSM")

# trend <- extract_trends(metro_ts, methods = "kalman")
# plot.ts(metro_ts)
# lines(trend, col = "red")

# fit <- StructTS(metro_ts, type = "BSM", fixed = c(0.1, 0.1, NA, NA))
# plot(tsSmooth(fit))
# plot(tsSmooth(fit))

# # Tendência pré-pandemia
# pre <- tab_mes |> filter(date < as.Date("2020-03-01"))
# fit <- lm(total ~ date, data = pre)

# tab_mes <- tab_mes |>
#   mutate(counterfactual = predict(fit, newdata = tab_mes))

# ggplot(tab_mes, aes(x = date)) +
#   geom_line(aes(y = total)) +
#   geom_line(aes(y = counterfactual), linetype = "dashed", color = "red") +
#   labs(y = "Embarques")

# tab_mes |>
#   mutate(
#     year = lubridate::year(date),
#     # Índice: jan/2020 = 100
#     index = total / mean(total[year == 2019]) * 100
#   ) |>
#   ggplot(aes(date, index)) +
#   geom_line() +
#   geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
#   annotate(
#     "text",
#     x = as.Date("2019-06-01"),
#     y = 103,
#     label = "Média 2019",
#     color = "red"
#   ) +
#   labs(y = "Índice (média 2019 = 100)")

embarques_hora |>
  filter(business_unit == "ViaQuatro") |>
  filter(
    data >= as.Date("2025-04-07"),
    data <= as.Date("2025-04-11"),
    station_name == "Luz"
  ) |>
  mutate(
    ymdhm = lubridate::ymd_hm(str_glue(
      "{data} {str_pad(hora, 2, pad = '0')}:00"
    ))
  ) |>
  ggplot(aes(ymdhm, embarques)) +
  geom_line()

embarques_hora |>
  filter(business_unit == "ViaQuatro") |>
  filter(
    station_name == "São Paulo - Morumbi"
  ) |>
  group_by(hora) |>
  slice_max(embarques, n = 1) |>
  print(n = 24)

# Show do Bruno Mars no Estádio Morumbi
event_day <- as.Date("2024-10-13")

tab_embarques_noturnos <- embarques_hora |>
  filter(business_unit == "ViaQuatro", station_name == "São Paulo - Morumbi") |>
  filter(hora %in% c(0, 21, 22, 23), embarques < 1e5) |>
  mutate(
    hora = if_else(hora == 0, 24, hora),
    data = if_else(hora == 24, data - days(1), data),
    day_week = lubridate::wday(data),
    is_weekend = if_else(day_week %in% c(1, 7), 1L, 0L)
  ) |>
  arrange(data) |>
  summarise(
    total = sum(embarques, na.rm = TRUE),
    .by = c("data", "is_weekend")
  )

# tab_embarques_noturnos |>
#   filter(is_weekend == 1) |>
#   mutate(ano = lubridate::year(data)) |>
#   ggplot(aes(total)) +
#   geom_histogram(bins = 50, color = "white", lwd = 0.1) +
#   facet_wrap(vars(ano))
# tab_embarques_noturnos |>
#  filter(is_weekend == 1) |>
#  arrange(desc(total)) |>
#  slice(1:20) |>
#  mutate(data = as.character(data)) |>
#  pull(data) |>
#  dput()

# Theme -------------------------------------------------------------------
main_color <- "#F2A900"
colors_days <- c("#2C6E8A", "#D4960A")
colors <- c("#D4960A", "#E0AA3E")

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
    as.Date("2013-06-01"),
    as.Date("2014-11-01"),
    as.Date("2018-01-01"),
    as.Date("2018-04-01"),
    as.Date("2018-10-01"),
    as.Date("2020-01-01"),
    as.Date("2021-12-01"),
    as.Date("2024-01-01"),
    as.Date("2025-01-01")
  ),
  date_label = c(
    "Jun-13",
    "Nov-14",
    "Jan-18",
    "Abr-18",
    "Out-18",
    "Jan-20",
    "Dez-21",
    "Jan-24",
    "Jan-25"
  ),
  label = c(
    "Aumento leva às\nJornadas de Junho\n(3,00 -> 3,20 -> 3,00)",
    "Inauguração\nFradique\nCoutinho",
    "Inauguração\nHigienópolis\nMackenzie",
    "Inauguração\nOscar Freire\n",
    "Inauguração\nSão Paulo\nMorumbi",
    "Último aumento\ntarifário\nantes da pandemia\n(4,30 -> 4,40)",
    "Inauguração\nVila Sônia\n",
    "Aumento\nda tarifa\n(4,40 -> 5,00)",
    "Aumento\nda tarifa\n(5,00 -> 5,20)"
  ),
  xtext = c(
    as.Date("2013-06-02") %m+% months(0),
    as.Date("2014-11-15") %m+% months(0),
    as.Date("2018-01-01"),
    as.Date("2018-04-01") %m+% months(0),
    as.Date("2018-10-01") %m+% months(0),
    as.Date("2020-01-01") %m+% months(0),
    as.Date("2021-12-01"),
    as.Date("2024-01-01"),
    as.Date("2025-01-06") %m+% months(0)
  ),
  ytext_label = c(2000, 5000, 7000, 25000, 2000, 25000, 2000, 2000, 7000),
  ytext_date_label = case_when(
    ytext_label < 20000 ~ ytext_label + 2000,
    date_label == "Jan-20" ~ ytext_label - 2000,
    TRUE ~ ytext_label - 2000
  )
)

total_passengers <- tab_mes_trend |>
  rename(value = total) |>
  mutate(
    value = value / 1000,
    trend_stl = trend_stl / 1000
  )

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
    title = "Total de Passageiros (Linha-4, Amarela)",
    subtitle = "Numero total de passageiros transportados na linha-4 Amarela (incluindo baldeações) a cada mês.",
    x = NULL,
    y = "Milhões",
    caption = "Fonte: ONMS (Motiva). @viniciusoike"
  ) +
  theme_metro +
  theme(
    axis.ticks.x = element_line(linewidth = 0.5)
  )

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
      accuracy = 0.1
    ),
    num_label = if_else(
      station_name == "Fradique Coutinho",
      paste(num_label, "milhões de passageiros nos últimos 12 meses"),
      num_label
    )
  )

p_station_ranking <- ggplot(
  stations_recent,
  aes(x = acum_embarques, y = station_name)
) +
  geom_col(width = 0.4, fill = colors[1]) +
  geom_vline(xintercept = 0, color = "#000000", lwd = 0.8) +
  # Text labels with station names
  geom_text(
    aes(x = 0.5, label = station_label),
    size = 3,
    family = "Lato",
    color = "#000000",
    hjust = 0,
    nudge_y = 0.375,
    fontface = "bold"
  ) +
  # Text labels with numbers of passengers
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
    subtitle = "Total de passageiros transportados por estação da linha 4-Amarela (Maio/24 - Abr/25)",
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
    params = list(s.window = 13, robust = TRUE)
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

# 1. Base por estação (já remove Vila Sônia uma única vez)
tab_emb_stations <- embarques |>
  filter(
    business_unit == "ViaQuatro",
    !is.na(tipo_embarque),
    station_name != "Vila Sônia"
  ) |>
  summarise(
    total = sum(embarques, na.rm = TRUE),
    .by = c("data", "station_name")
  ) |>
  mutate(
    ano_mes = floor_date(data, "month"),
    weekday = wday(data),
    is_weekend = if_else(weekday %in% c(1, 7), 1L, 0L)
  )

# 2. Empilha sistema + estações
tab_emb_all <- tab_emb_stations |>
  summarise(total = sum(total), .by = c("data", "ano_mes", "is_weekend")) |>
  mutate(station_name = "Total Sistema") |>
  bind_rows(tab_emb_stations)

# 3. Médias diárias por grupo
tab_medias <- tab_emb_all |>
  summarise(
    avg = mean(total, na.rm = TRUE),
    .by = c("ano_mes", "station_name", "is_weekend")
  )

# 4. Base 2019 + índice
tab_index <- tab_medias |>
  left_join(
    tab_medias |>
      filter(ano_mes >= "2019-01-01", ano_mes <= "2019-12-31") |>
      summarise(
        base = mean(avg, na.rm = TRUE),
        .by = c("station_name", "is_weekend")
      ),
    by = c("station_name", "is_weekend")
  ) |>
  mutate(
    index = avg / base * 100,
    station_name = str_replace(station_name, " - ", "-"),
    station_label = factor(
      station_name,
      levels = c(metro_station$station_name, "Total Sistema")
    ),
    is_system = factor(if_else(station_name == "Total Sistema", 1L, 0L))
  )

# 5. Tendências suavizadas
tab_stations_index_trend <- tab_index |>
  augment_trends(
    date_col = "ano_mes",
    value_col = "index",
    group_vars = c("station_name", "is_weekend"),
    methods = c("ma", "stl", "kalman"),
    params = list(s.window = 13, robust = TRUE)
  )

tab_stations_index_trend |>
  filter(is_weekend == 0, ano_mes == max(ano_mes)) |>
  tail(10)

# Nível antigo da Paulista, República e Pinheiros
t0 <- tab_stations_index_trend |>
  filter(
    is_weekend == 0,
    ano_mes == as.Date("2019-04-01"),
    station_name %in% c("Pinheiros", "República", "Paulista")
  ) |>
  summarise(total = sum(avg))

# tab_stations_index_trend |>
#   filter(
#     is_weekend == 0,
#     ano_mes == as.Date("2019-04-01")
#   )

# Fluxo atual da linha (somente Paulista, República e Pinheiros)
c0 <- tab_stations_index_trend |>
  filter(
    is_weekend == 0,
    ano_mes == max(ano_mes),
    station_name %in% c("Paulista", "República", "Pinheiros")
  ) |>
  summarise(total = sum(avg))

# Fluxo atual da linha (sem Paulista, República e Pinheiros)
c1 <- tab_stations_index_trend |>
  filter(
    is_weekend == 0,
    ano_mes == max(ano_mes),
    !station_name %in% c("Total Sistema", "Pinheiros", "República", "Paulista")
  ) |>
  summarise(total = sum(avg))

# 6. Gráfico
subdat <- tab_stations_index_trend |>
  filter(ano_mes >= as.Date("2019-01-01"), is_weekend == 0)


p_stations_weekday <- ggplot(
  subdat,
  aes(ano_mes, trend_stl, color = is_system)
) +
  geom_point(aes(y = index), alpha = 0.5, size = 0.5) +
  geom_line(lwd = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40") +
  facet_wrap(vars(station_label)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 140)) +
  scale_color_manual(values = rev(colors_days)) +
  labs(
    title = "Atividade de Embarques por Estação",
    subtitle = "Índice de Embarques em dias úteis (100 = média de 2019)",
    x = NULL,
    y = NULL,
    caption = "Fonte: ONMS (Motiva) | @viniciusoike"
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

subdat <- tab_stations_index_trend |>
  filter(ano_mes >= as.Date("2019-01-01"), is_weekend == 1)

p_stations_weekend <-
  ggplot(subdat, aes(ano_mes, trend_stl, color = is_system)) +
  geom_point(aes(y = index), alpha = 0.5, size = 0.5) +
  geom_line(lwd = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40") +
  facet_wrap(vars(station_label)) +
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 140)) +
  scale_color_manual(values = rev(colors_days)) +
  labs(
    title = "Atividade de Embarques por Estação",
    subtitle = "Índice de Embarques em finais de semana (100 = média de 2019)",
    x = NULL,
    caption = "Fonte: ONMS (Motiva) | @viniciusoike"
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

# tab_stations_index_trend |>
#   filter(station_name == "Total Sistema") |>
#   group_by(is_weekend) |>
#   slice_tail(n = 10) |>
#   select(ano_mes, station_name, is_weekend, index, trend_stl, trend_ma, trend_kalman)

# tab_index |>
#   filter(ano_mes >= max(ano_mes) %m-% months(11)) |>
#   summarise(avg_index = mean(index, na.rm = TRUE), .by = is_weekend)

# Shows e Eventos --------------------------------------------------------

#| fmt: skip
eventos_morumbi <- tibble::tribble(
  ~data        , ~tipo_evento     , ~desc_short                      , ~desc_long                                                                    ,
  "2019-07-13" , "jogo"           , "São Paulo x Palmeiras (2019)"   , "São Paulo 1 x 1 Palmeiras (Brasileirão)."                                    ,
  "2019-10-06" , "show"           , "Show Iron Maiden (2019)"        , "Iron Maiden – tour Legacy of the Beast."                                     ,
  "2020-02-08" , "show_religioso" , "The Send Brasil"                , "The Send Brasil, megaevento evangélico no Morumbi (10h–22h)."                ,
  "2020-02-15" , "jogo"           , "São Paulo x Corinthians (2020)" , "São Paulo 0 x 0 Corinthians (Paulistão, 6ª rodada, público 44.280)."         ,
  "2022-09-04" , "show"           , "Show Iron Maiden (2022)"        , "Iron Maiden – tour Legacy of the Beast/Senjutsu."                            ,
  "2023-01-29" , "jogo"           , "São Paulo x Corinthians"        , "São Paulo 1 x 2 Corinthians (Paulistão, 5ª rodada, público 54.970)."         ,
  "2023-03-11" , "show"           , "Show Coldplay (2023/2)"         , "Coldplay – Music of the Spheres World Tour (2ª noite)."                      ,
  "2023-03-17" , "show"           , "Show Coldplay (2023/5)"         , "Coldplay – Music of the Spheres World Tour (5ª noite)."                      ,
  "2023-03-18" , "show"           , "Show Coldplay (2023/6)"         , "Coldplay – Music of the Spheres World Tour (6ª noite)."                      ,
  "2023-05-20" , "jogo"           , "São Paulo x Vasco"              , "São Paulo 4 x 2 Vasco (Brasileirão Série A, recorde de público no Morumbi)." ,
  "2023-11-12" , "show"           , "Show RBD (2023/1)"              , "RBD – Soy Rebelde Tour (1ª noite)."                                          ,
  "2023-11-13" , "show"           , "Show RBD (2023/2)"              , "RBD – Soy Rebelde Tour (2ª noite)."                                          ,
  "2024-01-20" , "jogo"           , "São Paulo x Santo André"        , "São Paulo 3 x 1 Santo André (Paulistão, 1ª rodada, público 45.270)."         ,
  "2024-03-03" , "jogo"           , "São Paulo x Palmeiras (2024)"   , "São Paulo 1 x 1 Palmeiras (Paulistão, 11ª rodada, público 50.000+)."         ,
  "2024-09-07" , "show"           , "Show The Weeknd"                , "The Weeknd – show no MorumBIS."                                              ,
  "2024-10-05" , "show"           , "Show Bruno Mars (2024/2)"       , "Bruno Mars – tour Live in BRAZIL (2ª noite)."                                ,
  "2024-10-12" , "show"           , "Show Bruno Mars (2024/5)"       , "Bruno Mars – tour Live in BRAZIL (5ª noite)."                                ,
  "2024-10-13" , "show"           , "Show Bruno Mars (2024/6)"       , "Bruno Mars – tour Live in BRAZIL (6ª noite)."                                ,
  "2024-12-14" , "show_festival"  , "Show Gusttavo Lima"             , "Buteco do Gusttavo Lima, festival no MorumBIS."                              ,
  "2025-01-26" , "jogo"           , "São Paulo x Corinthians (2025)" , "São Paulo 3 x 1 Corinthians (Paulistão, 4ª rodada)."                         ,
  "2025-04-05" , "show"           , "Show Stray Kids (2025/1)"       , "Stray Kids – show de K-pop no MorumBIS (1ª noite)."                          ,
  "2025-04-06" , "show"           , "Show Stray Kids (2025/2)"       , "Stray Kids – show de K-pop no MorumBIS (2ª noite)."
)

eventos_morumbi <- eventos_morumbi |>
  mutate(data = as.Date(data))


## Histograma e boxplot ---------------------------------------------------

xbreaks <- c(200, 500, 1000, 2000, 5000, 10000, 15000, 20000)

embarques_noturnos_fds <- tab_embarques_noturnos |>
  filter(is_weekend == 1, total > 100) |>
  mutate(
    ltotal = log(total),
    is_highlight = factor(if_else(total >= 10300, 1L, 0L))
  )

df_label_highlight <- tibble(
  x = 8000,
  y = 120,
  label = str_wrap(
    "Em dias de eventos, o fluxo de embarques é mais de 15x maior, variando entre 10-15 mil e podendo chegar a quase 18 mil",
    21
  )
)

tab_summary <- tab_embarques_noturnos |>
  filter(is_weekend == 1) |>
  summarise(
    avg = mean(total, na.rm = TRUE),
    q25 = quantile(total, 0.1, na.rm = TRUE),
    med = quantile(total, 0.5, na.rm = TRUE),
    q75 = quantile(total, 0.75, na.rm = TRUE),
    p90 = quantile(total, 0.90, na.rm = TRUE),
    p95 = quantile(total, 0.95, na.rm = TRUE),
    p100 = quantile(total, 1, na.rm = TRUE)
  )


df_label <- tibble(
  x = tab_summary$q25,
  xend = tab_summary$q75,
  y = 180,
  yend = 180,
  label = str_wrap(
    str_glue(
      "O fluxo típico de passageiros aos finais de semana fica entre {round(x, -1)} e {format(round(xend, -1), big.mark = '.')} embarques."
    ),
    31
  ),
)


### Histograma -------------------------------------------------------------

p_embarques_hist <- ggplot() +
  geom_histogram(
    data = embarques_noturnos_fds,
    aes(x = ltotal, fill = is_highlight),
    bins = 14,
    color = "white",
    lwd = 0.1
  ) +
  geom_hline(yintercept = 0, lwd = 1) +
  geom_label(
    data = df_label,
    aes(x = log(x), y = y + 20, label = label),
    size = 3,
    family = "Lato",
    color = "gray20",
    hjust = 0
  ) +
  # geom_segment(
  #   data = df_label,
  #   aes(x = log(x), xend = log(xend), y = y, yend = yend),
  #   color = "gray30",
  #   lwd = 0.5,
  #   arrow = arrow(length = unit(0.1, "cm"), ends = "both")
  # ) +
  geom_segment(
    data = tibble(x = log(13000), xend = log(13000), y = 90, yend = 25),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "gray30",
    lwd = 0.5,
    arrow = arrow(length = unit(0.1, "cm"))
  ) +
  geom_label(
    data = df_label_highlight,
    aes(x = log(x), y = y, label = label),
    size = 3,
    family = "Lato",
    color = "gray20",
    hjust = 0
  ) +
  scale_x_continuous(
    breaks = log(xbreaks),
    labels = scales::number(xbreaks, big.mark = ".")
  ) +
  scale_y_continuous(limits = c(NA, 210), expand = expansion(c(0, 0.1))) +
  scale_fill_manual(values = c("gray50", main_color)) +
  labs(
    title = "Madrugadas lotadas: eventos aumentam em mais de 15x fluxo do metrô",
    subtitle = "Total de embarques entre 21h até horário de fechamento da estação São Paulo - Morumbi de novembro de 2018 a abril de 2025.\nBoxplot no painel inferior é proporcional ao gráfico principal e mostra distribuição do número de embarques.",
    x = "Número de embarques (escala log)",
    y = "Frequência"
  ) +
  guides(fill = "none") +
  theme_metro

gb <- ggplot_build(p_embarques_hist)


### Boxplot ----------------------------------------------------------------

p_embarques_boxplot <- ggplot(embarques_noturnos_fds, aes(x = ltotal, y = 1)) +
  geom_vline(xintercept = log(xbreaks), color = "gray50", lwd = 0.5) +
  geom_boxplot(width = 0.5) +
  scale_x_continuous(
    limits = gb$layout$panel_params[[1]]$x.range,
    breaks = log(xbreaks),
    labels = scales::number(xbreaks, big.mark = "."),
    expand = expansion(0)
  ) +
  scale_y_continuous(expand = expansion(0.25)) +
  theme_metro +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )


### Painel -----------------------------------------------------------------

panel <- (p_embarques_hist / p_embarques_boxplot) +
  plot_layout(heights = c(1, 0.2), guides = "collect")

## Top Eventos (interativo) -----------------------------------------------

tab_top_eventos <- tab_embarques_noturnos |>
  filter(is_weekend == 1) |>
  slice_max(total, n = 20) |>
  left_join(eventos_morumbi, by = "data") |>
  mutate(
    desc_short = factor(desc_short),
    desc_short = forcats::fct_reorder(desc_short, total),
    is_show = if_else(
      tipo_evento %in% c("show", "show_religioso", "show_festival"),
      1L,
      0L
    ),
    is_show = factor(is_show),
    tooltip = glue::glue(
      "<div style='font-family: Lato, sans-serif; padding: 6px 10px;'>
        <div style='color: #888; font-size: 11px;'>{format(data, '%d/%m/%Y')}</div>
        <div style='font-weight: bold; font-size: 13px;'>{desc_short}</div>
        <div style='font-size: 12px; margin-top: 4px;'>{desc_long}</div>
        <div style='font-size: 12px; margin-top: 4px;'>Embarques: {format(total, big.mark = '.')}</div>
      </div>"
    )
  )

tab_top_eventos_static <- tab_top_eventos |>
  slice(1:10) |>
  mutate(
    num_label = scales::number(
      total,
      decimal.mark = ",",
      scale = 0.001,
      accuracy = 0.1
    ),
    num_label = if_else(
      desc_short == "Show Bruno Mars (2024/5)",
      paste(num_label, "mil embarques"),
      num_label
    )
  )

# Versão estática para export
p_eventos_static <- ggplot(
  tab_top_eventos_static,
  aes(total, desc_short, fill = is_show)
) +
  geom_col(width = 0.8) +
  # Labels do evento
  geom_text(
    aes(x = 500, label = desc_short),
    size = 4,
    family = "Lato",
    color = "#ffffff",
    hjust = 0
  ) +
  # Labels do número de embarques
  geom_text(
    aes(x = total, label = num_label),
    size = 4,
    family = "Lato",
    color = "#ffffff",
    hjust = 1,
    nudge_x = -500
  ) +
  geom_vline(xintercept = 0, lwd = 0.8) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = ".", scale = 0.001),
    expand = expansion(c(0, 0.05))
  ) +
  scale_fill_manual(values = colors_days) +
  guides(fill = "none") +
  labs(
    title = "Voltando de metrô do show",
    subtitle = "Eventos no Morumbi que mais movimentaram a estação da linha 4 (2018-abr/2025).\nValores indicam o número total de embarques na estação das 21h até o horário de fechamento.",
    x = "Embarques (mil)",
    y = NULL
  ) +
  theme_metro +
  theme(
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  )

p_eventos <- ggplot(tab_top_eventos, aes(total, desc_short, fill = is_show)) +
  geom_col_interactive(aes(tooltip = tooltip, data_id = desc_short)) +
  geom_vline(xintercept = 0, lwd = 0.8) +
  scale_x_continuous(
    labels = scales::label_number(big.mark = ".", scale = 0.001),
    expand = expansion(c(0, 0.05))
  ) +
  scale_fill_manual(values = colors_days) +
  guides(fill = "none") +
  labs(
    title = "Voltando pra casa de metrô",
    subtitle = "Eventos no Morumbi que mais movimentaram a estação da linha 4 (2018-abr/2025).\nValores indicam o número total de embarques na estação das 21h até o horário de fechamento.",
    x = "Embarques (mil)",
    y = NULL
  ) +
  theme_metro +
  theme(
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 12),
    text = element_text(family = "Helvetica", size = 12),
    plot.title.position = "plot",
    panel.grid.major.y = element_blank()
  )

plot_interative <- girafe(
  ggobj = p_eventos,
  width_svg = 8,
  height_svg = 7,
  options = list(
    opts_hover(
      css = "stroke:black;stroke-width:1px;cursor:pointer;opacity:0.7;"
    ),
    opts_tooltip(
      css = "background-color: white; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); border: 1px solid #e0e0e0;",
      use_fill = FALSE
    )
  )
)


# Mapa Interativo ---------------------------------------------------------

tab_station_embarques <- embarques |>
  filter(business_unit == "ViaQuatro") |>
  filter(data >= as.Date("2025-01-01")) |>
  mutate(
    # Make stations names standardized between datasets
    station_name = str_replace(station_name, " - ", "-"),
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
    <h4 style='margin: 0 0 8px 0; color: #D4960A;'>{station_name}</h4>
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
  markerColor = "black",
  iconColor = "#D4960A"
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

dirpath <- here("posts/general-posts/2026-01-line-4-metro")

# panel_plots <- list(
#   "weekday" = p_stations_weekday,
#   "weekend" = p_stations_weekend
# )

# saveRDS(plot_interative, here(dirpath, "plot_interative.rds"))
# saveRDS(panel_plots[[1]], here(dirpath, "panel_plots_weekday.rds"))
# saveRDS(panel_plots[[2]], here(dirpath, "panel_plots_weekend.rds"))
# saveRDS(leaflet_map, here(dirpath, "leaflet_map.rds"))

dirpath_export <- here("static/images/spo_metro")

# ggsave(
#   here("static/images/spo_metro/line_4_station_weekday.png"),
#   panel_plots[[1]],
#   device = ragg::agg_png,
#   width = 7.5,
#   height = 6.2
# )

# ggsave(
#   here("static/images/spo_metro/line_4_station_weekend.png"),
#   panel_plots[[2]],
#   device = ragg::agg_png,
#   width = 7.5,
#   height = 6.2
# )

# ggsave(
#   here("static/images/spo_metro/line_4_v2_station_ranking.png"),
#   p_station_ranking,
#   device = ragg::agg_png,
#   width = 7.5,
#   height = 6.2
# )

# ggsave(
#   here("static/images/spo_metro/line_4_v2_pandemic.png"),
#   p_weekday,
#   device = ragg::agg_png,
#   width = 7,
#   height = 6
# )

# ggsave(
#   here("static/images/spo_metro/line_4_v2_overview.png"),
#   p_flow_total,
#   device = ragg::agg_png,
#   width = 9,
#   height = 5
# )

# ggsave(
#   here("static/images/spo_metro/line_4_eventos.png"),
#   p1,
#   device = ragg::agg_png,
#   width = 7.1,
#   height = 5.6
# )

# ggsave(
#   here("static/images/spo_metro/line_4_eventos_panel.png"),
#   panel,
#   device = ragg::agg_png,
#   width = 7.1,
#   height = 5.5
# )

plots_linkedin <- list(
  "carrossel_1" = p_flow_total,
  "carrossel_2" = p_station_ranking,
  "carrossel_3" = p_stations_weekday,
  "carrossel_4" = p_embarques_hist +
    labs(
      subtitle = "Total de embarques entre 21h até horário de fechamento da estação São Paulo - Morumbi de novembro de 2018 a abril de 2025."
    ),
  "carrossel_5" = p_eventos_static
)

dirpath_export <- here("static/images/spo_metro")
dirpath_linkedin <- here(dirpath_export, "linkedin")
fs::dir_create(dirpath_linkedin)

params <- tibble(
  file_name = names(plots_linkedin),
  w = c(7, 6, 7, 7, 6),
  h = c(5.5, 5.5, 7, 3.8, 5.5)
)

for (i in seq_along(plots_linkedin)) {
  if (i %in% c(2, 3, 4, 5)) {
    plot <- plots_linkedin[[i]] +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.border = element_blank()
      )
  } else {
    plot <- plots_linkedin[[i]] +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "#000")
      )
  }

  ggsave(
    here(dirpath_linkedin, paste0(params$file_name[i], ".png")),
    plot,
    width = params$w[i],
    height = params$h[i],
    device = ragg::agg_png,
    bg = "transparent"
  )
}
