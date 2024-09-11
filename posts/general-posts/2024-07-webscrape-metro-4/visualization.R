library(tidyverse)

dat <- readr::read_csv(here::here("static/data/metro_sp_line_4.csv"))
station <- read_csv(here::here("static/data/metro_sp_line_4_stations.csv"))

sysfonts::font_add("Helvetica", "Helvetica.ttc")
sysfonts::font_add("Helvetica Neue", "HelveticaNeue.ttc")
sysfonts::font_add("Fira Code", "FiraCode-Regular.ttf")
showtext::showtext_auto()


colors <- c("#e6bb3e")

theme_metro <- theme_minimal(base_family = "Helvetica", base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linetype = 2),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 8)
  )

total_passengers <- dat |> 
  filter(variable == "passenger_transported", metric == "Total") |> 
  trendseries::add_trend()

ggplot() +
  geom_line(
    data = total_passengers,
    aes(x = date, y = value),
    color = colors,
    alpha = 0.5) +
  geom_point(
    data = total_passengers,
    aes(x = date, y = value),
    color = colors,
    alpha = 0.5
    ) +
  geom_line(
    data = total_passengers,
    aes(x = date, y = trend_stl),
    color = colors,
    lwd = 1) +
  geom_rect(
    aes(xmin = as.Date("2020-03-16"),
        xmax = as.Date("2023-05-12"),
        ymin = -Inf,
        ymax = Inf),
    fill = "gray70",
    alpha = 0.4) +
  geom_text(
    aes(x = as.Date("2021-10-13"),
        y = 21000,
        label = "Pandemia Covid-19\n(mar/20-mai/23)"),
    family = "Fira Sans",
    size = 3) +
  geom_text(
    aes(x = as.Date("2024-06-01"),
        y = 20500,
        label = "Aumento\nda tarifa\n(4,40 -> 5,00)"),
    family = "Fira Sans",
    size = 3) +
  geom_vline(xintercept = as.Date("2024-01-01"), color = "gray20") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(
    title = "Total de Passageiros (Linha-4, Amarela)",
    subtitle = "Numero total de passageiros transportados na linha-4 Amarela (incluindo baldeações) a cada mês.",
    x = NULL,
    y = "Milhões",
    caption = "Fonte: ViaQuatro. @viniciusoike"
  ) +
  theme_metro

passenger <- dat |> 
  filter(variable == "passenger_transported")

pre_pandemic_baseline <- passenger |> 
  mutate(month = month(date)) |> 
  filter(year < 2020) |> 
  summarise(base_index = mean(value), .by = c("metric", "month"))

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

df1 |> 
  trendseries::add_trend(value_colname = "index_2", trend = "stl")

df1 <- df1 |> 
  group_by(cat) |> 
  nest() |> 
  mutate(trend = map(data, \(x) trendseries::add_trend(x, value_colname = "index_2", trend = "stl"))) |> 
  unnest(trend) |> 
  ungroup()

library(ggtext)

ggplot(df1, aes(date, trend_stl, color = cat)) +
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
    subtitle = "Indexed average passenger flow for <b><span style='color:#2a9d8f'>weekdays</span></b> and <b><span style='color:#e76f51'>workdays</span></b>.",
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

rank_station <- station |> 
  filter(date == max(date)) |> 
  # summarise(total = sum(value), .by = "name_station") |> 
  mutate(
    name_station = factor(name_station),
    name_station = fct_reorder(name_station, value)
  )

ggplot(rank_station, aes(x = name_station, y = value)) +
  geom_col(fill = colors) +
  geom_text(
    aes(y = 2, label = str_wrap(str_remove(name_station, " -"), 11)),
    size = 3,
    color = "black",
    hjust = 0
  ) +
  geom_text(
    aes(y = value + 5, label = round(value)),
    family = "Helvetica",
    size = 3
  ) +
  coord_flip() +
  theme_metro +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
