library(ggplot2)
library(lubridate)
sysfonts::font_add("Helvetica", "Helvetica.ttc")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()

ipca <- GetBCBData::gbcbd_get_series(433, first.date = as.Date("1980-01-01"))

ipca <- ipca |> 
  dplyr::rename(date = ref.date)

df <- tidyr::tribble(
  ~date, ~event, ~date_label,
  "1980-01-01", "Delfim + Oil Crisis", "DEC 1979",
  "1983-02-01", "Delfim (2nd)", "FEB 1983",
  "1985-05-01", "Military Regime Ends", "MAR 1985",
  "1986-04-01", "Cruzado Plan", "FEB 1986",
  "1987-08-01", "Bresser Plan", "JUN 1987",
  "1989-03-01", "Verão Plan", "JAN 1989",
  "1990-05-01", "Collor Plan", "MAR 1990",
  "1991-04-01", "Collor Plan II", "JAN 1991",
  "1993-01-01", "Collor Impeachment", "DEC 1992",
  "1994-08-01", "Real Plan", "JUL 1994",
  "1999-01-01", "Flotaing Exchange Rate", "JAN '99",
  "1999-08-01", "Inflation Targeting", "JUN '99",
  "2000-07-01", "New Fiscal Regime", "MAY '00",
  "2002-11-01", "Argentina + Lula Confidence Crisis", "2002",
  "2008-04-01", "Brazil gains Investment Grade", NA,
  "2008-07-01", "Great Financial Recession", "2008",
  "2015-09-01", "Brazil loses Investment Grade", "2015",
  "2016-08-01", "Dilma Impeachment", "AUG 2016",
  "2020-03-01", "Covid-19", "MAR 2020",
  "2021-03-01", "Central Bank Independence", "FEB 2021"
)

df <- df |> 
  dplyr::mutate(
    date = as.Date(date),
    event_label = stringr::str_wrap(event, width = 9)
  )

events <- dplyr::left_join(df, ipca, by = "date")

events <- events |> 
  dplyr::mutate(
    yseg = c(-8, -9.5, 25, -9.5, 45, -9.5, -9.5, 55, -9.5, 65, 10, -9.5, 15, -9.5, -8, 10, 10, -9.5, -10, 10),
    x = stringr::str_count(event_label, "\\\n"),
    vjust = 1.5 + 1.5 * x,
    ytext = ifelse(yseg < 0, yseg - vjust, yseg + vjust)
  )

acum <- ipca |> 
  dplyr::mutate(
    segment = ifelse(date < as.Date("1995-01-01"), "Pre-Real", "Post-Real")
    ) |> 
  dplyr::summarise(
    acum = (prod(1 + value / 100) - 1) * 100,
    start = min(date),
    end = max(date),
    .by = "segment"
  ) |> 
  dplyr::mutate(
    acum_label = format(
      round(acum, 2),
      big.mark = ",",
      decimal.mark = ".",
      scientific = FALSE
      ),
    acum_label = stringr::str_glue("Total Inflation = {acum_label}%"),
    acum_label = stringr::str_replace_all(acum_label, "    ", " "),
    acum_label = stringr::str_replace(acum_label, "    ", " "),
    midpoint = start + days(floor((end - start) / 2))
    )

presidents <- tidyr::tribble(
  ~start,         ~end, ~name,
  "1979-12-01", "1985-02-01", "Figueiredo",
  "1985-03-01", "1989-12-01", "Sarney",
  "1990-01-01", "1992-12-01", "Collor",
  "1993-01-01", "1994-12-01", "Itamar Franco",
  "1995-01-01", "2001-12-01", "FHC",
  "2002-01-01", "2008-12-01", "Lula",
  "2009-01-01", "2016-06-01", "Dilma",
  "2016-07-01", "2017-12-01", "Temer",
  "2018-01-01", "2022-12-01", "Bolsonaro",
  "2023-01-01", "2023-12-01", "Lula"
)

presidents <- presidents |> 
  dplyr::mutate(
    start = as.Date(start),
    end = as.Date(end),
    x_label = dplyr::if_else(name == "Collor", start + months(6), start + months(3))
  )

# ggplot() +
#   geom_segment(
#     data = acum,
#     aes(x = start, xend = end, y = 85, yend = 85, group = segment),
#     arrow = arrow(length = unit(0.3, "cm"), ends = "both"),
#     ) +
#   geom_text(
#     data = acum,
#     aes(x = midpoint, y = 88, label = acum_label),
#     family = "Helvetica",
#     fontface = "bold",
#     color = "#de2d26"
#   ) +
#   scale_y_continuous(limits = c(0, 90))

plot_inflation <- ggplot() +
  geom_rect(
    data = presidents,
    aes(xmin = start, xmax = end, ymin = 0, ymax = 82, fill = name),
    alpha = 0.5
  ) +
  geom_area(
    data = ipca,
    aes(x = date, y = value),
    fill = "gray30") +
  geom_segment(
    data = events,
    aes(x = date, xend = date, y = yseg, yend = value),
    color = "#de2d26"
  ) +
  geom_segment(
    data = events,
    aes(x = date, xend = date + months(12), y = yseg, yend = yseg),
    color = "#de2d26"
  ) +
  geom_point(
    data = events,
    aes(x = date, y = value),
    shape = 21,
    fill = "#de2d26",
    color = "gray90"
  ) +
  geom_label(
    data = events,
    aes(x = date, y = -3.5, label = date_label),
    label.r = unit(0, "pt"),
    label.padding = unit(0.1, "line"),
    nudge_y = c(0, 0, 0, -3, 0, -3, 0, -3, 0, -3, 0, -3, 0, 0, 0, 0, -3, 0, -3, 0),
    size = 3,
    fill = "black",
    color = "white"
  ) +
  geom_text(
    data = events,
    aes(x = date, y = ytext, label = stringr::str_wrap(event, 9)),
    size = 3,
    hjust = 0,
    lineheight = .8,
    family = "Helvetica"
  ) +
  geom_segment(
    data = acum,
    aes(x = start, xend = end, y = 85, yend = 85, group = segment),
    arrow = arrow(length = unit(0.3, "cm"), ends = "both"),
  ) +
  geom_text(
    data = acum,
    aes(x = midpoint, y = 88, label = acum_label),
    family = "Helvetica",
    fontface = "bold",
    color = "#de2d26"
  ) +
  geom_text(
    data = presidents,
    aes(x = x_label, y = 40.5, label = name),
    angle = 90,
    hjust = 0,
    family = "Helvetica",
    color = "gray20"
  ) +
  geom_hline(yintercept = 0) +
  scale_x_date(
    breaks = seq(as.Date("1980-06-01"), as.Date("2020-06-01"), by = "5 year"),
    date_labels = "%Y"
    ) +
  scale_y_continuous(breaks = seq(0, 80, 20)) +
  scale_fill_manual(values = MetBrewer::met.brewer("VanGogh1", n = 9)) +
  guides(fill = "none") +
  labs(
    title = "The Story of Brazilian Inflation",
    subtitle = "Monthly percent variation in Consumer Price Index",
    caption = "Source: IBGE. @viniciusoike",
    x = NULL,
    y = "IPCA\n(MoM, %)") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 2),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, angle = 0, vjust = 1)
  )

ggsave(
  here::here("static/images/inflation_historic.png"),
  plot_inflation,
  width = 16,
  height = 6.2
  )

ggsave(
  here::here("static/images/inflation_historic.svg"),
  plot_inflation,
  width = 16,
  height = 6.2
)

subipca <- ipca |> 
  dplyr::mutate(acum = (RcppRoll::roll_prodr(1 + value / 100, n = 12) - 1) * 100) |> 
  dplyr::filter(date >= as.Date("1995-07-01"))

df <- tidyr::tribble(
  ~date,                          ~event,        ~date_label,    ~yseg,
  "1997-08-01", "Asian Financial Crisis",             "1997", -3,
  "1998-08-01", "Russian Crisis",                     "1998",  7,
  "1999-01-01", "Flotaing Exchange Rate",             "JAN '99", -3,
  "1999-08-01", "Inflation Targeting",                "JUN '99", 14,
  "2000-07-01", "New Fiscal Regime",                  "MAY '00", -3,
  "2002-11-01", "Argentina + Lula Confidence Crisis", "2002", -3,
  "2008-04-01", "Brazil gains Investment Grade",      "SEP 2008", 13,
  "2008-07-01", "Great Financial Recession",          "2008", -3,
  "2013-06-01", "Riots and Protests of 2013",         "JUN 2013", 13,
  "2014-12-01", "Administered Prices Adjust",         "JAN 2015", -3,
  "2015-09-01", "Brazil loses Investment Grade",      "SEP 2015", 21,
  "2016-08-01", "Dilma Impeachment",                  "AUG 2016", -3,
  "2016-12-01", "Expenditure Ceiling",                "DEC 2016", 13,
  "2019-10-01", "Pensions Reform",                    "OCT 2019", -3,
  "2020-03-01", "Covid-19",                           "MAR 2020", 13,
  "2021-03-01", "Central Bank Independence",          "FEB 2021", -3
)

df <- df |> 
  dplyr::mutate(
    date = as.Date(date),
    event_label = stringr::str_wrap(event, width = 9)
  ) |> 
  dplyr::filter(date >= as.Date("1995-01-01"))

events <- dplyr::left_join(df, subipca, by = "date")

right_event <- c(
  "Russian Crisis",
  "Brazil gains Investment Grade",
  "Brazil loses Investment Grade"
  )

events <- events |> 
  dplyr::mutate(
    #yseg = c(-4, 7, -4, 14, -4, -3, -3, 12, 13, -4, 8, -4, 8, -4, -4, -4),
    x_event_label = dplyr::if_else(event %in% right_event, date - months(12), date),
    y_date_label = dplyr::if_else(yseg > 0, yseg - 1.5, -1.5),
    x = stringr::str_count(event_label, "\\\n"),
    vjust = 0.75 + 0.5 * x,
    ytext = ifelse(yseg < 0, yseg - vjust, yseg + vjust),
    segment_end = dplyr::if_else(event %in% right_event, date - months(12), date + months(12))
  )

plot_inflation_recent <- ggplot() +
  geom_hline(yintercept = 0) +
  # Shaded area
  geom_rect(
    data = dplyr::filter(presidents, start >= as.Date("1995-01-01")),
    aes(xmin = start, xmax = end, ymin = 0, ymax = 30, fill = name),
    alpha = 0.5
  ) +
  # Inflation
  geom_area(
    data = subipca,
    aes(x = date, y = acum),
    fill = "gray30") +
  # Vertical red line
  geom_segment(
    data = dplyr::filter(events, date >= as.Date("1995-01-01")),
    aes(x = date, xend = date, y = yseg, yend = acum),
    color = "#de2d26"
  ) +
  # Horizontal red line
  geom_segment(
    data = dplyr::filter(events, date >= as.Date("1995-01-01")),
    aes(x = date, xend = segment_end, y = yseg, yend = yseg),
    color = "#de2d26"
  ) +
  # Points
  geom_point(
    data = dplyr::filter(events, date >= as.Date("1995-07-01")),
    aes(x = date, y = acum),
    shape = 21,
    fill = "#de2d26",
    color = "gray90"
  ) +
  # Events dates labels
  geom_label(
    data = dplyr::filter(events, date >= as.Date("1995-07-01")),
    aes(x = date, y = y_date_label, label = date_label),
    label.r = unit(0, "pt"),
    label.padding = unit(0.1, "line"),
    #nudge_y = c(0, 6.5, 0, 13, 0, 0, 0, 0, 0, -1.5, 0, -1.5, 0, 0),
    size = 3,
    fill = "black",
    color = "white"
  ) +
  # Events labels
  geom_text(
    data = dplyr::filter(events, date >= as.Date("1995-01-01")),
    aes(x = x_event_label, y = ytext, label = stringr::str_wrap(event, 11)),
    size = 3,
    hjust = 0,
    lineheight = .8,
    family = "Helvetica"
  ) +
  geom_text(
    data = dplyr::filter(presidents, start >= as.Date("1995-01-01")),
    aes(x = x_label, y = 20.5, label = name),
    angle = 90,
    hjust = 0,
    family = "Helvetica",
    color = "gray20"
  ) +
  scale_x_date(
    breaks = seq(as.Date("1980-06-01"), as.Date("2020-06-01"), by = "5 year"),
    date_labels = "%Y",
    expand = c(0.0, 0.1)
  ) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  scale_fill_manual(values = MetBrewer::met.brewer("VanGogh1", n = 6)) +
  guides(fill = "none") +
  labs(
    title = "The Story of Brazilian Inflation",
    subtitle = "Year on year percent variation in Consumer Price Index since the end of hyperinflation.",
    caption = "Source: IBGE. @viniciusoike",
    x = NULL,
    y = "IPCA\n(YoY, %)") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 2),
    plot.title = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, angle = 0, vjust = 1)
  )

plot_inflation_recent

ggsave(
  here::here("static/images/inflation_historic_post_real.png"),
  plot_inflation_recent,
  width = 16,
  height = 6.2
)

ggsave(
  here::here("static/images/inflation_historic_post_real.svg"),
  plot_inflation_recent,
  width = 16,
  height = 6.2
)

igpdi <- GetBCBData::gbcbd_get_series(190, first.date = as.Date("1944-01-01"))

igpdi <- igpdi |> 
  dplyr::mutate(acum12m = (RcppRoll::roll_prodr(1 + value / 100, n = 12) - 1) * 100)

igpdi |> 
  dplyr::filter(ref.date < as.Date("1980-01-01"), !is.na(acum12m)) |> 
  ggplot(aes(x = ref.date, y = acum12m)) +
  geom_line(lwd = 1) +
  geom_hline(yintercept = 0) +
  scale_x_date(
    breaks = seq(from = as.Date("1945-01-01"), to = as.Date("1980-01-01"), by = "5 year"),
    date_label = "%Y") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  labs(
    title = "Brazil endured persistent high inflation throughout most of its history.",
    subtitle = "Year on year inflation (overall prices).",
    caption = "Source: IGP-DI (FGV)",
    x = NULL,
    y = "%") +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank()
  )

ggplot(igpdi, aes(x = ref.date, y = acum12m)) +
  geom_line()
