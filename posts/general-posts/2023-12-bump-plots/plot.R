library(ggplot2)
library(ggbump)
library(dplyr)
library(tidyr)
library(showtext)

font_add_google("Lato", "Lato")
showtext_opts(dpi = 300)
showtext_auto()


dat <- readr::read_csv("static/data/gdp_over_hours_worked_with_estimated_hours_worked.csv")

countries_sel <- c("Norway", "Belgium", "Austria", "United States", "Germany")

measures <- c("gdp_over_pop", "gdp_ppp_over_pop", "gdp_ppp_over_k_hours_worked")

sub <- dat |> 
  select(country, year, all_of(measures)) |> 
  na.omit()

ranking <- sub |> 
  filter(year == max(year)) |> 
  pivot_longer(cols = -c(country, year), names_to = "measure") |> 
  mutate(rank = rank(-value), .by = "measure")

ranking <- ranking |> 
  mutate(
    highlight = if_else(country %in% countries_sel, country, ""),
    highlight = factor(highlight, levels = c(countries_sel, "")),
    is_highlight = factor(if_else(country %in% countries_sel, 1L, 0L)),
    rank_labels = if_else(rank %in% c(1, 5, 10, 15, 20), rank, NA),
    rank_labels = stringr::str_replace(rank_labels, "^1$", "1st"),
    measure = factor(measure, levels = measures)
    )

cores <- c("#101010", "#f7443e", "#8db0cc", "#fa9494", "#225d9f", "#c7c7c7")

df_gdp <- tibble(
  measure = measures,
  measure_label = c(
    "GDP per person at market rates",
    "Adjusted for cost differences*",
    "Adjusted for costs and hours worked"
  ),
  position = -1.5
)

df_gdp <- df_gdp |> 
  mutate(
    measure = factor(measure, levels = measures),
    measure_label = stringr::str_wrap(measure_label, width = 12),
    measure_label = paste0("  ", measure_label)
    )

p <- ggplot(ranking, aes(measure, rank, group = country)) +
  geom_bump(aes(color = highlight, linewidth = is_highlight)) +
  geom_point(shape = 21, color = "white", aes(fill = highlight), size = 3) +
  geom_text(
    data = filter(ranking, measure == measures[[3]], is_highlight != 1L),
    aes(x = measure, y = rank, label = country),
    nudge_x = 0.05,
    hjust = 0,
    family = "Lato",
    size = 3
  ) +
  geom_text(
    data = filter(ranking, measure == measures[[3]], is_highlight == 1L),
    aes(x = measure, y = rank, label = country),
    nudge_x = 0.05,
    hjust = 0,
    family = "Lato",
    fontface = "bold",
    size = 3
  ) +
  geom_text(
    data = filter(ranking, measure == measures[[1]]),
    aes(x = measure, y = rank, label = rank_labels),
    nudge_x = -0.15,
    hjust = 0,
    family = "Lato",
    size = 3
  ) +
  geom_text(
    data = df_gdp,
    aes(x = measure, y = position, label = measure_label),
    inherit.aes = FALSE,
    hjust = 0,
    family = "Lato",
    fontface = "bold",
    size = 3
  ) +
  annotate("text", x = 1, y = -3, label = expression("\u2193")) +
  annotate("text", x = 2, y = -3, label = expression("\u2193")) +
  annotate("text", x = 3, y = -3, label = expression("\u2193")) +
  coord_cartesian(ylim = c(21, -2)) +
  scale_color_manual(values = cores) +
  scale_fill_manual(values = cores) +
  scale_linewidth_manual(values = c(0.5, 1.2)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_blank()
  )

cowplot::save_plot(here::here("static/images/replication_economist_bump_plot.jpeg"), p)
