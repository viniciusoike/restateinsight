library(ggplot2)
library(dplyr)
library(showtext)
library(cowplot)
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_opts(dpi = 300)
showtext_auto()

cities_age <- readr::read_rds(
  here::here("static/data/census_aging_index_city.rds")
)

breaks <- c(0.25, 0.5, 0.75, 1) * 100
labels <- c(
  "Até 25", "25 a 50", "50 a 75", "75 a 100", "100 ou mais"
)

age_to_color <- Vectorize(function(x) {
  
  breaks <- c(0.25, 0.5, 0.75, 1) * 100
  y <- findInterval(x, breaks)
  
  colors <- c(
    "#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF"
  )
  
  colors[y + 1]
  
})

map_census <- function(year) {
  
  col_name <- glue::glue("age_index_{year}")
  
  sub <- cities_age %>%
    mutate(
      color = age_to_color(.data[[col_name]]),
      color = if_else(is.na(color), "gray50", color)
    )
  
  ggplot(sub) +
    # geom_sf(aes(fill = color), lwd = 0.01, color = "gray80") +
    geom_sf(aes(fill = color, color = color)) +
    scale_fill_identity() +
    scale_color_identity() +
    coord_sf(xlim = c(NA, -34.469802)) +
    ggtitle(year) +
    ggthemes::theme_map(base_family = "Roboto Condensed") +
    theme(plot.title = element_text(hjust = 0.5, size = 18))
  
}

m1 <- map_census(1991)
m2 <- map_census(2000)
m3 <- map_census(2010)
m4 <- map_census(2022)

breaks <- c(0.25, 0.5, 0.75, 1) * 100
labels <- c("Até 25", "25 a 50", "50 a 75", "75 a 100", "100 ou mais")

map_legend <- cities_age |> 
  mutate(
    group = factor(findInterval(age_index_2022, breaks))
  ) |> 
  ggplot(aes(fill = group)) +
  geom_sf(color = "gray80") +
  scale_fill_viridis_d(labels = labels, name = "Índice de\nEnvelhecimento")

fill_legend <- cowplot::get_legend(map_legend)
plot_maps <- cowplot::plot_grid(m1, m2, m3, m4, nrow = 2, ncol = 2)
plot_compare <- cowplot::plot_grid(plot_maps, fill_legend, ncol = 2, rel_widths = c(0.9, 0.1))

ggsave(here::here("static/images/census_age_index_map.png"), plot_compare, width = 11, height = 11)

map2022 <- cities_age |> 
  mutate(group = factor(findInterval(age_index_2022, breaks))) |> 
  ggplot() +
  geom_sf(aes(fill = group), lwd = 0.01, color = "gray80") +
  scale_fill_viridis_d(
    name = "Índice de\nEnvelhecimento",
    labels = labels
  ) +
  coord_sf(xlim = c(NA, -34.469802)) +
  ggtitle("Índice de Envelhecimento (2022)") +
  ggthemes::theme_map(base_family = "Roboto Condensed") +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

ggsave(here::here("static/images/census_age_index_map_2022.png"), map2022, width = 9, height = 9)