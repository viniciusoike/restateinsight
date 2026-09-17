# Plot helpers for the circle maps ----
# Data comes from static/data-raw/R/circles_cities.R.

library(sf)
library(dplyr)
library(ggplot2)

font <- "Futura"
offwhite <- "#F4F0E0"

colors_features <- c(
  sea = "#1F5680",
  water = "#1F5680",
  green = "#7A9E6B",
  parking = "#2F3737"
)

density_breaks <- c(0, 2500, 5000, 10000, 20000, 40000, Inf)
density_labels <- c("< 2,5", "2,5–5", "5–10", "10–20", "20–40", "> 40")
density_colors <- viridis::inferno(8)[2:7]

street_widths <- c(
  motorway = 0.5,
  trunk = 0.5,
  primary = 0.35,
  secondary = 0.3,
  tertiary = 0.25,
  residential = 0.2
)

# Functions -----------------------------------------------------------------

classify_streets <- function(streets) {
  out <- streets |>
    mutate(
      density_group = cut(
        density,
        breaks = density_breaks,
        labels = density_labels,
        right = FALSE
      ),
      linewidth = street_widths[highway]
    ) |>
    arrange(density)

  return(out)
}

format_number <- function(x, digits = 0) {
  out <- format(
    round(x, digits),
    big.mark = ".",
    decimal.mark = ",",
    nsmall = digits,
    trim = TRUE
  )

  return(out)
}

make_subtitle <- function(summary) {
  share <- summary$pop_circle / summary$pop_urban * 100

  out <- stringr::str_glue(
    "{format_number(summary$pop_circle / 1e3)} mil pessoas no círculo\n",
    "{format_number(share, 1)}% da concentração urbana"
  )

  return(out)
}

plot_circle <- function(
  city,
  title_size = 22,
  subtitle = TRUE,
  legend = FALSE
) {
  streets <- classify_streets(city$streets)
  streets_pop <- dplyr::filter(streets, density > 0)
  streets_empty <- dplyr::filter(streets, density == 0)

  bbox <- st_bbox(city$circle)
  arrow_y <- bbox[["ymin"]] - 400

  df_arrow <- data.frame(
    x = bbox[["xmin"]],
    xend = bbox[["xmax"]],
    y = arrow_y,
    label_y = arrow_y - 500
  )

  p <- ggplot() +
    geom_sf(data = city$sea, fill = colors_features[["sea"]], color = NA) +
    geom_sf(
      data = city$green,
      fill = colors_features[["green"]],
      color = NA
    ) +
    geom_sf(
      data = city$water,
      fill = colors_features[["water"]],
      color = NA
    ) +
    geom_sf(
      data = city$parking,
      fill = colors_features[["parking"]],
      color = NA
    ) +
    geom_sf(
      data = streets_empty,
      aes(linewidth = linewidth),
      color = "gray20"
    ) +
    geom_sf(
      data = streets_pop,
      aes(linewidth = linewidth, color = density_group)
    ) +
    geom_sf(data = city$circle, fill = NA, color = "gray10", linewidth = 1) +
    geom_segment(
      data = df_arrow,
      aes(x = x, xend = xend, y = y, yend = y),
      arrow = grid::arrow(ends = "both", length = unit(4, "pt"))
    ) +
    geom_text(
      data = df_arrow,
      aes(x = (x + xend) / 2, y = label_y, label = "12 km"),
      family = font,
      size = 3.5
    ) +
    scale_linewidth_identity() +
    scale_color_manual(
      name = "Habitantes por km² (mil)",
      values = density_colors,
      drop = FALSE
    ) +
    coord_sf(crs = st_crs(city$circle), datum = NA) +
    labs(
      title = city$summary$name_city,
      subtitle = if (subtitle) make_subtitle(city$summary)
    ) +
    theme_void(base_family = font) +
    theme(
      plot.title = element_text(size = title_size, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
      plot.background = element_rect(color = NA, fill = offwhite),
      panel.background = element_rect(color = NA, fill = offwhite),
      plot.margin = margin(10, 10, 10, 10)
    )

  if (legend) {
    p <- p +
      guides(
        color = guide_legend(
          nrow = 1,
          title.position = "top",
          label.position = "bottom",
          override.aes = list(linewidth = 3)
        )
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.key.width = unit(1.2, "cm")
      )
  } else {
    p <- p + guides(color = "none")
  }

  return(p)
}

plot_legend <- function() {
  df <- data.frame(
    x = seq_along(density_labels),
    group = factor(density_labels, levels = density_labels)
  )

  p <- ggplot(df, aes(x = x, y = 1, fill = group)) +
    geom_tile(height = 0.25, width = 0.9) +
    geom_text(aes(y = 0.7, label = group), family = font, size = 3.5) +
    scale_fill_manual(values = density_colors) +
    scale_y_continuous(limits = c(0.55, 1.35)) +
    guides(fill = "none") +
    labs(title = "Habitantes por km² (mil), no setor censitário de cada rua") +
    theme_void(base_family = font) +
    theme(
      plot.title = element_text(size = 11, hjust = 0.5),
      plot.background = element_rect(color = NA, fill = offwhite)
    )

  return(p)
}

plot_ranking <- function(summary_cities) {
  dat <- summary_cities |>
    mutate(
      pop_circle = pop_circle / 1e3,
      density_land = pop_circle / land_km2,
      name_city = forcats::fct_reorder(name_city, pop_circle)
    )

  theme_bars <- theme_minimal(base_family = font, base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 12),
      plot.background = element_rect(color = NA, fill = offwhite),
      axis.title = element_blank()
    )

  p_pop <- ggplot(dat, aes(x = pop_circle, y = name_city)) +
    geom_col(fill = density_colors[4], width = 0.7) +
    geom_text(
      aes(label = format_number(pop_circle)),
      hjust = -0.15,
      family = font,
      size = 3.2
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Pessoas no círculo (mil)") +
    theme_bars +
    theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())

  p_density <- ggplot(dat, aes(x = density_land, y = name_city)) +
    geom_col(fill = density_colors[2], width = 0.7) +
    geom_text(
      aes(label = format_number(density_land, 1)),
      hjust = -0.15,
      family = font,
      size = 3.2
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Habitantes por km² de terra (mil)") +
    theme_bars +
    theme(
      axis.text = element_blank(),
      panel.grid.major.x = element_blank()
    )

  p <- patchwork::wrap_plots(p_pop, p_density, nrow = 1) +
    patchwork::plot_annotation(
      theme = theme(plot.background = element_rect(color = NA, fill = offwhite))
    )

  return(p)
}
