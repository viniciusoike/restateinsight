library(rvest)
library(ggplot2)
library(stringr)
library(dplyr)
library(showtext)

# https://help.imdb.com/article/imdb/track-movies-tv/ratings-faq/G67Y87TFYYP6TWAV?showReportContentLink=false&recentlyAuthenticated=true#
wr = function(R, v, m = 25000, C) {
  (v / (v + m)) * R + (m / (v + m)) * C
}

# Tipografia --------------------------------------------------------------

# Verifica a fonte do texto

check_fonts_nexo <- function() {
  
  dbfonts <- sysfonts::font_files()
  
  nexo_fonts <- paste("Gotham Rounded", c("Bold", "Medium", "Light"))
  nexo_fonts_path <- paste0(nexo_fonts, ".otf")
  
  cond <- stringr::str_glue(
    "({nexo_fonts[1]})|({nexo_fonts[2]})|({nexo_fonts[3]})"
  )
  
  check_fonts <- sum(stringr::str_detect(dbfonts$family, cond))
  check_fonts <- ifelse(check_fonts == 3, TRUE, FALSE)
  
  if (!check_fonts) {
    
    fonts_found <- dbfonts$family[stringr::str_detect(dbfonts$family, cond)]
    
    message(glue::glue(
      "Missing fonts. Only found: {paste(fonts_found, collapse = ', ')}"
    ))
  }
  
  return(check_fonts)
  
}

load_fonts_nexo <- function(dpi = 96, ...) {

  check_fonts <- check_fonts_nexo()
  
  if (check_fonts) {
    # Adiciona as fonts Gotham Rounded Bold e Light
    sysfonts::font_add("Gotham Rounded Bold", "Gotham Rounded Bold.otf")
    sysfonts::font_add("Gotham Rounded Medium", "Gotham Rounded Medium.otf")
    sysfonts::font_add("Gotham Rounded Light", "Gotham Rounded Light.otf")
  } else {
    # Adiciona Montserrat caso as fontes Gotham nao estejam disponiveis
    sysfonts::font_add_google("Montserrat", "Montserrat")
  }
  
  sysfonts::font_add_google("Crimson Pro", "Crimson Text")
  
  if (dpi > 0) {
    showtext::showtext_opts(dpi = dpi, ...)
  }
  
  showtext::showtext_auto()
  
  if (check_fonts) {
    message("Gotham Rounded fonts successfully loaded.")
  } else {
    message("Gotham Rounded font not found. Montserrat was loaded instead.")
  }
  
}

load_fonts_nexo()

font <- ifelse(check_fonts_nexo(), "Gotham Rounded Bold", "Montserrat")
font_axis <- ifelse(check_fonts_nexo(), "Gotham Rounded Light", "Montserrat")
font_title <- "Crimson Text"


# Data --------------------------------------------------------------------

url <- "https://250.took.nl/compare/full"

page <- read_html(url)

page_tables <- html_table(page)

tab <- page_tables[[9]]

imdb <- tab |> 
  janitor::clean_names() |> 
  select(rank_imdb = rank, rating, title, votes) |> 
  mutate(
    year = as.numeric(str_extract(title, "(?<=\\()[0-9]{1,4}(?=\\))")),
    votes = as.numeric(str_remove_all(votes, ",")),
    wr = wr(rating, votes, C = mean(rating)),
    rank = rank(-wr)
    )

imdb <- imdb |>  
  mutate(
    decade = floor(year / 10) * 10,
    trunc_rating = round(wr, 1),
    trunc_decade = case_when(
      decade < 1950 ~ 1940,
      decade > 2020 ~ 2020,
      TRUE ~ decade
    ),
    #trunc_decade = factor(trunc_decade),
    #trunc_decade = forcats::fct_reorder(trunc_decade, decade),
    is_top20 = factor(if_else(rank <= 20, 1L, 0L))
  )

# Plot --------------------------------------------------------------------

# imdb <- qs::qread("static/data/greatest_movies.qs")

nexo_labels <- c("até\n1950", "1960", "70", "80", "90", "00", "10", "20", "até\nhoje")

colors <- c("#328bff", "#88bce4")

plot_base <- ggplot(imdb, aes(trunc_decade, trunc_rating)) +
  geom_count(aes(color = is_top20)) +
  geom_hline(yintercept = 7.9) +
  scale_x_continuous(breaks = seq(1940, 2020, 10), labels = nexo_labels) +
  scale_y_continuous(
    limits = c(7.9, 9.45),
    breaks = seq(7.9, 9.3, 0.1),
    labels = scales::label_number(decimal.mark = ",", accuracy = 0.1),
    expand = c(0, 0)
  ) +
  scale_color_manual(values = rev(colors)) +
  scale_size_area(name = "", breaks = c(4, 8, 12, 16)) +
  guides(
    color = "none",
    size = guide_legend(
      label.position = "bottom",
      override.aes = list(color = "gray80"))
  )

plot_annotations <- plot_base +
  labs(
    title = "Notas dos 250 filmes melhor\navaliados por década de estreia",
    subtitle = "SEGUNDO AVALIAÇÕES DO IMDB ATÉ DEZ. DE 2024\n\n\n\n",
    x = NULL,
    y = NULL
  ) +
  annotate(
    "label",
    x = 1970,
    y = 9.31,
    label = "entre os melhores\n20 filmes",
    family = font,
    size = 3,
    color = colors[1],
    hjust = 0.5,
    label.size = 0
  ) +
  annotate(
    "label",
    x = 2015,
    y = 9.15,
    label = "entre os\nmelhores\n250 filmes",
    family = font,
    size = 3,
    color = colors[2],
    hjust = 0.5,
    label.size = 0
  )

plot_arrows <- plot_annotations +
  geom_curve(
    data = data.frame(x = 1990, xend = 1978, y = 9.31, yend = 9.4),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  geom_curve(
    data = data.frame(x = 2020, xend = 2015, y = 8.6, yend = 9),
    aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.45,
    arrow = arrow(length = unit(0.03, "npc"))
  )

plot_full <- plot_arrows +
  theme_minimal(base_family = font, base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linetype = 3, color = "#d9d9d9", linewidth = 0.35),
    panel.grid.major.x = element_line(color = "#838484", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    legend.position.inside = c(0.09, 1.1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    plot.title = element_text(family = font_title, size = 22, hjust = 0),
    
    axis.title.y = element_text(color = "#767676"),
    axis.ticks.x = element_line(color = "#000000")
  )

plot_full


# My version --------------------------------------------------------------

font_add_google("Montserrat", "Montserrat")
showtext_auto()

font <- "Montserrat"
font_axis <- "Montserrat"

x_labels <- c("Until\n1940's", "50's", "60's", "70's", "80's", "90's", "2000's",
              "10's", "20's")

colors <- c("#328bff", "#88bce4")

plot_base <- ggplot(imdb, aes(trunc_decade, trunc_rating)) +
  geom_count(aes(color = is_top20)) +
  geom_hline(yintercept = 7.9) +
  scale_x_continuous(breaks = seq(1940, 2020, 10), labels = x_labels) +
  scale_y_continuous(
    limits = c(7.9, 9.45),
    breaks = seq(7.9, 9.3, 0.1),
    expand = c(0, 0)
  ) +
  scale_color_manual(values = rev(colors)) +
  scale_size_area(name = "", breaks = c(4, 8, 12, 16)) +
  guides(
    color = "none",
    size = guide_legend(
      label.position = "bottom",
      override.aes = list(color = "gray80"))
  )

plot_annotations <- plot_base +
  labs(
    title = "Recency bias in the IMDB Top 250",
    subtitle = "Ratings of the 250 top-Rated movies by decade of release according to IMDb ratings up to December 2024.\n\n\n\n",
    x = NULL,
    y = NULL
  ) +
  annotate(
    "label",
    x = 1975,
    y = 9.31,
    label = "Top 20 films",
    family = font,
    size = 3,
    color = colors[1],
    hjust = 0.5,
    label.size = 0
  ) +
  annotate(
    "label",
    x = 2015,
    y = 8.9,
    label = "Top 250 films",
    family = font,
    size = 3,
    color = colors[2],
    hjust = 0.5,
    label.size = 0
  )

plot_arrows <- plot_annotations +
  geom_curve(
    data = data.frame(x = 1990, xend = 1982, y = 9.31, yend = 9.32),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = -0.45
  ) +
  geom_curve(
    data = data.frame(x = 2020, xend = 2015, y = 8.6, yend = 8.85),
    aes(x = x, xend = xend, y = y, yend = yend),
    curvature = -0.45,
    arrow = arrow(length = unit(0.03, "npc"))
  )

plot_full <- plot_arrows +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    panel.grid.major.y = element_line(linetype = 3, color = "#d9d9d9", linewidth = 0.35),
    panel.grid.major.x = element_line(color = "#838484", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    legend.position = c(0.09, 1.1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    plot.title = element_text(family = font_title, size = 22, hjust = 0),
    plot.caption = element_text(family = font_axis, size = 8, hjust = 1),
    
    axis.title.y = element_text(color = "#767676"),
    axis.ticks.x = element_line(color = "#000000")
  )

p1 <- plot_full

tab_imdb <- imdb |> 
  count(year) |> 
  arrange(year) |> 
  mutate(acum = cumsum(n) / sum(n))

imdb_series <- tibble(year = seq(min(tab_imdb$year), max(tab_imdb$year), 1))
  
imdb_series <- imdb_series |> 
  left_join(tab_imdb, by = "year") |> 
  mutate(imput_acum = zoo::na.locf(acum))

p2 <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = 2, color = "gray30") +
  geom_step(
    data = imdb_series,
    aes(x = year, y = imput_acum)
  ) +
  geom_hline(yintercept = 0) +
  geom_point(
    data = filter(imdb_series, year %% 10 == 0),
    aes(x = year, y = imput_acum),
    shape = 21,
    fill = colors[1],
    color = "#000000",
    size = 2
  ) +
  geom_label(
    data = filter(imdb_series, year %% 10 == 0),
    aes(x = year, y = imput_acum + 0.04, label = round(imput_acum * 100, 1)),
    color = "#000000",
    size = 3,
    family = "Montserrat"
  ) +
  annotate(
    "label",
    x = 1980,
    y = 0.72,
    size = 3,
    label = stringr::str_wrap(
      "Over half of the top 250 films were released after 1995",
      21),
    label.size = 0,
    family = "Montserrat"
  ) +
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = seq(0, 1, 0.2) * 100,
    limits = c(NA, 1.05),
    expand = c(0, 0)) +
  labs(
    subtitle = "Accumulated share of the top 250 movies by year of release.",
    x = NULL,
    y = "(%)"
    ) +
  theme_minimal(base_family = "Montserrat", base_size = 10) +
  theme(
    panel.grid.minor = element_blank()
  )

imdb_decade <- imdb |> 
  count(decade)

x_labels <- c(
  "1920's", "30's", "40's", "50's", "60's", "70's", "80's", "90's", "2000's",
  "10's", "20's"
  )

p3 <- ggplot(imdb_decade, aes(x = decade, y = n)) +
  geom_col(fill = colors[2]) +
  geom_hline(yintercept = 0) +
  geom_text(aes(y = n + 2, label = n), family = "Montserrat") +
  scale_x_continuous(breaks = seq(1920, 2020, 10), labels = x_labels) +
  labs(
    subtitle = "Top 250 films by decade",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "Montserrat", base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank()
  )

library(patchwork)

p1 / (p2 | p3)

caption = "Source: IMDB. Movies are ranked by their weighted rating.\n@viniciusoike",

ggplot(imdb, aes(trunc_decade, trunc_rating)) +
  geom_count(aes(color = is_top20)) +
  geom_hline(yintercept = 7.9) +
  scale_x_continuous(breaks = seq(1940, 2020, 10), labels = nexo_labels) +
  scale_y_continuous(
    limits = c(7.9, 9.4),
    breaks = seq(7.9, 9.3, 0.1),
    labels = scales::label_number(decimal.mark = ",", accuracy = 0.1),
    expand = c(0, 0)
    ) +
  scale_color_manual(values = colors) +
  scale_size_area(name = "", breaks = c(4, 8, 12, 16)) +
  guides(
    color = "none",
    size = guide_legend(
      label.position = "bottom",
      override.aes = list(color = "gray80"))
    ) +
  labs(
    title = "Ratings of the 250 Top-Rated Movies by Decade of Release",
    subtitle = "According to IMDb ratings up to September 2024\n\n\n\n",
    x = NULL,
    y = NULL
  ) +
  annotate(
    "label",
    x = 1970,
    y = 9.31,
    label = "entre os melhores\n20 filmes",
    family = font,
    size = 3,
    color = colors[1],
    hjust = 0.5,
    label.size = 0
    ) +
  annotate(
    "label",
    x = 2015,
    y = 9.2,
    label = "entre os\nmelhores\n250 filmes",
    family = font,
    size = 3,
    color = colors[2],
    hjust = 0.5,
    label.size = 0
  ) +
  theme_minimal(base_family = font, base_size = 14) +
  theme(
    panel.grid.major.y = element_line(linetype = 3, color = "#d9d9d9", linewidth = 0.35),
    panel.grid.major.x = element_line(color = "#838484", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    legend.position = c(0.09, 1.1),
    legend.direction = "horizontal",
    legend.text = element_text(size = 10),
    plot.subtitle = element_text(size = 10),
    plot.title = element_text(family = font_title, size = 22),
    
    axis.title.y = element_text(color = "#626262"),
    axis.ticks.x = element_line(color = "#000000")
    )
  

imdb <- imdb |> 
  mutate(trunc_note = round(rating, 1))

imdb |> 
  mutate(
    trunc_decade = case_when(
      decade <= 1950 ~ 1950,
      decade > 2020 ~ 2020,
      TRUE ~ decade
    )
  ) |> 
  count(decade, trunc_decade)

ggplot(imdb, aes(x = decade, y = trunc_note)) +
  geom_count()


# GPT output --------------------------------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)

# Create a sample dataset
set.seed(123)
data <- expand.grid(
  decade = c("até 1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020", "até hoje"),
  rating = seq(8.1, 9.3, by = 0.1)
) %>%
  mutate(
    count = sample(c(0, 4, 8, 12, 16), n(), replace = TRUE)  # Random counts for illustration
  ) %>%
  filter(count > 0)  # Keep only points with counts

# Define a color palette and point sizes
palette <- c("#71c7ec")  # Light blue
sizes <- c(4, 8, 12, 16)

# Create the plot
ggplot(data, aes(x = decade, y = rating)) +
  geom_point(aes(size = count), color = palette, alpha = 0.8) +
  scale_size_continuous(range = c(3, 10), name = "Número de filmes") +
  scale_y_continuous(breaks = seq(8.1, 9.3, by = 0.2)) +
  scale_x_discrete(limits = c("até 1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020", "até hoje")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  ) +
  annotate("text", x = "1990", y = 9.2, label = "entre os melhores\n20 filmes", size = 4, color = "#1d73b2", hjust = 0) +
  annotate("text", x = "até hoje", y = 9.2, label = "entre os melhores\n250 filmes", size = 4, color = "#1d73b2", hjust = 0) +
  labs(
    title = "Notas dos 250 filmes melhor avaliados por década de estreia",
    subtitle = "Segundo avaliações do IMDB até março de 2024",
    caption = "Quanto maior o círculo, mais filmes com aquela nota"
  )

