library(ggplot2)
library(patchwork)

x <- c("Houston", "Dallas", "Denton County", "Austin",
       "San Antonio", "Collin County", "Fort Worth", "Fort Bend")

sub <- txhousing |>
  dplyr::filter(city %in% x) |> 
  dplyr::group_by(city, year) |> 
  dplyr::summarise(year_sales = sum(sales)) |> 
  dplyr::filter(year %in% c(2006, 2010)) 

lvls <- sub |> 
  dplyr::filter(year == 2006) |> 
  dplyr::arrange(year_sales) |> 
  dplyr::pull(city)

sub <- dplyr::mutate(sub, city = factor(city, levels = lvls))

p2 <- ggplot(sub, aes(x = city, y = year_sales)) +
  geom_line(aes(group = city), color = "gray30") +
  geom_point(aes(color = as.factor(year)), size = 3) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) +
  scale_y_continuous(
    breaks = seq(0, 80000, 20000),
    labels = scales::label_number(big.mark = "."),
    limits = c(0, NA)) +
  scale_color_manual(
    name = "Ano",
    values = c("#2A9D8F", "#E9C46A"),
    labels = c("2006", "2010")
  ) +
  coord_flip() +
  labs(
    title = "Queda na venda de imóveis nos principais mercados", 
    x = NULL,
    y = "Vendas") +
  theme_light() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank())

p1 <- ggplot(filter(sub, year == 2006)) +
  geom_segment(aes(x = city, xend = city, y = 0, yend = year_sales),
               color = "gray30") +
  geom_point(aes(x = city, y = year_sales), size = 3, color = "#2A9D8F") +
  labs(
    title = "Vendas de imóveis em 2006", 
    x = NULL,
    y = "Vendas") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  theme_light()

sub <- txhousing |>
  dplyr::filter(city %in% c("Houston", "Dallas", "Austin")) |> 
  dplyr::group_by(city, year) |> 
  dplyr::summarise(year_sales = sum(sales)) |> 
  dplyr::filter(year %in% 2006:2010) 

p3 <- ggplot(sub, aes(x = city, xend = city, y = year_sales, yend = 0)) +
  geom_segment(color = "gray30") +
  geom_point(size = 3, color = "#2A9D8F") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  coord_flip() +
  facet_grid(vars(year)) +
  labs(
    title = "Vendas 2006/2010", 
    x = NULL,
    y = "Vendas") +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text.y = element_text(angle = 0)
  )

panel <- (p1 + p3) / p2
