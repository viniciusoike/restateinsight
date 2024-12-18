library(ggplot2)
library(dplyr)
library(ggtext)

import::from(forcats, fct_rev)
import::from(countrycode, countrycode)
import::from(maddison, maddison)

countryname("Brazil", "region")

dat <- maddison |> 
  filter(year >= 1999) |> 
  mutate(
    growth = rgdpnapc / lag(rgdpnapc) - 1,
    is_growth = if_else(growth >= 0, 1L, 0L),
    .by = "iso3c"
  )

recession_region <- dat |> 
  filter(year >= 2000) |> 
  mutate(region_wb = countrycode(iso3c, "iso3c", "region")) |> 
  filter(!is.na(region_wb)) |> 
  summarise(
    growth = sum(is_growth, na.rm = TRUE),
    total = n(),
    .by = c("region_wb", "year")
  ) |> 
  mutate(
    share = (1 - growth / total) * 100,
    highlight = factor(if_else(share > 20, 1, 0)),
    region_wb = as.factor(region_wb),
    region_wb = fct_rev(region_wb)
  )

x_numbers <- seq(min(recession_region$year), max(recession_region$year), 1)

bn <- x_numbers[x_numbers %% 5 == 0]
sn <- x_numbers[x_numbers %% 5 != 0]
subsn <- stringr::str_sub(sn, 3, 4)


small_numbers <- glue::glue('<span style="font-size: 10px;">{subsn}</span>')
big_numbers <- glue::glue('<span style="font-size: 14px;">{bn}</span>')

names(big_numbers) <- as.integer(bn)
names(small_numbers) <- as.integer(sn)

xlabels <- c(big_numbers, small_numbers)
xlabels <- xlabels[order(names(xlabels))]

# xlabels <- c(
#   '<span style="font-size: 14px;">2000</span>',
#   '<span style="font-size: 10px;">01</span>',
#   '<span style="font-size: 10px;">02</span>',
#   '<span style="font-size: 10px;">03</span>',
#   '<span style="font-size: 10px;">04</span>',
#   '<span style="font-size: 14px;">2005</span>'
# )

# xlabels <- c(
#   "<b>2000<b/>", "01", "02", "03", "04", "2005", "06", "07", "08", "09", "2010",
#   "11", "12", "13", "14", "2015", "16"
#   )

p <- ggplot(recession_region, aes(year, region_wb)) + 
  geom_count(aes(size = share, color = highlight)) +
  scale_size_continuous(
    name = "Share of countries\nin recession (%)",
    breaks = c(0, 20, 40, 60, 80, 100)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2016, 1),
    labels = xlabels,
    guide = guide_axis(n.dodge = 1)) +
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, width = 17)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(3, "RdBu")[c(3, 1)]) +
  guides(
    color = "none", 
    size = guide_legend(label.position = "bottom", nrow = 1)
    ) +
  labs(
    title = "Economic Recessions across regions",
    subtitle = "Red circles show that over 20% of countries in the region are in recession.",
    x = NULL,
    y = NULL,
    caption = "Source: Maddison Project (2018)") +
  theme_minimal() +
  theme(
    axis.text.x = element_markdown(),
    legend.position = "top",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank()
    )

p

cowplot::save_plot("static/images/thumbnails/ggplot2_punchcard.svg", p, base_height = 6, base_asp = 1.1)
