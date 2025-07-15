library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(ragg)

# https://www.pewresearch.org/religion/2025/06/09/how-the-global-religious-landscape-changed-from-2010-to-2020/pr_2025-06-09_global-religious-change_0-012/

import::from(forcats, fct_rev)

# Convert to tribble format
relig <- tribble(
  ~religion,                    ~age_group,    ~percentage,
  "Muslims",                    "Ages 0-14",   33,
  "Muslims",                    "15-49",       51,
  "Muslims",                    "50+",         15,
  "Christians",                 "Ages 0-14",   27,
  "Christians",                 "15-49",       47,
  "Christians",                 "50+",         26,
  "Hindus",                     "Ages 0-14",   26,
  "Hindus",                     "15-49",       55,
  "Hindus",                     "50+",         20,
  "Jews",                       "Ages 0-14",   22,
  "Jews",                       "15-49",       42,
  "Jews",                       "50+",         36,
  "Other religions",            "Ages 0-14",   22,
  "Other religions",            "15-49",       51,
  "Other religions",            "50+",         28,
  "Religiously unaffiliated",   "Ages 0-14",   19,
  "Religiously unaffiliated",   "15-49",       51,
  "Religiously unaffiliated",   "50+",         31,
  "Buddhists",                  "Ages 0-14",   18,
  "Buddhists",                  "15-49",       47,
  "Buddhists",                  "50+",         36,
  "All",                        "Ages 0-14",   26,
  "All",                        "15-49",       50,
  "All",                        "50+",         24
)

relig_lvls <- c(
  "Muslims", "Christians", "Hindus", "Jews", "Other religions",
  "Religiously unaffiliated", "Buddhists", "All")


relig <- relig %>%
  mutate(
    age_group = factor(age_group, levels = c("Ages 0-14", "15-49", "50+")),
    age_group = forcats::fct_rev(age_group),
    religion = factor(religion, levels = relig_lvls),
    religion = forcats::fct_rev(religion)
  )

relig <- relig %>%
  mutate(
    label = if_else(
      religion == "Muslims",
      paste0(percentage, "%"),
      as.character(percentage)),
    text_color = if_else(
      stringr::str_detect(age_group, "^50+"),
      "#ffffff",
      "#000000")
    )

# Original font (Franklin Gothic) is not available on Mac OS
font_text <- "Libre Franklin"

main_plot <- ggplot(relig, aes(religion, percentage)) +
  geom_col(aes(fill = age_group), position = "fill") +
  geom_text(
    aes(y = percentage / 100, label = label, color = text_color, group = age_group),
    position = position_stack(vjust = 0.5),
    size = 3,
    family = font_text
  ) +
  coord_flip() +
  scale_fill_manual(
    name = "",
    values = c("#1c5d7b", "#5ab9e5", "#92d1ed"),
    # Small hack to increase horizontal spacing between legend text
    labels = \(x) paste0(x, "        ")
  ) +
  scale_color_identity() +
  # Reverse the order of the legend to match original plot
  guides(fill = guide_legend(reverse = TRUE), color = "none") +
  # Remove horizontal spacing between columns and edges of the plot
  scale_y_continuous(expand = expansion(0)) +
  theme_minimal(base_family = font_text, base_size = 10) +
  theme(
    # Remove all grid lines
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove axis ticks and titles
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    # Remove x axis text but keep y
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10, color = '#000000'),
    # Adjust legend to match original
    legend.position = "top",
    legend.key.height = unit(0.25, "cm"),
    legend.key.width = unit(0.35, "cm"),
    legend.text = element_text(size = 10)
  )

prc_plot <- main_plot & plot_annotation(
  title = "Vast majority of Muslims around the world are younger than 50",
  subtitle = "As of 2020, % of people in each religious category who are ...",
  caption = "Note: Figures may not add up to 100% due to rounding.\nSource: Pew Research Center estimates based on more than 2,700 censuses and surveys.\n\"How the Global Religious Landscape Changed From 2010 to 2020\""
) &
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40", face = "italic", family = "Georgia"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0, margin = margin(t = 5)),
  )

prc_plot
