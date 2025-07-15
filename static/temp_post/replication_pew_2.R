library(dplyr)
library(ggplot2)
library(ragg)

# https://www.pewresearch.org/short-reads/2024/10/25/a-look-at-the-state-of-affordable-housing-in-the-us/

home_size_data <- tribble(
  ~size_category, ~year, ~homes_thousands,
  "Under 1,400 sf", 2023, 87,
  "Under 1,400 sf", 2004, 186,
  "1,400-1,799 sf", 2023, 177,
  "1,400-1,799 sf", 2004, 311,
  "1,800-2,399 sf", 2023, 304,
  "1,800-2,399 sf", 2004, 433,
  "2,400-2,999 sf", 2023, 196,
  "2,400-2,999 sf", 2004, 291,
  "3,000-3,999 sf", 2023, 155,
  "3,000-3,999 sf", 2004, 219,
  "4,000 sf or more", 2023, 80,
  "4,000 sf or more", 2004, 92
)


# Create the tribble with extracted data
home_size_data <- tribble(
  ~size_category, ~year, ~homes_thousands,
  "Under 1,400 sf", 2023, 87,
  "Under 1,400 sf", 2004, 186,
  "1,400-1,799 sf", 2023, 177,
  "1,400-1,799 sf", 2004, 311,
  "1,800-2,399 sf", 2023, 304,
  "1,800-2,399 sf", 2004, 433,
  "2,400-2,999 sf", 2023, 196,
  "2,400-2,999 sf", 2004, 291,
  "3,000-3,999 sf", 2023, 155,
  "3,000-3,999 sf", 2004, 219,
  "4,000 sf or more", 2023, 80,
  "4,000 sf or more", 2004, 92
)

homes <- home_size_data %>%
  mutate(
    size_category = factor(
      size_category,
      levels = c(
        "Under 1,400 sf",
        "1,400-1,799 sf",
        "1,800-2,399 sf",
        "2,400-2,999 sf",
        "3,000-3,999 sf",
        "4,000 sf or more"
      ),
      labels = c(
        "Under\n1,400 sf",
        "1,400-\n1,799 sf",
        "1,800-\n2,399 sf",
        "2,400-\n2,999 sf",
        "3,000-\n3,999 sf",
        "4,000 sf\nor more"
      )
    ),
    size_category = forcats::fct_rev(size_category),
    plot_order = factor(if_else(year == 2004, 1L, 2L))
  )

homes <- homes %>%
  mutate(
    yval = rev(homes_thousands), .by = "size_category",
    ytext = if_else(plot_order == 1L, homes_thousands + 30, homes_thousands - 30),
    col_text = if_else(plot_order == 1L, "#bcb7c1", "#e5e3e7")
  )
# Original font (Franklin Gothic) is not available on Mac OS
font_text <- "Libre Franklin"

library(ggtext)

main_plot <- ggplot(homes, aes(size_category, yval)) +
  geom_col(aes(fill = plot_order), position = position_dodge(width = -0.4), width = 0.8) +
  geom_text(
    aes(y = ytext, label = homes_thousands, color = col_text, group = plot_order),
    position = position_dodge(width = 0.4),
    family = font_text,
    size = 4
  ) +
  geom_text(
    data = subset(homes, size_category == "Under\n1,400 sf"),
    aes(y = c(50, 149), label = year, color = c("#6f6579", "#aaa3b0"), group = plot_order),
    vjust = c(-3, -1.7),
    family = font_text,
    fontface = "bold"
  ) +
  geom_textbox(
    data = tibble(x = 5.85, y = 312),
    aes(x, y),
    label = "<span style='color:#a8a1ae;'><b>186,000</b></span> homes under<br>1,400 square feet were<br>built in 2004, compared<br>with <span style='color:#71687c;'><b>87,000</b></span> in 2023.",
    hjust = 0,
    vjust = 0.5,
    box.colour = "#d1d5db",
    fill = "#f9fafb",
    box.padding = unit(c(8, 8, 8, 8), "pt")
  ) +
  coord_flip() +
  scale_y_continuous(expand = expansion(0, 0.45), limits = c(NA, 900)) +
  scale_fill_manual(values = c("#756b7f", "#c7c2cc")) +
  scale_color_identity() +
  guides(fill = "none", color = "none") +
  theme_minimal(base_family = font_text) +
  theme(
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10, color = "#333333"),
    axis.title = element_blank()
  )

library(patchwork)

main_plot & plot_annotation(
  title = "Fewer homes – especially starter\nhomes – are being built now compared\nwith about 2 decades ago",
  subtitle = "Newly built single-family homes in the U.S.,\nby square footage (in thousands)",
  caption = "Source: Census Bureau, Survey of Construction."
) &
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray40", face = "italic", family = "Georgia"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0, margin = margin(t = 5))
  )


ggplot() +
  geom_col(
    data = subset(homes, year == "2023"),
    aes(x = size_category, y = homes_thousands, fill = year),
    position = position_dodge(width = 0.4),
  ) +
  geom_col(
    data = subset(homes, year == "2004"),
    aes(x = size_category, y = homes_thousands, fill = year),
    position = position_dodge(width = 0.4)
  ) +
  coord_flip()




# Claude AI
ggplot(home_size_data, aes(x = factor(size_category, levels = rev(c("Under 1,400 sf", "1,400-1,799 sf", "1,800-2,399 sf", "2,400-2,999 sf", "3,000-3,999 sf", "4,000 sf or more"))), y = homes_thousands, fill = factor(year))) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("2023" = "#4a5568", "2004" = "#a0aec0")) +
  labs(
    title = "Fewer homes – especially starter homes – are being built now compared\nwith about 2 decades ago",
    subtitle = "Newly built single-family homes in the U.S., by square footage (in thousands)",
    caption = "Source: Census Bureau, Survey of Construction.\nPEW RESEARCH CENTER",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10), lineheight = 1.2),
    plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 15), style = "italic"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0, margin = margin(t = 15)),
    plot.margin = margin(20, 20, 20, 20),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0, 0, 10, 0)
  ) +
  geom_text(aes(label = scales::comma(homes_thousands)), 
            position = position_dodge(width = 0.7), 
            hjust = -0.1, 
            size = 3.5, 
            color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  # Add annotation for the highlighted stat
  annotate("text", x = 6.3, y = 350, 
           label = "186,000 homes under\n1,400 square feet were\nbuilt in 2004, compared\nwith 87,000 in 2023.", 
           size = 3, color = "gray30", hjust = 0, vjust = 0.5,
           lineheight = 1.1)
