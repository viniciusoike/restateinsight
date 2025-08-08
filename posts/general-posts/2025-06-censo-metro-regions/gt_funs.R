library(gt)
library(gtExtras)

# Theme 5: Professional Deep
ekio_professional_deep_table <- list(
  primary_color = "#0F3A65",
  secondary_color = "#7090b5",
  light_bg = "#f2f4f7",
  stripe_bg = "#f8f9fa",
  text_color = "#2c3e50",
  caption_color = "#7f8c8d",
  border_color = "#e0e6ed",
  font_family = "Helvetica Neue"
)

gt_theme_ekio <- function(
  dat,
  style = "professional_deep",
  table_width = "100%",
  font_size = 14
) {
  # Select theme configuration
  theme_config <- switch(
    style,
    "modern_premium" = ekio_modern_premium_table,
    "academic_authority" = ekio_academic_authority_table,
    "sophisticated_unique" = ekio_sophisticated_unique_table,
    "institutional_oxford" = ekio_institutional_oxford_table,
    "professional_deep" = ekio_professional_deep_table,
    "premium_steel" = ekio_premium_steel_table,
    ekio_modern_premium_table # default
  )

  # Apply EKIO styling
  fmt_table <- dat |>
    # Font and basic styling
    opt_table_font(font = theme_config$font_family) |>
    tab_options(
      # Table dimensions
      table.width = table_width,
      table.font.size = px(font_size),
      table.font.color = theme_config$text_color,

      # Header styling
      heading.background.color = "white",
      heading.title.font.size = px(font_size + 4),
      heading.title.font.weight = "normal",
      heading.subtitle.font.size = px(font_size - 1),
      heading.subtitle.font.weight = "normal",
      heading.padding = px(4),
      heading.border.bottom.style = "solid",
      heading.border.bottom.width = px(2),
      heading.border.bottom.color = theme_config$primary_color,

      # Column labels
      column_labels.background.color = theme_config$primary_color,
      column_labels.font.size = px(font_size),
      column_labels.font.weight = "500",
      column_labels.padding = px(8),
      column_labels.border.top.style = "none",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = theme_config$border_color,

      # Data rows
      data_row.padding = px(6),
      row.striping.include_table_body = TRUE,
      row.striping.background_color = theme_config$stripe_bg,

      # Table borders
      table.border.top.style = "none",
      # table.border.top.width = px(2),
      # table.border.top.color = theme_config$primary_color,
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      table.border.bottom.color = theme_config$primary_color,
      table.border.left.style = "none",
      table.border.right.style = "none",

      # Source notes
      source_notes.font.size = px(font_size - 2),
      source_notes.border.lr.style = "none",
      source_notes.padding = px(8),
      source_notes.background.color = theme_config$light_bg
    ) |>

    # Column label styling
    tab_style(
      style = list(
        cell_text(color = "white", weight = "500"),
        cell_fill(color = theme_config$primary_color)
      ),
      locations = cells_column_labels()
    ) |>

    # Column spanners styling
    tab_style(
      style = list(
        cell_text(weight = "600", color = "white"),
        cell_fill(color = theme_config$primary_color)
      ),
      locations = cells_column_spanners()
    ) |>

    # Data row borders
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = theme_config$border_color,
        weight = px(0.5)
      ),
      locations = cells_body()
    ) |>

    # Source notes styling
    tab_style(
      style = list(
        cell_text(color = theme_config$caption_color, size = px(font_size - 2)),
        cell_fill(color = theme_config$light_bg)
      ),
      location = cells_source_notes()
    ) |>

    # Title styling
    tab_style(
      style = cell_text(
        color = theme_config$title_color,
        weight = "400",
        align = "left"
      ),
      locations = cells_title()
    )

  return(fmt_table)
}

table_state <- function(dat, dat_long, code) {
  subdat <- dat %>%
    filter(code_state %in% code) %>%
    select(name_metro, total_round_2022, starts_with("tcg"))

  timeline <- dat_long %>%
    filter(code_state %in% code) %>%
    summarise(
      TCG = list(na.omit(c(tcg * 100))),
      .by = "name_metro"
    )

  gt_colnames <- c(
    "Região Metro",
    "Pop. 2022",
    "1991/2000",
    "2000/2010",
    "2010/2022"
  )

  names(gt_colnames) <- names(subdat)[1:length(gt_colnames)]

  subdat <- left_join(subdat, timeline)

  gt(subdat) %>%
    cols_hide(columns = starts_with("class")) %>%
    cols_label(.list = gt_colnames) %>%
    tab_spanner("Crescimento (%)", columns = 3:5) %>%
    fmt_number(starts_with("total"), decimals = 0, sep_mark = ".") %>%
    fmt_percent(starts_with("tcg"), decimals = 2, dec_mark = ",") %>%
    ## Target Timeline column
    gt_plt_sparkline(
      column = TCG,
      palette = c("gray30", "black", "firebrick", "dodgerblue", "lightgrey"),
      fig_dim = c(5, 28)
    ) %>%
    gt_theme_ekio() %>%
    data_color(
      columns = tcg_2000:tcg_2022,
      palette = pal_rdbu,
      method = "quantile",
      quantiles = 5
    )
}
