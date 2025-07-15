library(sf)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(showtext)
library(stringr)
import::from(sidrar, get_sidra)
font_add_google("Lato", "Lato")
showtext_auto()

gt_theme_simple <- function(dat) {
  
  fmt_table <- dat |>
    opt_table_font(font = "DIN Alternate") |>
    tab_options(
      column_labels.background.color = "#fefefe",
      data_row.padding = px(4),
      column_labels.font.size = px(16),
      table.font.size = px(14),
      table.font.color = "#374151",

      column_labels.border.top.style = "solid",
      column_labels.border.top.width = px(2),
      column_labels.border.top.color = "#1B365D",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      column_labels.border.bottom.color = "#E5E7EB",

      # Row styling
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#FAFBFC",

      # Borders
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      table.border.bottom.color = "#1B365D",
      # Source notes
      source_notes.font.size = px(10),
      source_notes.border.lr.style = "none",
      source_notes.padding = px(10)
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#fefefe", weight = "bold"),
        cell_fill(color = "#1B365D")
      ),
      locations = cells_column_labels()
    ) %>%
    # Style spanners
    tab_style(
      style = list(
        cell_text(weight = "bold", color = "#fefefe"),
        cell_fill(color = "#1B365D")
      ),
      locations = cells_column_spanners()
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "#E5E7EB",
        weight = px(1)
      ),
      locations = cells_body()
    ) %>%
    tab_style(
      style = list(
        cell_text(color = "#6B7280")
      ),
      location = cells_source_notes()
    )
}

state_border = geobr::read_state(showProgress = FALSE)
dim_state = as_tibble(st_drop_geometry(state_border))

codes = c(93070, 93084:93098, 49108, 49109, 60040, 60041, 6653)

tab_population = sidrar::get_sidra(
  9514,
  variable = 93,
  geo = "State",
  classific = "c287",
  category = list(codes)
)


tab_pop <- tab_population |>
  janitor::clean_names() |>
  as_tibble() |>
  filter(sexo == "Total", forma_de_declaracao_da_idade == "Total") |>
  select(
    code_state = unidade_da_federacao_codigo,
    age_group = idade,
    count = valor
  )

tab_pop <- tab_pop |>
  mutate(
    code_state = as.numeric(code_state),
    age_min = as.numeric(str_extract(age_group, "\\d+")),
    age_group = factor(age_group),
    age_group = forcats::fct_reorder(age_group, age_min),
    age_ibge = case_when(
      age_min < 15 ~ "young",
      age_min >= 15 & age_min < 65 ~ "adult",
      age_min >= 65 ~ "elder"
    ),
    factor(age_ibge, levels = c("young", "adult", "elder"))
  )

pop_state <- tab_pop |>
  summarise(
    total = sum(count),
    .by = c("age_ibge", "code_state")
  ) |>
  pivot_wider(
    id_cols = "code_state",
    names_from = "age_ibge",
    values_from = "total"
  ) |>
  mutate(
    dre = elder / adult * 100,
    dry = young / adult * 100,
    tdr = dre + dry
  )

tab_pop_state <- left_join(dim_state, pop_state, by = "code_state")
pop <- left_join(state_border, pop_state, by = "code_state")


# pal <- colorNumeric("Blues", pop$tdr)
# bins <- quantile(pop$tdr, probs = seq(0, 1, 0.2))
# bins <- BAMMtools::getJenksBreaks(pop$tdr, k = 6)

pal_tdr <- colorBin(
  palette = as.character(MetBrewer::met.brewer("Hokusai2", 5)),
  domain = pop$tdr,
  bins = BAMMtools::getJenksBreaks(pop$tdr, k = 6)
)

pal_rdi <- colorBin(
  palette = as.character(MetBrewer::met.brewer("Hokusai2", 5)),
  domain = pop$dre,
  bins = BAMMtools::getJenksBreaks(pop$dre, k = 6)
)

pal_rdj <- colorBin(
  palette = as.character(MetBrewer::met.brewer("Hokusai2", 5)),
  domain = pop$dry,
  bins = BAMMtools::getJenksBreaks(pop$dry, k = 6)
)

# pal <- colorBin(
#   palette = as.character(MetBrewer::met.brewer("Hokusai2", 5)),
#   domain = pop$tdr,
#   bins = bins
# )

labels <- sprintf(
  "<b>RDT<b/>: %s <br>
   <b>RDJ<b/>: %s <br>
   <b>RDI<b/>: %s <br>",
  format(round(pop$tdr, 1), decimal.mark = ","),
  format(round(pop$dry, 1), decimal.mark = ","),
  format(round(pop$dre, 1), decimal.mark = ",")
)

labels <- lapply(labels, htmltools::HTML)

map <- leaflet(pop) |>
  addTiles() |>
  addPolygons(
    group = "RDT (Total)",
    fillColor = ~ pal_tdr(tdr),
    weight = 2,
    color = "white",
    fillOpacity = 0.9,
    highlightOptions = highlightOptions(
      color = "#e09351",
      weight = 10,
      fillOpacity = 0.8,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", "font-family" = "Fira Code")
    )
  ) %>%
  addPolygons(
    group = "RDJ (Jovem)",
    fillColor = ~ pal_rdj(dry),
    weight = 2,
    color = "white",
    fillOpacity = 0.9,
    highlightOptions = highlightOptions(
      color = "#e09351",
      weight = 10,
      fillOpacity = 0.8,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", "font-family" = "Fira Code")
    )
  ) %>%
  addPolygons(
    group = "RDI (Idoso)",
    fillColor = ~ pal_rdi(dre),
    weight = 2,
    color = "white",
    fillOpacity = 0.9,
    highlightOptions = highlightOptions(
      color = "#e09351",
      weight = 10,
      fillOpacity = 0.8,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", "font-family" = "Fira Code")
    )
  ) %>%
  addLegend(
    pal = pal_tdr,
    values = ~tdr,
    labFormat = labelFormat(digits = 1),
    title = "RDT (2022)",
    position = "bottomright",
    group = "RDT (Total)"
  ) %>%
  addLegend(
    pal = pal_rdj,
    values = ~dry,
    labFormat = labelFormat(digits = 1),
    title = "RDJ (2022)",
    position = "bottomright",
    group = "RDJ (Jovem)"
  ) %>%
  addLegend(
    pal = pal_rdi,
    values = ~dre,
    labFormat = labelFormat(digits = 1),
    title = "RDI (2022)",
    position = "bottomright",
    group = "RDI (Idoso)"
  ) %>%
  addLayersControl(
    overlayGroups = c("RDT (Total)", "RDJ (Jovem)", "RDI (Idoso)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addProviderTiles(providers$CartoDB) |>
  setView(lng = -53.1873, lat = -10.58913, zoom = 4) %>%
  groupOptions(group = "RDT (Total)", zoomLevels = 4) %>%
  groupOptions(group = "RDJ (Jovem)", zoomLevels = c(1, 18)) %>%
  groupOptions(group = "RDI (Idoso)", zoomLevels = c(1, 18))


library(biscale)
library(patchwork)
dep_biclass <- bi_class(pop, dre, dry, dim = 3, style = "quantile")

p_map <- ggplot(dep_biclass) +
  geom_sf(
    aes(fill = bi_class),
    color = "white",
    lwd = 0.2,
    show.legend = FALSE
  ) +
  coord_sf(xlim = c(NA, -35)) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme()

p_legend <- bi_legend(
  pal = "GrPink",
  dim = 3,
  xlab = "RDI (%)",
  ylab = "RDJ (%)",
  size = 10
)

map_states <- p_map + inset_element(p_legend, 0, 0.1, 0.35, 0.45)

plot_map <- map_states +
  plot_annotation(
    title = "Transição Demográfica Brasileira",
    subtitle = stringr::str_wrap(
      "Razão de dependência jovem e idosa no Brasil por estado (2022). Sul e Sudeste são os mais envelhecidos enquanto Norte ainda é jovem. Nordeste e Centro-Oeste são mais heterogêneos, mas já apresentam sinais de envelhecimento populacional.",
      80
    )
  ) &
  theme(
    panel.background = element_rect(fill = "#ffffff", color = NA),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    plot.margin = margin(10, 5, 5, 10),
    plot.title = element_text(size = 18, hjust = 0, family = "Lato"),
    plot.subtitle = element_text(
      size = 10,
      hjust = 0,
      family = "Lato",
      color = "gray20"
    )
  )


tab <- tab_pop_state |>
  mutate(population = young + adult + elder) |>
  # mutate(across(dre:tdr, ~.x * 100)) |>
  select(name_state, dre, dry, tdr, population) |>
  arrange(desc(tdr))

table_population <- gt(tab) |>
  cols_label(
    name_state = "Nome UF",
    dre = "Idoso",
    dry = "Jovem",
    tdr = "Total",
    population = "População"
  ) |>
  tab_spanner(label = "Razão de Dependência", 2:4) |>
  fmt_number(2:4, decimals = 1, dec_mark = ",") |>
  fmt_number(5, decimals = 0, sep_mark = ".") |>
  tab_source_note("Fonte: IBGE (Censo 2022)") |>
  gt_theme_simple()


get_population <- function(state) {
  tab_population = sidrar::get_sidra(
    9514,
    variable = 93,
    geo = "State",
    geo.filter = list("State" = state)
  )

  return(tab_population)
}

clean_population <- function(dat, group_var) {
  tab_pop = dat |>
    janitor::clean_names() |>
    tidyr::as_tibble() |>
    dplyr::filter(
      sexo != "Total",
      forma_de_declaracao_da_idade == "Total",
      idade_codigo %in% codes
    ) |>
    dplyr::select(
      code_state = unidade_da_federacao_codigo,
      age_group = idade,
      sex = sexo,
      count = valor
    )

  tab_pop <- tab_pop |>
    dplyr::mutate(
      code_state = as.numeric(code_state),
      age_min = as.numeric(stringr::str_extract(age_group, "\\d+")),
      age_group = factor(age_group),
      age_group = forcats::fct_reorder(age_group, age_min),
      sex = factor(
        sex,
        levels = c("Homens", "Mulheres"),
        labels = c("Masculino", "Feminino")
      )
    ) |>
    dplyr::mutate(share = count / sum(count) * 100, .by = c("code_state")) |>
    dplyr::mutate(share = dplyr::if_else(sex == "Masculino", -share, share))

  return(tab_pop)
}


states <- c(13, 22, 43, 42)
ls_population <- lapply(states, get_population)
ls_population <- lapply(ls_population, clean_population)
dat_pop <- bind_rows(ls_population)


sub <- dplyr::filter(dat_pop, code_state %in% states)

theme_plot <- theme_minimal(base_family = "Lato", base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, hjust = 1)
  )

sub <- sub |>
  mutate(
    age_group_lump = if_else(age_min >= 80, "80 ou mais", age_group),
    age_group_lump = str_remove(age_group_lump, " anos"),
    age_group_lump = factor(age_group_lump),
    age_group_lump = forcats::fct_reorder(age_group_lump, age_min),
    .by = "code_state"
  ) |>
  summarise(
    total = sum(count),
    .by = c("code_state", "sex", "age_group_lump")
  ) |>
  mutate(share = total / sum(total) * 100, .by = c("code_state")) |>
  mutate(share = if_else(sex == "Masculino", -share, share))

sub <- left_join(sub, dim_state, by = "code_state")

colors_pyramid <- c("#1B9E77", "#7570B3")

breaks_share <- seq(-6, 6, 1)
labels_share <- str_remove(as.character(breaks_share), "-")

plot_pyramid <- ggplot(
  sub,
  aes(x = age_group_lump, y = share, fill = sex, color = sex)
) +
  geom_col(alpha = 0.9, width = 0.75) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(vars(name_state)) +
  scale_y_continuous(
    breaks = breaks_share,
    labels = labels_share
  ) +
  scale_fill_manual(values = colors_pyramid) +
  scale_color_manual(values = colors_pyramid) +
  labs(
    title = "Pirâmides demográficas",
    subtitle = stringr::str_wrap(
      "Amazonas tem população jovem, RDI baixo e RDJ elevado.
    Piauí está em transição com população jovem encolhendo e número de idosos crescendo.
    Rio Grande do Sul, tem população envelhecida, maior RDI do Brasil.
    Santa Catarina compensa população idosa com maior população adulta.",
      91
    ),
    x = NULL,
    y = "Percentual da população(%)"
  ) +
  guides(fill = "none", color = "none") +
  theme_plot +
  theme(
    panel.grid.major.x = element_line(linetype = 2),
    panel.border = element_rect(fill = NA, color = "gray15", linewidth = 0.5),
    strip.text = element_text(size = 12, color = "gray15"),
    axis.text.y = element_text(hjust = 0.5),
  )


demographic <- get_sidra(7360, variable = 10609:10612, geo = "State")

tab_demographic <- demographic |>
  janitor::clean_names() |>
  as_tibble() |>
  select(
    code_state = unidade_da_federacao_codigo,
    year = ano_2,
    variable = variavel,
    value = valor
  ) |>
  mutate(year = as.numeric(year), code_state = as.numeric(code_state))

tab_demographic_state <- left_join(
  tab_demographic,
  dim_state,
  by = "code_state"
)

demographic <- get_sidra(7360, variable = 10609:10612, geo = "Region")

tab_demographic <- demographic |>
  janitor::clean_names() |>
  as_tibble() |>
  select(
    code_region = grande_regiao_codigo,
    year = ano_2,
    variable = variavel,
    value = valor
  ) |>
  mutate(year = as.numeric(year), code_region = as.numeric(code_region))

dim_region <- distinct(dim_state, code_region, name_region)

tab_demographic_region <- left_join(
  tab_demographic,
  dim_region,
  by = "code_region"
)


table_region <- tab_demographic_region |>
  filter(
    year %in% seq(2010, 2060, 5),
    variable == "Razão de dependência total"
  ) |>
  pivot_wider(
    id_cols = "name_region",
    names_from = "year",
    values_from = "value"
  ) |>
  gt() |>
  tab_header(title = "RDT no Brasil por Regiões") |>
  cols_label(name_region = "Região") |>
  fmt_number(2:12, decimals = 1) |>
  tab_source_note("Fonte: IBGE (Censo 2022)") |>
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#fefefe"),
      cell_fill(color = "#1B365D")
    ),
    locations = cells_title()
  ) |>
  gt_theme_simple()


tab_demographic_region <- tab_demographic_region |>
  mutate(
    is_forecast = factor(if_else(year >= 2022, 1L, 0L))
  )

sub_dem_region <- tab_demographic_region |>
  filter(variable != "Índice de envelhecimento")


plot_proj <-
  ggplot(sub_dem_region, aes(year, value, color = variable)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(linetype = is_forecast), linewidth = 0.8) +
  scale_color_manual(
    name = "",
    values = c("#264653", "#2a9d8f", "#e76f51"),
    labels = c("RDI", "RDJ", "RDT")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(vars(name_region)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Evolução da RDT no Brasil (2000-2060)",
    subtitle = "Evolução da Razão de Dependência no Brasil 2000-2060",
    caption = "Fonte: IBGE (Projeções da População)"
  ) +
  guides(linetype = "none") +
  theme_minimal(base_family = "Lato") +
  theme_plot +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11, color = "gray30"),
    strip.text = element_text(size = 12, color = "gray15"),
    axis.text.x = element_text(angle = 90),
    panel.border = element_rect(fill = NA, color = "gray15", linewidth = 0.5)
  )


# Export

export <- list(
  table_region = table_region,
  table_population = table_population,
  leaflet = map,
  map = plot_map,
  proj = plot_proj,
  pyramid = plot_pyramid
)

readr::write_rds(
  export,
  here::here("posts/general-posts/2025-07-demografia-brasil/files.rds")
)
