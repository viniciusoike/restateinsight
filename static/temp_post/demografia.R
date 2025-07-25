library(sidrar)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(geobr)
library(sf)
library(gt)
library(gtExtras)
library(showtext)

font_add_google("Lato", "Lato")
showtext::showtext_auto()

theme_vini <- theme_minimal(base_family = "Lato") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, color = "gray10"),
    plot.subtitle = element_text(size = 10, color = "gray20"),
    plot.caption = element_text(size = 8, color = "gray60"),
    text = element_text(color = "gray20")
  )

# Import population data for all cities
# Takes a while
pop2022 <- get_sidra(4709, variable = 93, geo = "City")

pop_series <- get_sidra(
  136,
  variable = 93,
  period = "1991-2010",
  geo = "City",
  classific = c("c86"),
  category = list(0)
)

clean_sidra <- function(df, name_value = "pop") {
  
  cols <- c("code_muni" = "municipio_codigo", "year" = "ano", x = "valor")
  names(cols)[3] <- name_value
  
  df %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) 
  
  
}

metro_region <- read_metro_area(showProgress = FALSE)
dim_metro <- as_tibble(st_drop_geometry(metro_region))
dim_metro <- select(dim_metro, code_muni, name_metro)

cities <- read_municipality(year = 2020, showProgress = FALSE)
dim_city <- as_tibble(st_drop_geometry(cities))
dim_city <- left_join(dim_city, dim_metro, by = "code_muni")

#> Remove an exception: Murici is mapped into two different metro regions
dim_city <- dim_city |> 
  filter(!(code_muni == 2705507 & name_metro == "RM da Zona da Mata"))

cpop_series <- clean_sidra(pop_series)
cpop2022 <- clean_sidra(pop2022)

census_pop <- rbind(cpop_series, cpop2022)
census_pop <- left_join(census_pop, dim_city, by = "code_muni")


shorten_ride_names <- function(x) {
  
  ride_names <- c(
    "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno" = "RIDE DF",
    "RIDE TERESINA - Região Integrada de Desenvolvimento da Grande Teresina" = "RIDE Teresina",
    "RIDE Petrolina/Juazeiro Região Administrativa Integrada de Desenvolvimento do Polo Petrolina/PE e Juazeiro/BA" = "RIDE Petrolina/Juazeiro"
  )
  
  unname(ride_names[x])
  
}

tbl_pop_metro <- census_pop %>%
  filter(!is.na(name_metro)) %>%
  mutate(
    name_metro = str_remove(name_metro, "RM d[a-z]"),
    name_metro = str_remove(name_metro, "RM"),
    name_metro = str_replace(name_metro, "Aglomeração Urbana", "AU"),
    name_metro = ifelse(
      str_detect(name_metro, "^RIDE"),
      shorten_ride_names(name_metro),
      name_metro
    ),
    name_metro = str_trim(name_metro, side = "both")
  ) %>%
  summarise(
    total = sum(pop, na.rm = TRUE),
    state = paste(unique(abbrev_state), collapse = ", "),
    .by = c("year", "name_metro")
  ) %>%
  arrange(year) %>%
  arrange(name_metro)


big_metros <- tbl_pop_metro %>%
  filter(year == 2022, total > 5*1e5) %>%
  pull(name_metro)

# Get only 500k cities and compute average geometric growth
tbl_major <- tbl_pop_metro %>%
  filter(name_metro %in% big_metros) %>%
  # Not so trivial since year intervals are not the same between Census editions
  group_by(name_metro) %>%
  mutate(
    tcg = (total / lag(total))^(1/(year - lag(year) + 1)) - 1,
    total_round = round(total / 1000)) %>%
  ungroup()

# Convert to wider and arrange by 2022 population
tbl_major_wide <- tbl_major %>%
  pivot_wider(
    id_cols = c("name_metro", "state"),
    names_from = "year",
    values_from = c("total_round", "tcg")
  ) %>%
  select(-tcg_1991) %>%
  arrange(desc(total_round_2022))

timeline <- tbl_major %>%
  summarise(
    TCG = list(na.omit(c(tcg * 100))),
    .by = "name_metro"
  )

col_names <- c(
  "Região Metro", "Estado", "1991", "2000", "2010", "2022", "1991/2000", "2000/2010",
  "2010/2022"
)

names(col_names) <- names(tbl_major_wide)

gtable_cities <- tbl_major_wide %>%
  left_join(timeline) %>%
  gt() %>%
  cols_label(.list = col_names) %>%
  tab_spanner("Populacao (Mil)", columns = 3:6) %>%
  tab_spanner("Crescimento (%)", columns = 7:9) %>%
  fmt_number(starts_with("total"), decimals = 0, sep_mark = ".") %>%
  fmt_percent(starts_with("tcg"), decimals = 2) %>%
  ## Target Timeline column
  gt_plt_sparkline(
    column = TCG,
    palette = c("gray30", "black", "firebrick", "dodgerblue", "lightgrey"),
    fig_dim = c(5, 28)
  ) %>%
  gt_theme_538() %>%
  gt_color_rows(
    columns = c(tcg_2000, tcg_2010, tcg_2022),
    domain = seq(-0.005, 0.05, 0.005),
    #domain = c(min(tbl_major_wide$tcg_2022), 0, max(tbl_major_wide$tcg_2022)),
    palette = c(
      "#E63946", "#F1FAEE", "#CDEAE5", "#BBE2E1", "#A8DADC", "#90C3CD", "#77ABBD",
      "#457B9D", "#31587A", "#1D3557")
  )


pop_sul <- get_sidra(
  4709,
  variable = c(93, 10605),
  geo = "City",
  geo.filter = list("Region" = 4)
)

muni <- read_municipality(year = 2020, showProgress = FALSE)
muni <- filter(muni, code_region == 4)

state <- read_state(showProgress = FALSE)
state <- filter(state, code_region == 4)

tbl_pop_sul <- pop_sul %>%
  janitor::clean_names() %>%
  select(
    code_muni = municipio_codigo,
    name_series = variavel,
    value = valor
  ) %>%
  mutate(
    code_muni = as.numeric(code_muni),
    name_series = ifelse(str_detect(name_series, "^Pop"), "pop", "tgc")
  ) %>%
  pivot_wider(
    id_cols = "code_muni",
    names_from = "name_series",
    values_from = "value")

muni <- left_join(muni, tbl_pop_sul, by = "code_muni")

bks <- c(-2.23, -0.85, 0, 0.5, 1.2, 2.1, 3.5, 6.32)
lvls <- findInterval(muni$tgc, bks, rightmost.closed = TRUE)
fill_labels <- c(
  "-2.22, -0.85", "-0.85, 0", "0, 0.5", "0.5, 1.2", "1.2, ")
fill_labels <- paste(bks, bks[-1], sep = ",")
fill_labels

muni <- muni %>%
  mutate(
    group_tgc = classInt::classify_intervals(tgc, n = 7, style = "jenks")
  )

ggplot() +
  geom_sf(data = muni, aes(fill = as.factor(group_tgc)), color = "gray50") +
  geom_sf(data = state, fill = NA, linewidth = 0.6) +
  scale_fill_brewer(type = "div") +
  theme_void()

dfsul <- as_tibble(st_drop_geometry(muni))

ggplot(dfsul, aes(x = log(pop), y = tgc)) +
  geom_point(aes(color = abbrev_state), alpha = 0.5)

# Domicílios particulares ocupados (total e média de moradores)
domi22 <- get_sidra(4712, variable = c(381, 5930), geo = "City")

# Domicílios particulares ocupados
domi_series <- get_sidra(156, variable = 2048, geo = "City", period = "1991-2010")
# Número médio de moradores (por domicílio particular ocupado)
domi_media <- get_sidra(156, variable = 619, geo = "City", period = "1991-2010")

big_cities <- census_pop |> 
  filter(year == 2022, pop > 8 * 1e5) |> 
  pull(code_muni)

tab_big_cities <- census_pop |> 
  filter(code_muni %in% big_cities) |> 
  mutate(
    chg_abs = pop - lag(pop),
    chg_rel = pop / lag(pop) - 1,
    .by = c("code_muni", "name_muni")
  )

tab_chg <- tab_big_cities |> 
  filter(year == 2022) |> 
  mutate(
    name_muni = str_replace(name_muni, "São Bernardo Do Campo", "São Bernardo"),
    name_muni = str_replace(name_muni, " De ", " de "),
    name_muni = glue::glue("{name_muni} ({abbrev_state})"),
    name_muni = factor(name_muni),
    name_muni = forcats::fct_reorder(name_muni, chg_abs),
    chg_label = round(chg_abs / 1000),
    chg_label = ifelse(chg_abs > 0, paste0("+", chg_label), chg_label),
    pos_text_y = ifelse(chg_abs > 0, chg_abs / 1000 + 20, chg_abs / 1000 - 20)
  )

p_pop <- ggplot(tab_chg, aes(x = name_muni, y = chg_abs / 1000)) +
  geom_col(fill = "#1d3557") +
  geom_text(
    aes(x = name_muni, y = pos_text_y, label = chg_label),
    family = "Lato",
    size = 3) +
  coord_flip() +
  labs(
    title = "Ganhos e perdas de população nas grandes cidades",
    subtitle = "Mudança absoluta da população entre as cidades mais populosas do Brasil de 2010 a 2022.",
    x = NULL,
    y = "Milhares de Habitantes",
    caption = "Fonte: CENSO 2022 (IBGE).") +
  theme_vini

breaks <- c(20000, 50000, 100000, 500000, 1000000)

bb <- format(breaks, big.mark = ".", scientific = FALSE)
labels <- paste(bb, bb[-1], sep = "-")
labels <- c(" < 20.000", labels[-length(labels)], "> 1.000.000")
labels <- str_remove_all(labels, " ")

labels <- c(
  "Menos de 20 mil", "20 a 50 mil", "50 a 100 mil", "100 a 500 mil",
  "500 mil a 1 milhão", "Mais de 1 milhão"
)

dflabel <- tibble(
  pop_group = 0:5,
  labels = c(
    "Menos de 20 mil", "20 a 50 mil", "50 a 100 mil", "100 a 500 mil",
    "500 mil a 1 milhão", "Mais de 1 milhão"
  )
)

cpop2022 <- cpop2022 |> 
  mutate(pop_group = findInterval(pop, breaks))

tab_cities <- census_pop |> 
  filter(year %in% c(2010, 2022)) |> 
  mutate(
    chg_abs = pop - lag(pop),
    chg_rel = pop / lag(pop) - 1,
    .by = c("code_muni", "name_muni")
  ) |> 
  left_join(select(cpop2022, code_muni, pop_group), by = "code_muni")

tab_growth_cities <- tab_cities |> 
  filter(year == 2022) |> 
  mutate(is_growth = if_else(chg_abs > 0, 1L, 0L)) |> 
  summarise(
    total_growth = sum(chg_abs, na.rm = TRUE),
    total_wgt = weighted.mean(chg_rel, pop, na.rm = TRUE),
    total = sum(is_growth, na.rm = TRUE),
    count = n(),
    share = total / count * 100,
    .by = "pop_group"
  ) |> 
  arrange(pop_group) |> 
  left_join(dflabel)

plot_growth <- 
  ggplot(tab_growth_cities, aes(x = as.factor(pop_group), y = share)) +
  geom_col(fill = "#1d3557") +
  geom_text(
    aes(label = paste0(round(share, 1), "%")),
    color = "white",
    family = "Lato",
    nudge_y = -5) +
  scale_x_discrete(labels = str_wrap(labels, width = 15)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "Cidades de Médio e Médio-Grande Porte foram as que mais cresceram",
    subtitle = "Percentual de cidades que tiveram crescimento populacional entre 2010 e 2022.",
    x = NULL,
    y = NULL,
    caption = "Fonte: IBGE") +
  theme_vini + 
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, hjust = 0.5)
  )

tab_growth_cities <- tab_growth_cities |> 
  mutate(
    name_label = c("Pequeno", "Pequeno", "Médio", "Médio", "Médio-Grande", "Mega-Cidades")
  ) |> 
  select(name_label, labels, count, total_growth, total_wgt)

col_names <- c(
  "Porte", "Habitantes", "Número", "Crescimento\ntotal", "Crescimento\nMédio (*)"
)

names(col_names) <- names(tab_growth_cities)

gt_growth <- tab_growth_cities %>%
  gt() %>%
  cols_label(.list = col_names) %>%
  fmt_number(3:4, decimals = 0, sep_mark = ".") %>%
  fmt_percent(5) %>%
  tab_caption("Crescimento populacional das cidades por porte.") %>%
  gt_theme_538()

cities_loss <- tab_cities |> 
  slice_min(chg_abs, n = 9) |> 
  pull(code_muni)

tab_losses <- census_pop |> 
  filter(code_muni %in% cities_loss) |> 
  mutate(
    chg_abs = pop - lag(pop),
    chg_rel = pop / lag(pop) - 1,
    .by = c("code_muni", "code_region")
  ) |> 
  mutate(
    name_muni = if_else(name_muni == "Rio De Janeiro", "Rio de Janeiro", name_muni)
  )

ggplot(tab_losses, aes(x = year, y = chg_rel, fill = as.factor(code_region))) +
  geom_col() +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(name_muni)) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = c("#77ABBD", "#3B6A8C", "#A8DCCC", "#1D3557")) +
  labs(
    title = "População Caindo",
    subtitle = "Variação relativa da população entre os períodos de Censo",
    x = NULL,
    y = "%",
    caption = "Fonte: Censo 1991, 2000, 2010, 2022 (IBGE)."
  ) +
  theme_vini +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = NA, color = "gray20")
  )

tab_states <- census_pop |> 
  summarise(
    total = sum(pop, na.rm = TRUE),
    .by = c("year", "code_state", "name_state")
  ) |> 
  arrange(code_state) |> 
  mutate(
    chg_abs = total - lag(total),
    chg_rel = total / lag(total) - 1,
    tgc = (total / lag(total))^(1/(year - lag(year))) - 1,
    tgc = tgc * 100,
    .by = c("code_state")
  )

chg_state <- filter(tab_states, year == 2022)

shp <- read_state(showProgress = FALSE)

state_chg <- left_join(shp, chg_state, by = c("code_state"))

state_chg <- state_chg %>%
  mutate(
    label = format(round(tgc, 2), decimal.mark = ","),
    label = paste0(label, "%"),
    x = as.numeric(st_coordinates(st_centroid(.))[, 1]),
    y = as.numeric(st_coordinates(st_centroid(.))[, 2]),
    repel = if_else(code_state %in% c(24:28, 53), 1L, 0L),
    group = findInterval(tgc, c(0.015, 0.5, 1, 1.5, 2))
  )


ggplot(state_chg) +
  geom_sf(aes(fill = as.factor(group))) +
  geom_label(
    data = filter(state_chg, repel == 0),
    aes(x = x, y = y, label = label),
    size = 3,
    family = "Lato"
  ) +
  ggrepel::geom_label_repel(
    data = filter(state_chg, repel == 1),
    aes(x = x, y = y, label = label),
    size = 3,
    family = "Lato",
    nudge_x = 3
  ) +
  scale_fill_manual(
    name = " 2010/2022 (%)",
    values = RColorBrewer::brewer.pal(6, "PuBuGn")[2:6],
    labels = c("<0,15%", "0,15-0,5%", "0,5-1%", "1-2%", ">2%")
  ) +
  coord_sf(xlim = c(NA, -35)) +
  ggthemes::theme_map(base_family = "Lato")

unique(metro_region$name_metro)

sel_metro <- c(
  "RM São Paulo",
  "RM Rio de Janeiro",
  "RM Belo Horizonte",
  "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno",
  "RM Porto Alegre",
  "RM Fortaleza",
  "RM Recife",
  "RM Curitiba",
  "RM Salvador",
  "RM Campinas"
)

sel_city_code <- metro_region |> 
  st_drop_geometry() |> 
  filter(name_metro %in% sel_metro) |> 
  pull(code_muni)

tab_city_metro <- census_pop |> 
  filter(code_muni %in% sel_city_code) |> 
  mutate(
    chg_rel = pop / lag(pop) - 1,
    tgc = (pop / lag(pop))^(1/(year - lag(year))) - 1,
    .by = "code_muni"
  ) |> 
  pivot_wider(
    id_cols = c("code_muni", "name_metro"),
    names_from = "year",
    values_from = c("pop", "chg_rel", "tgc")
  )

cities_metro <- inner_join(cities, tab_city_metro, by = "code_muni")

breaks <- BAMMtools::getJenksBreaks(cities_metro$tgc_2022, k = 7)
breaks[3] <- 0
# labels <- format(round(breaks * 100, 2), decimal.mark = ",")
# labels <- paste(labels, labels[-1], sep = " a ")
# labels <- c(labels[-length(labels)], ">4,8%")

labels <- c(
  "-0,55 a -0,20", "-0,20 a 0,00", "0,00 a 0,81", "0,81 a  1,22", 
  "1,22 a 2,06", "2,06 a 4,79", "> 4,8%")

cities_metro <- cities_metro |> 
  dplyr::mutate(group = factor(findInterval(tgc_2022, breaks)))

plot_metro <- function(metro, size = 3, legend = FALSE) {
  
  metro <- ifelse(!str_detect(metro, "RM"), paste("RM", metro), metro)
  
  dat <- dplyr::filter(cities_metro, name_metro == metro)
  
  colors <- RColorBrewer::brewer.pal(11, "BrBG")[c(2, 4, 7, 8, 9, 10, 11)]
  
  plot <- 
    ggplot(dat) +
    geom_sf(aes(fill = group), linewidth = 0.5, color = "gray50") +
    geom_sf_label(
      aes(label = name_muni),
      size = size,
      family = "Lato",
      color = "gray20") +
    ggtitle(label = metro) +
    scale_fill_manual(
      values = colors,
      labels = labels
    ) +
    ggthemes::theme_map(base_family = "Lato") +
    theme(
      plot.title = element_text(
        size = 18,
        hjust = 0.5,
        color = "gray10"
      )
    )
  
  if (!legend) {
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
  
}

plot_metro("Curitiba")

cities_shrink <- tab_big_cities |> 
  filter(year == 2022, chg_rel < 0) |> 
  pull(code_muni)

tab_loss_cities <- tab_big_cities |> 
  filter(code_muni %in% cities_shrink, !is.na(chg_rel)) |>
  mutate(is_growth = factor(if_else(chg_rel > 0, 1, 0)))

plot_cities_loss <- 
  ggplot(tab_loss_cities, aes(x = year, y = chg_rel)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_point(color = "#224B5E") +
  geom_line(color = "#224B5E") +
  geom_text(
    data = filter(tab_loss_cities, year == 2022),
    aes(x = year, y = chg_rel, label = round(chg_rel * 100, 2)),
    family = "Lato",
    size = 3,
    nudge_x = 3
  ) +
  facet_wrap(vars(name_muni)) +
  scale_x_continuous(
    breaks = c(1991, 2000, 2010, 2022),
    limits = c(NA, 2027)
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Queda no crescimento em grandes cidades",
    subtitle = "Cidades de grande porte que tiveram as maiores quedas populacionais no último Censo.\nMudança percentual da população total.",
    x = NULL,
    y = NULL,
    caption = "Fonte: IBGE (CENSO, 1991, 2000, 2010, 2022)."
  ) +
  theme_vini


# Censo 2022

p_pop


plot_cities_loss

plot_growth

gt_growth

# Crescimento populacional das cidades por porte (tamanho)

gt(tab_growth_cities, id = "table_growth")

tab_growth_cities %>%
  gt(id = "table_growth") %>%
  cols_label(.list = col_names) %>%
  fmt_number(3:4, decimals = 0, sep_mark = ".") %>%
  fmt_percent(5) %>%
  tab_caption("Crescimento populacional das cidades por porte.") %>%
  opt_table_font(font = list(google_font("Lato"))) %>%
  opt_row_striping() %>%
  opt_all_caps() %>%
  tab_options(
    table.border.top.width = px(3),
    table.font.size = 12
  )

col_names <- c(
  "Porte", "Habitantes", "Número\nde\ncidades", "Ganho pop.", "Ganho pop.\nMédio (*)"
)

names(col_names) <- names(tab_growth_cities)

tab_growth_cities %>%
  mutate(share = total_growth / sum(total_growth) * 100)

tab_growth_cities %>%
  gt(id = "table_growth") %>%
  cols_label(.list = col_names) %>%
  fmt_number(3:4, decimals = 0, sep_mark = ".") %>%
  fmt_percent(5) %>%
  tab_footnote("Fonte: REstate Insights | IBGE (Censo 2022).") %>%
  tab_caption("Crescimento populacional das cidades por porte.") %>%
  opt_table_font(font = list(google_font("Lato"))) %>%
  opt_row_striping(row_striping = TRUE) %>%
  opt_all_caps() %>%
  tab_header(title = "Cidades de médio porte concentraram ") %>%
  tab_options(
    # Table structure
    table.border.top.width = px(3),
    table.border.top.color = "#1f4e79",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#1f4e79",
    table.font.size = 12,
    table.font.color = "#2c3e50",
    table.background.color = "#ffffff",
  
    # Header styling
    column_labels.background.color = "#1f4e79",
    column_labels.font.weight = "bold",
    column_labels.font.size = 13,
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#ffffff",
    
    # Row striping
    row.striping.background_color = "#f8f9fa",
    row.striping.include_table_body = TRUE,
    
    # Data rows
    data_row.padding = px(8),

  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())) %>%
  # Add subtle cell borders
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "#e8e8e8",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  
  # Highlight the growth percentage column with conditional formatting
  # tab_style(
  #   style = list(
  #     cell_fill(color = "#e8f4f8"),
  #     cell_text(weight = "bold", color = "#1f4e79")
  #   ),
  #   locations = cells_body(columns = 5)  # Assuming column 5 is the percentage
  # ) %>%
  
  # Style the header row
  tab_style(
    style = cell_text(
      font = google_font("Lato"),
      weight = "bold",
      size = px(13)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Add hover effect (if viewing in HTML)
  opt_css(
    css = "
    #table_growth .gt_row:hover {
      background-color: #f0f7ff;
      transition: background-color 0.2s ease;
    }
    
    #table_growth .gt_caption {
      text-align: left;
      padding-bottom: 10px;
    }
    "
  )
  
  # McKinsey-style color scheme and formatting
  tab_options(
    # Table structure
    table.border.top.width = px(3),
    table.border.top.color = "#1f4e79",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "#1f4e79",
    table.font.size = 12,
    table.font.color = "#2c3e50",
    table.background.color = "#ffffff",
    
    # Header styling
    column_labels.background.color = "#1f4e79",
    column_labels.font.color = "#ffffff",
    column_labels.font.weight = "bold",
    column_labels.font.size = 13,
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "#ffffff",
    
    # Row striping
    row.striping.background_color = "#f8f9fa",
    row.striping.include_table_body = TRUE,
    
    # Data rows
    data_row.padding = px(8),
    
    # Caption
    table_caption.font.size = 14,
    table_caption.font.color = "#1f4e79",
    table_caption.font.weight = "bold"
  ) %>%
  
  # Add subtle cell borders
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "#e8e8e8",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  
  # Highlight the growth percentage column with conditional formatting
  tab_style(
    style = list(
      cell_fill(color = "#e8f4f8"),
      cell_text(weight = "bold", color = "#1f4e79")
    ),
    locations = cells_body(columns = 5)  # Assuming column 5 is the percentage
  ) %>%
  
  # Style the header row
  tab_style(
    style = cell_text(
      font = google_font("Lato"),
      weight = "bold",
      size = px(13)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Add hover effect (if viewing in HTML)
  opt_css(
    css = "
    #table_growth .gt_row:hover {
      background-color: #f0f7ff;
      transition: background-color 0.2s ease;
    }
    
    #table_growth .gt_caption {
      text-align: left;
      padding-bottom: 10px;
    }
    "
  )


