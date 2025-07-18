library(leaflet)
library(geobr)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)

import::from(sidrar, get_sidra)

clean_sidra <- function(df, name_value = "pop") {
  cols <- c("code_muni" = "municipio_codigo", "year" = "ano", x = "valor")
  names(cols)[3] <- name_value

  df %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
}

pop2022 <- get_sidra(4709, variable = 93, geo = "City")

pop_series <- get_sidra(
  136,
  variable = 93,
  period = "1991-2010",
  geo = "City",
  classific = c("c86"),
  category = list(0)
)

# Shape files

# Metro Regions
metro_region <- read_metro_area(showProgress = FALSE)
dim_metro <- as_tibble(st_drop_geometry(metro_region))
dim_metro <- select(dim_metro, code_muni, name_metro)

# Cities
cities <- read_municipality(year = 2020, showProgress = FALSE)
dim_city <- as_tibble(st_drop_geometry(cities))

# Join dimension tables
dim_city <- left_join(dim_city, dim_metro, by = "code_muni")

#> Remove an exception: Murici is mapped into two different metro regions
dim_city <- dim_city |>
  filter(!(code_muni == 2705507 & name_metro == "RM da Zona da Mata"))

cpop_series <- clean_sidra(pop_series)
cpop2022 <- clean_sidra(pop2022)

census_pop <- rbind(cpop_series, cpop2022)
census_pop <- left_join(census_pop, dim_city, by = "code_muni")

# sel_metro <- c(
#   "RM São Paulo",
#   "RM Rio de Janeiro",
#   "RM Belo Horizonte",
#   "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno",
#   "RM Porto Alegre",
#   "RM Fortaleza",
#   "RM Recife",
#   "RM Curitiba",
#   "RM Salvador",
#   "RM Campinas"
# )

tab_city_metro <- census_pop |>
  mutate(
    chg_rel = (pop / lag(pop) - 1) * 100,
    tgc = ((pop / lag(pop))^(1 / (year - lag(year))) - 1) * 100,
    .by = "code_muni"
  ) |>
  pivot_wider(
    id_cols = c("code_muni", "name_metro"),
    names_from = "year",
    values_from = c("pop", "chg_rel", "tgc")
  )

cities_metro <- inner_join(cities, tab_city_metro, by = "code_muni")

subdata <- subset(cities_metro, name_metro == "RM Belo Horizonte")

# jenks_breaks <- BAMMtools::getJenksBreaks(subdata$tgc_2022, k = 7)

jenks_breaks <- c(-0.5, -0.25, 0, 0.75, 1.5, 2.5)

legend_labels <- c(
  "-0,5% a -0,25%",
  "-0,25% a 0%",
  "0% a 0,75%",
  "0,75% a 1,5%",
  "1,5% a 2,5%",
  "> 2,5%"
)

# jenks_breaks[3] <- 0

# jenks_breaks <- c(-0.55, -0.2, 0, 0.4, 1.2, 3)

subdata <- subdata %>%
  mutate(
    class = factor(
      findInterval(tgc_2022, jenks_breaks),
      labels = legend_labels
    ),
    saldo_pop = pop_2022 - pop_2010,
    is_capital = factor(if_else(name_muni == "Belo Horizonte", 1L, 0L))
  )

RColorBrewer::brewer.pal(9, "BrBG")

color_palette <- c(
  "#8C510A",
  "#DFC27D",
  "#C7EAE5",
  "#80CDC1",
  "#35978F",
  "#01665E"
)

pal <- colorFactor(color_palette, subdata$class, levels = levels(subdata$class))

labels <- str_glue(
  "<b>Nome</b>: {nome}<br>
   <b>Pop (2022)</b>: {pop2022}<br>
   <b>Pop (2010)</b>: {pop2010}<br>
   <b>Saldo</b>: {saldo}<br>
   <b>TGC</b>: {tgc}%",
  nome = subdata$name_muni,
  pop2022 = format(round(subdata$pop_2022, -3), big.mark = "."),
  pop2010 = format(round(subdata$pop_2010, -3), big.mark = "."),
  saldo = format(round(subdata$saldo_pop, -2), big.mark = "."),
  tgc = format(round(subdata$tgc_2022, 1), decimal.mark = ",")
)


labels <- lapply(labels, htmltools::HTML)

subdata <- st_transform(subdata, crs = 4326)

map <- leaflet(subdata) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB) |>
  addPolygons(
    fillColor = ~ pal(class),
    weight = 2,
    color = "white",
    opacity = ~ ifelse(is_capital == 1, 0.95, 0.7),
    fillOpacity = ~ ifelse(is_capital == 1, 0.95, 0.6), # Slightly more opaque
    highlightOptions = highlightOptions(
      color = "#D35400",
      weight = 3,
      fillOpacity = 0.8,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", "color" = "#333333")
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~class,
    labFormat = labelFormat(digits = 1),
    title = "TGC (2022)",
    position = "bottomright",
  ) |>
  setView(lng = -43.95988, lat = -19.90272, zoom = 10)

saveRDS(
  map,
  here::here("posts/general-posts/2025-06-censo-metro-regions/leaflet_map.rds")
)

# htmlwidgets::saveWidget(
#   map,
#   here::here(
#     "posts/general-posts/2025-06-censo-metro-regions/leaflet_map.html"
#   ),
#   selfcontained = TRUE
# )

# cutoff <- subdata %>%
#   slice_max(pop_2010, n = 10) %>%
#   pull(pop_2010) %>%
#   min() %>%
#   log10()

# ggplot(subdata, aes(log10(pop_2010), tgc_2022)) +
#   geom_point(
#     aes(fill = class),
#     color = "black",
#     shape = 21,
#     size = 3) +
#   geom_hline(yintercept = 0) +
#   geom_hline(yintercept = mean(subdata$tgc_2022), linetype = 2, color = "gray30") +
#   geom_vline(xintercept = cutoff - 0.025, linetype = 1, color = "gray30") +
#   guides(fill = "none") +
#   scale_fill_brewer(
#     palette = "BrBG"
#   )

# breaks <- BAMMtools::getJenksBreaks(cities_metro$tgc_2022, k = 7)
# breaks[3] <- 0
# # labels <- format(round(breaks * 100, 2), decimal.mark = ",")
# # labels <- paste(labels, labels[-1], sep = " a ")
# # labels <- c(labels[-length(labels)], ">4,8%")

# labels <- c(
#   "-0,55 a -0,20", "-0,20 a 0,00", "0,00 a 0,81", "0,81 a  1,22",
#   "1,22 a 2,06", "2,06 a 4,79", "> 4,8%"
#   )

# cities_metro <- cities_metro |>
#   dplyr::mutate(group = factor(findInterval(tgc_2022, breaks)))

# plot_metro <- function(metro, size = 3, legend = FALSE) {

#   metro <- ifelse(!str_detect(metro, "RM"), paste("RM", metro), metro)

#   dat <- dplyr::filter(cities_metro, name_metro == metro)

#   colors <- RColorBrewer::brewer.pal(11, "BrBG")[c(2, 4, 7, 8, 9, 10, 11)]

#   plot <-
#     ggplot(dat) +
#     geom_sf(aes(fill = group), linewidth = 0.5, color = "gray50") +
#     geom_sf_label(
#       aes(label = name_muni),
#       size = size,
#       family = "Lato",
#       color = "gray20") +
#     ggtitle(label = metro) +
#     scale_fill_manual(
#       values = colors,
#       labels = labels
#     ) +
#     ggthemes::theme_map(base_family = "Lato") +
#     theme(
#       plot.title = element_text(
#         size = 18,
#         hjust = 0.5,
#         color = "gray10"
#       )
#     )

#   if (!legend) {
#     plot <- plot + theme(legend.position = "none")
#   }

#   return(plot)

# }

# plot_metro("Porto Alegre")

# shorten_ride_names <- function(x) {

#   ride_names <- c(
#     "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno" = "RIDE DF",
#     "RIDE TERESINA - Região Integrada de Desenvolvimento da Grande Teresina" = "RIDE Teresina",
#     "RIDE Petrolina/Juazeiro Região Administrativa Integrada de Desenvolvimento do Polo Petrolina/PE e Juazeiro/BA" = "RIDE Petrolina/Juazeiro"
#   )

#   unname(ride_names[x])

# }

# tbl_pop_metro <- census_pop %>%
#   filter(!is.na(name_metro)) %>%
#   mutate(
#     name_metro = str_remove(name_metro, "RM d[a-z]"),
#     name_metro = str_remove(name_metro, "RM"),
#     name_metro = str_replace(name_metro, "Aglomeração Urbana", "AU"),
#     name_metro = ifelse(
#       str_detect(name_metro, "^RIDE"),
#       shorten_ride_names(name_metro),
#       name_metro
#     ),
#     name_metro = str_trim(name_metro, side = "both")
#   ) %>%
#   summarise(
#     total = sum(pop, na.rm = TRUE),
#     state = paste(unique(abbrev_state), collapse = ", "),
#     .by = c("year", "name_metro")
#   ) %>%
#   arrange(year) %>%
#   arrange(name_metro)

# big_metros <- tbl_pop_metro %>%
#   filter(year == 2022, total > 5*1e5) %>%
#   pull(name_metro)

# # Get only 500k cities and compute average geometric growth
# tbl_major <- tbl_pop_metro %>%
#   filter(name_metro %in% big_metros) %>%
#   # Not so trivial since year intervals are not the same between Census editions
#   group_by(name_metro) %>%
#   mutate(
#     tcg = (total / lag(total))^(1/(year - lag(year) + 1)) - 1,
#     total_round = round(total / 1000)) %>%
#   ungroup()

# # Convert to wider and arrange by 2022 population
# tbl_major_wide <- tbl_major %>%
#   pivot_wider(
#     id_cols = c("name_metro", "state"),
#     names_from = "year",
#     values_from = c("total_round", "tcg")
#   ) %>%
#   select(-tcg_1991) %>%
#   arrange(desc(total_round_2022))

# # Domicílios particulares ocupados (total e média de moradores)
# domi22 <- get_sidra(4712, variable = c(381, 5930), geo = "City")

# # Domicílios particulares ocupados
# domi_series <- get_sidra(156, variable = 2048, geo = "City", period = "1991-2010")
# # Número médio de moradores (por domicílio particular ocupado)
# domi_media <- get_sidra(156, variable = 619, geo = "City", period = "1991-2010")

# big_cities <- census_pop |>
#   filter(year == 2022, pop > 8 * 1e5) |>
#   pull(code_muni)

# tab_big_cities <- census_pop |>
#   filter(code_muni %in% big_cities) |>
#   mutate(
#     chg_abs = pop - lag(pop),
#     chg_rel = pop / lag(pop) - 1,
#     .by = c("code_muni", "name_muni")
#   )

# tab_chg <- tab_big_cities |>
#   filter(year == 2022) |>
#   mutate(
#     name_muni = str_replace(name_muni, "São Bernardo Do Campo", "São Bernardo"),
#     name_muni = str_replace(name_muni, " De ", " de "),
#     name_muni = glue::glue("{name_muni} ({abbrev_state})"),
#     name_muni = factor(name_muni),
#     name_muni = forcats::fct_reorder(name_muni, chg_abs),
#     chg_label = round(chg_abs / 1000),
#     chg_label = ifelse(chg_abs > 0, paste0("+", chg_label), chg_label),
#     pos_text_y = ifelse(chg_abs > 0, chg_abs / 1000 + 20, chg_abs / 1000 - 20)
#   )

# p_pop <- ggplot(tab_chg, aes(x = name_muni, y = chg_abs / 1000)) +
#   geom_col(fill = "#1d3557") +
#   geom_text(
#     aes(x = name_muni, y = pos_text_y, label = chg_label),
#     family = "Lato",
#     size = 3) +
#   coord_flip() +
#   labs(
#     title = "Ganhos e perdas de população nas grandes cidades",
#     subtitle = "Mudança absoluta da população entre as cidades mais populosas do Brasil de 2010 a 2022.",
#     x = NULL,
#     y = "Milhares de Habitantes",
#     caption = "Fonte: CENSO 2022 (IBGE).") +
#   theme_vini

# tab_growth_cities <- tab_growth_cities |>
#   mutate(
#     name_label = c("Pequeno", "Pequeno", "Médio", "Médio", "Médio-Grande", "Mega-Cidades")
#   ) |>
#   select(name_label, labels, count, total_growth, total_wgt)

# col_names <- c(
#   "Porte", "Habitantes", "Número", "Crescimento\ntotal", "Crescimento\nMédio (*)"
# )

# names(col_names) <- names(tab_growth_cities)

# gt_growth <- tab_growth_cities %>%
#   gt() %>%
#   cols_label(.list = col_names) %>%
#   fmt_number(3:4, decimals = 0, sep_mark = ".") %>%
#   fmt_percent(5) %>%
#   tab_caption("Crescimento populacional das cidades por porte.") %>%
#   opt_stylize(style = 6)

# cities_loss <- tab_cities |>
#   slice_min(chg_abs, n = 9) |>
#   pull(code_muni)

# tab_losses <- census_pop |>
#   filter(code_muni %in% cities_loss) |>
#   mutate(
#     chg_abs = pop - lag(pop),
#     chg_rel = pop / lag(pop) - 1,
#     .by = c("code_muni", "code_region")
#   ) |>
#   mutate(
#     name_muni = if_else(name_muni == "Rio De Janeiro", "Rio de Janeiro", name_muni)
#   )

# ggplot(tab_losses, aes(x = year, y = chg_rel, fill = as.factor(code_region))) +
#   geom_col() +
#   geom_hline(yintercept = 0) +
#   facet_wrap(vars(name_muni)) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   scale_fill_manual(values = c("#77ABBD", "#3B6A8C", "#A8DCCC", "#1D3557")) +
#   labs(
#     title = "População Caindo",
#     subtitle = "Variação relativa da população entre os períodos de Censo",
#     x = NULL,
#     y = "%",
#     caption = "Fonte: Censo 1991, 2000, 2010, 2022 (IBGE)."
#   ) +
#   theme_vini +
#   theme(
#     legend.position = "none",
#     panel.grid.major.x = element_blank(),
#     strip.background = element_rect(fill = NA, color = "gray20")
#   )

# tab_states <- census_pop |>
#   summarise(
#     total = sum(pop, na.rm = TRUE),
#     .by = c("year", "code_state", "name_state")
#   ) |>
#   arrange(code_state) |>
#   mutate(
#     chg_abs = total - lag(total),
#     chg_rel = total / lag(total) - 1,
#     tgc = (total / lag(total))^(1/(year - lag(year))) - 1,
#     tgc = tgc * 100,
#     .by = c("code_state")
#   )

# chg_state <- filter(tab_states, year == 2022)

# shp <- read_state(showProgress = FALSE)

# state_chg <- left_join(shp, chg_state, by = c("code_state"))

# state_chg <- state_chg %>%
#   mutate(
#     label = format(round(tgc, 2), decimal.mark = ","),
#     label = paste0(label, "%"),
#     x = as.numeric(st_coordinates(st_centroid(.))[, 1]),
#     y = as.numeric(st_coordinates(st_centroid(.))[, 2]),
#     repel = if_else(code_state %in% c(24:28, 53), 1L, 0L),
#     group = findInterval(tgc, c(0.015, 0.5, 1, 1.5, 2))
#   )

# ggplot(state_chg) +
#   geom_sf(aes(fill = as.factor(group))) +
#   geom_label(
#     data = filter(state_chg, repel == 0),
#     aes(x = x, y = y, label = label),
#     size = 3,
#     family = "Lato"
#   ) +
#   ggrepel::geom_label_repel(
#     data = filter(state_chg, repel == 1),
#     aes(x = x, y = y, label = label),
#     size = 3,
#     family = "Lato",
#     nudge_x = 3
#   ) +
#   scale_fill_manual(
#     name = " 2010/2022 (%)",
#     values = RColorBrewer::brewer.pal(6, "PuBuGn")[2:6],
#     labels = c("<0,15%", "0,15-0,5%", "0,5-1%", "1-2%", ">2%")
#   ) +
#   coord_sf(xlim = c(NA, -35)) +
#   ggthemes::theme_map(base_family = "Lato")

# ggplot(state_chg) +
#   geom_sf(aes(fill = tgc)) +
#   geom_label(
#     data = filter(state_chg, repel == 0),
#     aes(x = x, y = y, label = label),
#     size = 3,
#     family = "Lato"
#   ) +
#   ggrepel::geom_label_repel(
#     data = filter(state_chg, repel == 1),
#     aes(x = x, y = y, label = label),
#     size = 3,
#     family = "Lato",
#     nudge_x = 3
#   ) +
#   scale_fill_manual(
#     name = " 2010/2022 (%)",
#     values = RColorBrewer::brewer.pal(6, "PuBuGn")[2:6],
#     labels = c("<2,5%", "2,5-5%", "5-10%", "10-20%", ">20%")
#   ) +
#   coord_sf(xlim = c(NA, -35)) +
#   ggthemes::theme_map(base_family = "Lato")

# sel_metro <- c(
#   "RM São Paulo",
#   "RM Rio de Janeiro",
#   "RM Belo Horizonte",
#   "RIDE - Região Integrada de Desenvolvimento do Distrito Federal e Entorno",
#   "RM Porto Alegre",
#   "RM Fortaleza",
#   "RM Recife",
#   "RM Curitiba",
#   "RM Salvador",
#   "RM Campinas"
# )

# sel_city_code <- metro_region |>
#   st_drop_geometry() |>
#   filter(name_metro %in% sel_metro) |>
#   pull(code_muni)

# tab_city_metro <- census_pop |>
#   filter(code_muni %in% sel_city_code) |>
#   mutate(
#     chg_rel = pop / lag(pop) - 1,
#     tgc = (pop / lag(pop))^(1/(year - lag(year))) - 1,
#     .by = "code_muni"
#   ) |>
#   pivot_wider(
#     id_cols = c("code_muni", "name_metro"),
#     names_from = "year",
#     values_from = c("pop", "chg_rel", "tgc")
#   )

# cities_metro <- inner_join(cities, tab_city_metro, by = "code_muni")

# breaks <- BAMMtools::getJenksBreaks(cities_metro$tgc_2022, k = 7)
# breaks[3] <- 0
# # labels <- format(round(breaks * 100, 2), decimal.mark = ",")
# # labels <- paste(labels, labels[-1], sep = " a ")
# # labels <- c(labels[-length(labels)], ">4,8%")

# labels <- c(
#   "-0,55 a -0,20", "-0,20 a 0,00", "0,00 a 0,81", "0,81 a  1,22",
#   "1,22 a 2,06", "2,06 a 4,79", "> 4,8%"
#   )

# cities_metro <- cities_metro |>
#   dplyr::mutate(group = factor(findInterval(tgc_2022, breaks)))

# plot_metro <- function(metro, size = 3, legend = FALSE) {

#   metro <- ifelse(!str_detect(metro, "RM"), paste("RM", metro), metro)

#   dat <- dplyr::filter(cities_metro, name_metro == metro)

#   colors <- RColorBrewer::brewer.pal(11, "BrBG")[c(2, 4, 7, 8, 9, 10, 11)]

#   plot <-
#     ggplot(dat) +
#     geom_sf(aes(fill = group), linewidth = 0.5, color = "gray50") +
#     geom_sf_label(
#       aes(label = name_muni),
#       size = size,
#       family = "Lato",
#       color = "gray20") +
#     ggtitle(label = metro) +
#     scale_fill_manual(
#       values = colors,
#       labels = labels
#     ) +
#     ggthemes::theme_map(base_family = "Lato") +
#     theme(
#       plot.title = element_text(
#         size = 18,
#         hjust = 0.5,
#         color = "gray10"
#       )
#     )

#   if (!legend) {
#     plot <- plot + theme(legend.position = "none")
#   }

#   return(plot)

# }

# plot_metro("Curitiba")

# plot_metro("Porto Alegre")

# cities_shrink <- tab_big_cities |>
#   filter(year == 2022, chg_rel < 0) |>
#   pull(code_muni)

# tab_loss_cities <- tab_big_cities |>
#   filter(code_muni %in% cities_shrink, !is.na(chg_rel)) |>
#   mutate(is_growth = factor(if_else(chg_rel > 0, 1, 0)))

# plot_cities_loss <-
#   ggplot(tab_loss_cities, aes(x = year, y = chg_rel)) +
#   geom_hline(yintercept = 0, linewidth = 0.4) +
#   geom_point(color = "#224B5E") +
#   geom_line(color = "#224B5E") +
#   geom_text(
#     data = filter(tab_loss_cities, year == 2022),
#     aes(x = year, y = chg_rel, label = round(chg_rel * 100, 2)),
#     family = "Lato",
#     size = 3,
#     nudge_x = 3
#   ) +
#   facet_wrap(vars(name_muni)) +
#   scale_x_continuous(
#     breaks = c(1991, 2000, 2010, 2022),
#     limits = c(NA, 2027)
#   ) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   labs(
#     title = "Queda no crescimento em grandes cidades",
#     subtitle = "Cidades de grande porte que tiveram as maiores quedas populacionais no último Censo.\nMudança percentual da população total.",
#     x = NULL,
#     y = NULL,
#     caption = "Fonte: IBGE (CENSO, 1991, 2000, 2010, 2022)."
#   ) +
#   theme_vini

# tab_growth_cities %>%
#   gt(id = "table_growth") %>%
#   cols_label(.list = col_names) %>%
#   fmt_number(3:4, decimals = 0, sep_mark = ".") %>%
#   fmt_percent(5) %>%
#   tab_caption("Crescimento populacional das cidades por porte.") %>%
#   opt_table_font(font = list(google_font("Lato"))) %>%
#   opt_row_striping() %>%
#   opt_all_caps() %>%
#   tab_options(
#     table.border.top.width = px(3),
#     table.font.size = 12
#   )
