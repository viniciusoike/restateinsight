library(ggplot2)
library(sf)
library(dplyr)
library(ggimage)
library(showtext)
library(leaflet)
library(gt)
library(gtExtras)
import::from(here, here)

get_isochrone <- function(point) {
  
  iso <- osrm::osrmIsochrone(point, breaks = 10, osrm.profile = "foot")
  iso <- sf::st_make_valid(iso)
  
  return(iso)
  
}

get_buffer_zone <- function(point, dist = NULL, add_id = TRUE) {
  
  stopifnot(is.numeric(dist) && dist > 0)
  
  buffer <- point %>%
    sf::st_transform(crs = 31983) %>%
    sf::st_buffer(dist = dist) %>%
    sf::st_transform(crs = 4326)
  
  if (add_id) {
    buffer <- dplyr::mutate(buffer, gid = dplyr::row_number())
  }
  
  return(buffer)
  
}

sysfonts::font_add("HelveticaNeue", "HelveticaNeue.ttc")
showtext_auto()

# Data --------------------------------------------------------------------

spo_setores <- st_read(
  here("static/data/census/spo_setores_2022.gpkg"),
  quiet = TRUE
)

spo_setores_2010 <- st_read(
  here("static/data/census/spo_setores_2010.gpkg"),
  quiet = TRUE
)

stations <- st_read(
  here("static/data/spo_metro_stations.gpkg"),
  quiet = TRUE
)

# Stations ----------------------------------------------------------------

sta_line_4 <- stations %>%
  filter(line_number == 4)

leaflet() %>%
  addTiles() %>%
  addMarkers(data = sta_line_4)

# Example -----------------------------------------------------------------

## Station -----------------------------------------------------------------
oscar <- sta_line_4 %>%
  filter(station_name == "Oscar Freire")

## Buffers and Data --------------------------------------------------------

# Buffers

oscar_iso <- get_isochrone(oscar)
oscar_buff_400 <- get_buffer_zone(oscar, dist = 400)
oscar_buff_800 <- get_buffer_zone(oscar, dist = 800)

# Census

oscar_setores_antigo <- spo_setores_2010 %>%
  st_transform(crs = 4326) %>%
  st_join(oscar_buff_800) %>%
  filter(!is.na(gid))

oscar_setores_800 <- spo_setores %>%
  st_transform(crs = 4326) %>%
  st_join(oscar_buff_800) %>%
  filter(!is.na(gid))

censobr::data_dictionary(2010, "population")

census10 <- read_tracts(year = 2010, dataset = "Basico")

spo_census10 <- census10 %>%
  filter(Cod_municipio == "3550308") %>%
  # Pop is actually number of dwellers in private households
  select(code_tract, dom_prt = V001, pop = V002) %>%
  collect()

spo_setores_2010 <- left_join(spo_setores_2010, spo_census10, by = "code_tract")

### Map ---------------------------------------------------------------------

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = oscar_setores_800,
    weight = 1,
    color = "black"
    ) %>%
  addPolygons(
    data = oscar_buff_800,
    weight = 2,
    color = "#e76f51",
    fillOpacity = 0.5,
    label = "800m"
  ) %>%
  addPolygons(
    data = oscar_buff_400,
    weight = 2,
    color = "#e76f51",
    fillOpacity = 0.5,
    label = "400m"
  ) %>%
  addMarkers(
    data = oscar,
    label = "Oscar Freire"
  )

### Map Intersection --------------------------------------------------------

# Intermediate step maps

inter_800 <- st_intersection(oscar_setores_800, oscar_buff_800)
inter_400 <- st_intersection(oscar_setores_800, oscar_buff_400)
inter_iso <- st_intersection(st_transform(spo_setores, 4326), oscar_iso)

# Import subway station icon
oscar$image <- here("static/images/icons/subway.png")

# Define lng and lat to position the icon
oscar <- oscar %>%
  mutate(lng = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

ggplot(oscar) +
  geom_sf(data = inter_400, aes(fill = code_tract), color = "white") +
  geom_image(data = oscar, aes(x = lng, y = lat, image = image), size = 0.05) +
  scale_fill_manual(
    values = sample(MetBrewer::met.brewer("Hokusai1", n = nrow(inter_800)))
  ) +
  guides(fill = "none") +
  ggtitle("Buffer 400m") +
  theme_void(base_family = "HelveticaNeue") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22)
  )

ggplot(oscar) +
  geom_sf(data = inter_800, aes(fill = code_tract), color = "white") +
  geom_image(data = oscar, aes(x = lng, y = lat, image = image), size = 0.05) +
  scale_fill_manual(
    values = sample(MetBrewer::met.brewer("Hokusai1", n = nrow(inter_800)))
  ) +
  guides(fill = "none") +
  ggtitle("Buffer 800m") +
  theme_void(base_family = "HelveticaNeue") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22)
  )

ggplot(oscar) +
  geom_sf(data = inter_iso, aes(fill = code_tract), color = "white") +
  geom_image(data = oscar, aes(x = lng, y = lat, image = image), size = 0.05) +
  scale_fill_manual(
    values = sample(MetBrewer::met.brewer("Hokusai1", n = nrow(inter_800)))
  ) +
  guides(fill = "none") +
  ggtitle("Isochrone 10 min.") +
  theme_void(base_family = "HelveticaNeue") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22)
  )

## Census Interpolation ----------------------------------------------------

# Interpolation

get_census_data_interp_1 <- function(census, poly) {
  
  census %>%
    dplyr::select(pop, dom_prt) %>%
    sf::st_interpolate_aw(poly, extensive = TRUE) %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble()
  
}

get_census_data_interp <- function(...) {
  
  suppressWarnings(get_census_data_interp_1(...))
  
}

spo_setores <- st_transform(spo_setores, crs = 4326)
spo_setores_2010 <- st_transform(spo_setores_2010, crs = 4326)

## Summarize ---------------------------------------------------------------

station_summary <- function(census_old, census_new, station) {
  
  message("Creating buffer zones")
  
  buff_400 <- get_buffer_zone(station, dist = 400)
  buff_800 <- get_buffer_zone(station, dist = 800)
  iso <- get_isochrone(station)
  
  params_old <- tibble(
    type = c("Buffer 400m", "Buffer 800m", "Isochrone 10 min."),
    census = list(census_old),
    poly = list(buff_400, buff_800, iso)
  )
  
  params_new <- tibble(
    type = c("Buffer 400m", "Buffer 800m", "Isochrone 10 min."),
    census = list(census_new),
    poly = list(buff_400, buff_800, iso)
  )
  
  message("Performing intersection with census tracts (2010)")
  
  tab_old <- params_old |> 
    mutate(dat = purrr::map2(census, poly, get_census_data_interp)) %>%
    select(type, dat) %>%
    tidyr::unnest(dat)
  
  message("Performing intersection with census tracts (2022)")
  
  tab_new <- params_new |> 
    mutate(dat = purrr::map2(census, poly, get_census_data_interp)) %>%
    select(type, dat) %>%
    tidyr::unnest(dat)
  
  tab <- bind_rows(list(old = tab_old, new = tab_new), .id = "source")
  
  tab <- tab %>%
    mutate(chg_pop = pop / lag(pop) - 1,
           chg_dom = dom_prt / lag(dom_prt) - 1,
           .by = "type")
  
  return(tab)
  
}

## Oscar Freire ------------------------------------------------------------

station_summary(spo_setores_2010, spo_setores, oscar)

# Buffer Stations ---------------------------------------------------------

parallel::mclapply(
  split(stations, stations$line_name_pt),
  \(station) station_summary(spo_setores_2010, spo_setores, station)
)

split(stations, stations$line_name_pt)

# params <- tibble(
#   type = c("Buffer 400m", "Buffer 800m", "Isochrone 10 min."),
#   census = list(spo_setores),
#   poly = list(oscar_buff_400, oscar_buff_800, oscar_iso)
# )
# 
# tab <- params |> 
#   mutate(dat = purrr::map2(census, poly, get_census_data_interp)) %>%
#   select(type, dat) %>%
#   tidyr::unnest(dat)

gt(tab)
