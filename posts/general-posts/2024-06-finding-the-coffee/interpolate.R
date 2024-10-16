library(sf)
library(dplyr)

# Define a city
code_city <- 4106902

## Import Census -----------------------------------------------------------

# Path to data
path_census <- "/Volumes/T7 Touch/github/tidyibge/data-raw/dados_setores_censitarios/Agregados_preliminares_por_setores_censitarios_BR.zip"
# Use data.table::fread to import csv
census <- data.table::fread(path_census)
# Clean column names and subset data only for the chosen city
city_census_dt <- census |> 
  janitor::clean_names() |> 
  dplyr::filter(cd_mun == code_city)
# Adjust code tract 
city_census_dt <- city_census_dt |> 
  mutate(code_tract = as.numeric(stringr::str_remove(cd_setor, "P$")))

# Download census tract shape using geobr
city_tract <- geobr::read_census_tract(code_tract = code_city, year = 2022)

# Join census tract shapefile with census data
city_census <- left_join(city_tract, city_census_dt, by = "code_tract")

## Import Shops ------------------------------------------------------------

# Imports all The Coffee shops (target)
shops <- st_read(here::here("static/data/coffeeshops_the_coffee.gpkg"))
# Select only shops in the selected city
city_shops <- shops |> 
  filter(city_name == unique(city_census$nm_mun))

# Interpolation -------------------------------------------------------------

## Functions ---------------------------------------------------------------

# Creates a n minute walk isochrone around a point
get_buffer <- function(point, radius = 5, simplified = FALSE) {
  
  stopifnot(length(radius) == 1 && is.numeric(radius))
  
  if (simplified) {
    point |> 
      sf::st_transform(crs = 31982) |> 
      sf::st_buffer(dist = ((1.5 - 1.2) / 2 + 1.2) * 60 * radius)
  } else {
    point |> 
      sf::st_transform(crs = 31982) |> 
      osrm::osrmIsochrone(breaks = radius, osrm.profile = "foot") |> 
      nngeo::st_remove_holes()
  }
  
}

# Interpolates an area with census tracts and aggregates population and households
interpolate_census <- function(census, target, variables = c("v0001", "v0003")) {

  if (st_crs(census) != st_crs(target)) {
    warning("CRS mismatch")
    
    census <- sf::st_transform(census, crs = 31982)
    target <- sf::st_transform(target, crs = 31982)
    
  }

  # Select variables
  census <- dplyr::select(census, dplyr::all_of(variables))
  # Interpolate areas
  interp <- sf::st_interpolate_aw(census, target, extensive = TRUE)
  
  return(interp)
  
}

# Wrapper around get_buffer and interpolate_census
find_population <- function(shop, census, radius = 5, simplified = FALSE) {
  
  # Compute a 5-minute isochrone around
  buffer <- get_buffer(shop, radius, simplified)
  interpolated <- suppressWarnings(interpolate_census(census, buffer))
  
  return(interpolated)
  
}

## Interpolate -------------------------------------------------------------

# Uniquely identifies each shop
city_shops <- city_shops |> 
  mutate(shop_id = row_number())

# To improve speed convert the full census data to 31982
city_census_utm <- st_transform(city_census, crs = 31982)

city_shops_census <- parallel::mclapply(
  split(city_shops, city_shops$shop_id),
  \(x) find_population(x, census = city_census_utm)
  )

city_shops_census <- bind_rows(city_shops_census, .id = "shop_id")

city_shops <- city_shops |> 
  mutate(shop_id = as.character(shop_id)) |> 
  left_join(st_drop_geometry(city_shops_census))

st_write(city_shops, here::here("static/data/curitiba_tcf_interpolate.gpkg"))

city_shops <- st_read(here::here("static/data/curitiba_tcf_interpolate.gpkg"))


code_shop <- 21

sub_shop <- filter(city_shops, shop_id == code_shop)

area <- get_buffer(sub_shop)

area_rad <- st_transform(area, crs = 4326)
area_rad <- st_make_valid(area_rad)

census_map <- st_join(city_census_utm, st_buffer(area, dist = 250))

sub_census <- st_join(city_census_utm, area)
sub_census <- filter(sub_census, !is.na(isomin))
sub_census <- st_transform(sub_census, 4326)
sub_census <- st_make_valid(sub_census)

# library(tmap)
# library(tmaptools)
# tmap_mode("view")
# 
# tm_shape(sub_shop) +
#   tm_dots() +
#   tm_shape(area_rad) +
#   tm_polygons(alpha = 0.15, col = "darkgreen") +
#   tm_shape(sub_census) +
#   tm_fill(alpha = 0.5, col = "v0001", n = 3) +
#   tm_borders() +
#   tm_view(set.view = 14) +
#   tm_basemap(server = "CartoDB.Positron")

library(ggplot2)
library(basemaps)
library(patchwork)

inter_areas <- st_intersection(area_rad, sub_census)

limits_map <- area_rad |> 
  st_bbox() |> 
  st_as_sfc() |> 
  st_transform(crs = 31982) |> 
  st_buffer(dist = 100) |> 
  st_transform(crs = 3857) |> 
  st_bbox()

ext <- st_as_sfc(limits_map)
ext <- st_transform(ext, crs = 3857)

area_rad <- st_transform(area_rad, crs = 3857)

breaks <- c(100, 200, 300, 400, 600, Inf)

census_map <- census_map %>%
  st_transform(crs = 3857) %>%
  mutate(
    cat = findInterval(v0001, breaks),
    color = RColorBrewer::brewer.pal(6, "Greens")[cat + 1]
    )

sub_census <- st_transform(sub_census, crs = 3857)
sub_census <- sub_census %>%
  mutate(
    cat = findInterval(v0001, breaks),
    color = RColorBrewer::brewer.pal(6, "Greens")[cat + 1]
    )

inter_areas <- st_transform(inter_areas, crs = 3857)

s1 <- sub_census %>%
  mutate(area_total = as.numeric(st_area(.))) |> 
  select(code_tract, cat_census = cat, v0001, area_total) |> 
  st_drop_geometry()

s2 <- inter_areas %>%
  mutate(area_final = as.numeric(st_area(.))) |> 
  select(code_tract, area_final)

inter_census <- s2 |> 
  left_join(s1) |> 
  mutate(
    share = area_final / area_total,
    pop = v0001 * share,
    cat = findInterval(pop, breaks),
    color = RColorBrewer::brewer.pal(6, "Greens")[cat + 1]
    )

inter_census |> 
  select(code_tract, share, cat_census, cat) |> 
  arrange(desc(share))

theme_map <- theme_void() +
  theme(legend.position = "none")

m0 <- ggplot(area_rad) +
  geom_sf(alpha = 0.5, fill = "gray50") +
  geom_sf(data = sub_shop, shape = 21) +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  ) + 
  theme_map

m1 <- ggplot(sub_census) +
  geom_sf(aes(fill = v0001)) +
  scale_fill_fermenter(palette = "Greens", direction = 1, breaks = c(300, 400, 500, 600)) +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  )

m1 <- ggplot() +
  geom_sf(data = census_map, aes(fill = color), color = "white") +
  geom_sf(data = sub_shop, shape = 21) +
  scale_fill_identity() +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  ) +
  theme_map

m2 <- ggplot() +
  geom_sf(data = sub_census, aes(fill = v0001)) +
  geom_sf(data = area_rad, alpha = 0.5, fill = "gray50") +
  geom_sf(data = sub_shop, shape = 21) +
  scale_fill_fermenter(palette = "Greens", direction = 1, breaks = c(200, 400, 500, 600)) +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  )


m2 <- ggplot() +
  geom_sf(data = sub_census, aes(fill = color), color = "white") +
  geom_sf(data = area_rad, alpha = 0.5, fill = "gray50") +
  geom_sf(data = sub_shop, shape = 21) +
  scale_fill_identity() +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  ) +
  theme_map

m3 <- ggplot() +
  geom_sf(data = inter_census, aes(fill = pop)) +
  scale_fill_fermenter(palette = "Greens", direction = 1, breaks = c(200, 400, 500, 600)) +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  )

m3 <- ggplot() +
  geom_sf(data = inter_census, aes(fill = color), color = "white") +
  geom_sf(data = area_rad, alpha = 0) + 
  scale_fill_identity() +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  ) +
  theme_map

(m0 | m1)
(m2 | m3)

library(tmap)
library(tmaptools)
tmap_mode("view")

tm_shape(inter_census) +
  tm_fill(col = "pop", popup.vars = c("pop", "v0001"))

inter_census |> 
  filter(cat == 0)
