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
    
    census <- st_transform(census, crs = 31982)
    target <- st_transform(target, crs = 31982)
    
  }

  # Select variables
  census <- dplyr::select(census, dplyr::all_of(variables))
  # Interpolate areas
  interp <- st_interpolate_aw(census, target, extensive = TRUE)
  
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

library(tmap)
library(tmaptools)
tmap_mode("view")

code_shop <- 21

sub_shop <- filter(city_shops, shop_id == code_shop)

area <- get_buffer(sub_shop)

area_rad <- st_transform(area, crs = 4326)
area_rad <- st_make_valid(area_rad)

sub_census <- st_join(city_census_utm, area)
sub_census <- filter(sub_census, !is.na(isomin))
sub_census <- st_transform(sub_census, 4326)
sub_census <- st_make_valid(sub_census)

tm_shape(sub_shop) +
  tm_dots() +
  tm_shape(area_rad) +
  tm_polygons(alpha = 0.15, col = "darkgreen") +
  tm_shape(sub_census) +
  tm_fill(alpha = 0.5, col = "v0001", n = 3) +
  tm_borders() +
  tm_view(set.view = 14) +
  tm_basemap(server = "CartoDB.Positron")

library(ggplot2)
library(basemaps)

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

ggplot(area_rad) +
  geom_sf(alpha = 0.5, fill = "gray50") +
  geom_sf(data = sub_shop, shape = 21) +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  ) +
  scale_fill_identity()

sub_census <- st_transform(sub_census, crs = 3857)

ggplot(sub_census) +
  geom_sf(aes(fill = v0001)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  )

inter_areas <- st_transform(inter_areas, crs = 3857)

sub_census %>%
  mutate(area_total = st_area(.))

inter_areas %>%
  mutate(area_final = st_area(.))


ggplot(inter_areas) +
  geom_sf(aes(fill = v0001)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  coord_sf(
    xlim = c(limits_map[1], limits_map[3]),
    ylim = c(limits_map[2], limits_map[4])
  )
