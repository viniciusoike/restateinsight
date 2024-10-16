# Load required packages
library(sf)
library(ggplot2)
library(patchwork)
library(dplyr)
library(data.table)
import::from(RColorBrewer, brewer.pal)
import::from(janitor, clean_names)
import::from(here, here)
import::from(geobr, read_census_tract)
import::from(osrm, osrmIsochrone)
import::from(nngeo, st_remove_holes)

# Define a city code
city_code <- 4106902

## Import Census Data ------------------------------------------------------

# Path to census data
census_path <- "/Volumes/T7 Touch/github/tidyibge/data-raw/dados_setores_censitarios/Agregados_preliminares_por_setores_censitarios_BR.zip"

# Import and clean census data
city_census_data <- fread(census_path) %>% 
  clean_names() %>% 
  filter(cd_mun == city_code) %>%
  mutate(code_tract = as.numeric(stringr::str_remove(cd_setor, "P$")))

# Download census tract shape using geobr
city_census <- read_census_tract(code_tract = city_code, year = 2022, showProgress = FALSE) %>%
  left_join(city_census_data, by = "code_tract")

## Import Shops Data -------------------------------------------------------

# Function to create isochrones
create_isochrone_buffer <- function(point, radius = 5) {
  
  point %>% 
    st_transform(crs = 31982) %>% 
    osrmIsochrone(breaks = radius, osrm.profile = "foot") %>%
    st_remove_holes()
    
}

# Read shop data
city_shops <- st_read(here("static/data/curitiba_tcf_interpolate.gpkg"), quiet = TRUE)

# Subset the selected shop
selected_shop <- city_shops %>%
  filter(shop_id == 21)

# Create a buffered area around the selected shop
buffered_area <- create_isochrone_buffer(selected_shop)

# Transform the buffered area to the correct CRS
buffered_area_transformed <- buffered_area %>%
  st_transform(crs = 3857) %>%
  st_make_valid()

# Prepare census data within the buffered area
city_census_utm <- st_transform(city_census, crs = 31982)

# Join census data with the buffered area
sub_census_data <- city_census_utm %>%
  st_join(buffered_area) %>%
  filter(!is.na(isomin)) %>%
  st_transform(crs = 3857) %>%
  st_make_valid()

# Perform intersection with areas
intersected_areas <- st_intersection(buffered_area_transformed, sub_census_data)

# Calculate map limits for plotting
map_limits <- st_bbox(buffered_area_transformed) %>% 
  st_as_sfc() %>% 
  st_transform(crs = 31982) %>% 
  st_buffer(dist = 100) %>% 
  st_transform(crs = 3857) %>% 
  st_bbox()

# Define breaks for population categories
population_breaks <- c(100, 200, 300, 400, 600, Inf)

# Join and categorize census data
census_map_data <- st_join(city_census_utm, st_buffer(buffered_area, dist = 250)) %>%
  filter(!is.na(isomin)) %>%
  st_transform(crs = 3857) %>%
  mutate(cat = findInterval(v0001, population_breaks),
         color = brewer.pal(6, "Greens")[cat + 1])

# Process sub-census data
sub_census_data <- sub_census_data %>%
  mutate(cat = findInterval(v0001, population_breaks),
         color = brewer.pal(6, "Greens")[cat + 1])

# Transform intersected areas
intersected_areas <- st_transform(intersected_areas, crs = 3857)

census_total_area <- sub_census_data %>%
  mutate(area_total = as.numeric(st_area(.))) %>%
  select(code_tract, cat_census = cat, v0001, area_total) %>%
  st_drop_geometry()

# Prepare data for intersection analysis
intersection_census <- intersected_areas %>%
  mutate(area_final = as.numeric(st_area(.))) %>%
  left_join(census_total_area) %>%
  mutate(share = area_final / area_total,
         pop = v0001 * share,
         cat = findInterval(pop, population_breaks),
         color = brewer.pal(6, "Greens")[cat + 1])

# Function to add elements to map
add_map_elements <- function(plot) {
  plot_added <- plot +
    geom_sf(data = selected_shop, shape = 21) +
    scale_fill_identity() +
    coord_sf(xlim = map_limits[c(1, 3)], ylim = map_limits[c(2, 4)]) +
    theme_void() +
    theme(legend.position = "none")
  
  return(plot_added)
}

# Create plots

m0 <- ggplot() +
  geom_sf(data = buffered_area_transformed) +
  geom_sf(data = selected_shop, shape = 21) +
  theme_void()

plot_census_map <- ggplot() +
  geom_sf(data = census_map_data, aes(fill = color), color = "white")

m1 <- add_map_elements(plot_census_map)

plot_sub_census <- ggplot() +
  geom_sf(data = sub_census_data, aes(fill = color), color = "white") +
  geom_sf(data = buffered_area_transformed, alpha = 0.5, fill = "gray50")

m2 <- add_map_elements(plot_sub_census)

plot_intersection_census <- ggplot() +
  geom_sf(data = intersection_census, aes(fill = color), color = "white") +
  geom_sf(data = buffered_area_transformed, alpha = 0)

m3 <- add_map_elements(plot_intersection_census)
