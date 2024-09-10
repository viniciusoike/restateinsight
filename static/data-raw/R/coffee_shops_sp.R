library(tidyverse)
library(tidygeocoder)
library(osmdata)
library(rvest)
library(geobr)
library(sf)

url = "https://thecoffee.jp/shortcut/brasil/sao-paulo"

node = url |> 
  read_html() |> 
  html_elements(xpath = '//div[@class="styles_texts__tQR1s"]')

name_store = node |> 
  html_elements(xpath = '//div/a/h4') |> 
  html_text()

address_stores = node |> 
  html_elements(xpath = '//div/a/p') |> 
  html_text()

address_stores = address_stores[!str_detect(address_stores, "coming soon")]

the_coffee = tibble(
  name = name_store,
  address = address_stores
)

the_coffee = the_coffee |> 
  mutate(
    street_name = str_extract(address, "[A-z].+(?=,)"),
    street_number = as.numeric(str_extract(address, "(?<= )\\d+(?= )")),
    address_1 = str_glue("The Coffee, {street_name}, {street_number}, São Paulo, Brasil")
  )

the_coffee = ggmap::mutate_geocode(the_coffee, address_1)





# Get all coffee shops in São Paulo

border = read_municipality(code_muni = 3550308)
bbox = st_bbox(border)
qr = opq(bbox)

qr_amenities = add_osm_feature(qr, key = "amenity")

list_amenities = osmdata::available_tags("amenity")
qr_cafeterias = add_osm_feature(qr, key = "amenity", value = "cafe")

cafeterias = osmdata_sf(qr_cafeterias)

cafe = cafeterias$osm_points

library(dbscan)

points <- st_coordinates(cafe)

clustering <- dbscan(points, eps = 0.02, minPts = 5)

clustering


# Maybe take these as well...
cafeterias$osm_polygons |> 
  st_centroid()

mapview::mapview(cafe)

mapview::mapview(border)

cafe |> 
  pull(name) |> 
  unique()

pattern = "(Fran's Cafe)|(Gocoffee)|(Casa Bauducco)|(Fran's)|(Havanna)|(Gran Expresso)|(Ofner)|(Go Coffee)|(Rei do Mate)|(Cheirin Bão)|(Franz Café)"

cafe |> 
  filter(str_detect(name, pattern)) |> 
  mapview::mapview()
