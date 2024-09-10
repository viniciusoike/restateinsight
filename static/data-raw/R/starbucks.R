library(dplyr)
library(tidyr)
library(readxl)
library(tidygeocoder)
library(sf)

dat <- readxl::read_excel(
  here::here("static/data/raw/starbucks_location.xlsx"),
  col_names = "text")

starbucks <- dat |> 
  mutate(id = rep(seq(1, nrow(dat) / 3), each = 3)) |> 
  summarise(full_text = paste(text, collapse = "\n"), .by = "id") |> 
  separate(full_text, into = c("name", "street_number", "city"), sep = "\n") |> 
  mutate(
    address = paste(street_number, city, sep = ","),
    country = "Brazil"
    )

geo <- geocode(starbucks, street = street_number, city = city, country = country)

manual_geo <- tribble(
  ~id, ~lat, ~long,
  13, -23.546743258147007, -46.652014307352424,
  45, -23.488326221992324, -46.5488218326831,
  69, -23.025510543823263, -46.73839079518286,
  83, -22.90943409306036, -45.3178457954662
)

manual_geo <- manual_geo %>%
  left_join(st_drop_geometry(geo), by = "id", suffix = c("", "drop")) %>%
  select(-contains("drop"))

geo <- geo %>%
  filter(!(id %in% manual_geo$id)) %>%
  rbind(manual_geo) %>%
  arrange(id)

pstarbucks <- st_as_sf(geo, coords = c("long", "lat"), crs = 4326, remove = FALSE)

pstarbucks <- pstarbucks %>%
  mutate(
    address = stringr::str_replace(address, ",([A-Za-z])", ", \\1"),
    address = stringr::str_trim(address)
  )

qs::qsave(pstarbucks, here::here("static/data/starbucks_locations_sp.qs"))

# library(osrm)
# 
# test = osrmIsochrone(loc = cafeterias[1, ], breaks = seq(0, 10, 5), osrm.profile = "foot")
# 
# cafeterias <- as_tibble(cafeterias)
# 
# caf <- cafeterias |> 
#   slice(1:10) |> 
#   mutate(gid = 1:10) |> 
#   group_by(gid) |> 
#   nest()
# 
# get_walking_isochrone <- function(x) {
#   osrmIsochrone(loc = x, breaks = seq(0, 10, 5), osrm.profile = "foot")
#   }
# 
# caf <- caf |> 
#   mutate(iso = purrr::map(data, get_walking_isochrone))
# 
# mapview::mapview(test)
# 
# ?osrmIsochrone
