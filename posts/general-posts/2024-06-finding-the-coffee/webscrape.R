library(purrr)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(rvest)
library(gt)
library(gtExtras)
library(sf)
library(leaflet)
library(googleway)

import::from(tidygeocoder, geocode)
import::from(purrr, map, map2)
import::from(tidyr, unnest)


# Base url 
base_url <- "https://thecoffee.jp/shortcut/brasil"
# Parse HTML
page_list_brazil <- xml2::read_html(base_url)

page_list_cities <- page_list_brazil |> 
  html_elements(xpath = "//div/ul/li/a") |> 
  html_attr("href")

page_list_cities <- page_list_cities[str_detect(page_list_cities, "brasil/")]

url_cities <- str_c(base_url, str_remove(page_list_cities, "shortcut/brasil/"))

scrape_the_coffee <- function(url) {
  
  page <- xml2::read_html(url)

  coffee_shop_name <- page |> 
    rvest::html_elements(xpath = "//div/ul/li/div/div/a/h4") |> 
    rvest::html_text()
  
  address_list <- page |> 
    rvest::html_elements(xpath = "//div/ul/li/div/div/a/p") |> 
    rvest::html_text()
  
  address_list <- address_list[!str_detect(address_list, "coming soon")]
  street_name <- address_list[seq(1, length(address_list), 2)]
  city_name <- address_list[2]
  
  full_address <- paste(street_name, city_name)

  out <- tibble::tibble(
    name = coffee_shop_name,
    address = full_address,
    street_name = street_name,
    city_name = city_name
  )
  
  return(out)

}

coffee_locations <- map(url_cities, scrape_the_coffee)
names(coffee_locations) <- url_cities

pb <- txtProgressBar(min = 1, max = length(url_cities), style = 3)
ls <- vector("list", length(url_cities))

for (i in seq_along(url_cities)) {
  
  url <- url_cities[i]
  current_city <- basename(url)
  message("Scraping data for: ", current_city)
  ls[[i]] <- scrape_the_coffee(url)
  setTxtProgressBar(pb, i)
  
}


dat <- bind_rows(coffee_locations, .id = "url")

unnabreviate <- function() {
  c("Av\\." = "Avenida",
    "Al\\." = "Alameda",
    "R\\."  = "Rua",
    "Dr\\." = "Doutor",
    "Visc\\." = "Visconde",
    "Pres\\." = "Presidente",
    "Mal\\." = "Marechal")
}

dat <- dat |> 
  mutate(
    city_name = str_remove(city_name, " - Brasil"),
    address = str_replace_all(address, unnabreviate()),
    country = "Brasil"
    )

tab_count <- dat |> 
  count(city_name, sort = TRUE) |> 
  mutate(share_brasil = n / sum(n)) |> 
  head(10)

gt(tab_count, caption = "The Coffee shops, major cities") |> 
  cols_label(
    city_name = "Cidade",
    n = "Lojas",
    share_brasil = "Share BR (%)"
  ) |> 
  fmt_percent(3) |> 
  opt_stylize(style = 6) |> 
  opt_table_font(font = google_font("Open Sans"))

coffee <- tidygeocoder::geocode(dat, address = address, method = "google")

shops <- st_as_sf(coffee, coords = c("long", "lat"), crs = 4326, remove = FALSE)

cur_shops <- filter(shops, city_name == "Curitiba")

get_ratings <- function(lat, lng) {
  
  location <- c(lat, lng)
  places <- google_places("The Coffee", location = location, radius = 10)
  res <- places$results
  
  subres <- res %>%
    unnest(cols = "geometry") %>%
    unnest(cols = "location") %>%
    select(
      business_status, name, formatted_address, rating, user_ratings_total,
      lat, lng
    )
  
}

ratings <- map2(cur_shops$lat, cur_shops$long, get_ratings)

dat_ratings <- ratings |> 
  bind_rows() |> 
  distinct() |> 
  filter(str_detect(name, "^The Coffee"))

dat_ratings <- dat_ratings |> 
  mutate(
    street_name = str_extract(formatted_address, "^[^,]+"),
    street_name = str_replace_all(street_name, unnabreviate()),
    street_name = stringi::stri_trans_general(street_name, "latin-ascii"),
    street_number = as.numeric(str_extract(formatted_address, "(?<=, )\\d+(?=\\b)"))
  )

cur_shops <- cur_shops |> 
  rename(short_address = street_name) |> 
  mutate(
    street_name = str_extract(address, "^[^,]+"),
    street_name = str_replace_all(street_name, unnabreviate()),
    street_name = stringi::stri_trans_general(street_name, "latin-ascii"),
    street_number = as.numeric(str_extract(address, "(?<=, )\\d+(?=\\b)"))
  )

cur_shops |> 
  arrange(address) |> 
  left_join(dat_ratings, by = c("street_name", "street_number")) |> 
  filter(is.na(rating))

dat_ratings$street_name

subres <- st_as_sf(subres, coords = c("lng", "lat"), crs = 4326)

# Mapa iterativo simples
leaflet(subres) |> 
  addTiles() |> 
  addCircleMarkers(label = ~name) |> 
  addProviderTiles("CartoDB")
