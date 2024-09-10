library(dplyr)
library(stringr)
library(tidygeocoder)
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
