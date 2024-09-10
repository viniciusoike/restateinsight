# Clean metro station and line data
# Downloaded from https://geosampa.prefeitura.sp.gov.br/

# Libraries and functions
library(sf)

import::from(dplyr, rename, mutate, tibble, filter, left_join, bind_rows)
import::from(stringr, str_to_title, str_replace)
import::from(here, here)

# Dimension table with name/numbers of each line
dim_line <- tibble(
  line_name_pt = c(
    "Azul", "Verde", "Vermelha",  "Amarela", "Lilás", "Laranja", "Prata",
    "Violeta", "Ouro", "Celeste", "Rosa", "Marrom"),
  line_name = c(
    "Blue", "Green", "Red", "Yellow", "Lilac", "Orange", "Silver", "Violet", 
    "Gold", "Sky Blue", "Pink", "Brown"),
  line_number = c(1L, 2L, 3L, 4L, 5L, 6L, 15L, 16L, 17L, 19L, 20L, 22L)
)

# Vector to standardize column names
swap_names <- c(
  "line_name_pt" = "lmtp_nome",
  "line_name_pt" = "lmt_nome",
  "line_name_pt" = "emt_linha",
  "line_name_pt" = "emtp_linha",
  "line_full_name" = "lmt_linom",
  "line_full_name" = "lmtp_linom",
  "line_company" = "lmt_empres",
  "line_company" = "lmtp_empre",
  "line_company" = "emt_empres",
  "line_company" = "emtp_empre",
  "line_number" = "lmt_linha",
  "line_number" = "lmtp_linha",
  "status" = "emt_situac",
  "status" = "emtp_situa",
  "station_name" = "emt_nome",
  "station_name" = "emtp_nome",
  "station_name_abbrev" = "emtp_sigla"
)

# Cleans metro line data
clean_metro_line <- function(dat) {
  
  dat |> 
    # Rename columns
    dplyr::rename(dplyr::any_of(swap_names)) |> 
    # Drop line_name column
    dplyr::select(-line_name_pt) |> 
    # Fix company name
    dplyr::mutate(
      line_company = dplyr::case_when(
        line_number == 4 ~ "ViaQuatro",
        line_number == 5 ~ "ViaMobilidade",
        TRUE ~ "Metrô")
      ) |> 
    # Joins with dimension table
    dplyr::left_join(dim_line, by = c("line_number"))
  
}

clean_metro_station <- function(dat) {
  
  dat |> 
    # Rename columns
    dplyr::rename(dplyr::any_of(swap_names)) |> 
    dplyr::mutate(
      # Fix line name and station name
      line_name_pt = stringr::str_to_title(line_name_pt),
      line_name_pt = stringr::str_replace(line_name_pt, "Lilas", "Lilás"),
      station_name = stringr::str_to_title(station_name),
    ) |> 
    # Joins with dimension table
    dplyr::left_join(dim_line, by = "line_name_pt") |> 
    dplyr::mutate(
      # Fix company name
      line_company = dplyr::case_when(
        line_number == 4 ~ "ViaQuatro",
        line_number == 5 ~ "ViaMobilidade",
        TRUE ~ "Metrô")
    )
  
}

# Import and clean --------------------------------------------------------

# Define directory
dir <- here("static/data/raw/geosampa")

# Imports subway line data
metro_linha <- st_read(here(dir, "geosampa_metro_linha.gpkg"))
metro_linha_futura <- st_read(here(dir, "geosampa_metro_linha_futura.gpkg"))
# Imports subway station data
metro_estacao <- st_read(here(dir, "geosampa_metro_estacao.gpkg"))
metro_estacao_futura <- st_read(here(dir, "geosampa_metro_estacao_futura.gpkg"))

# Cleans subway line data
metro_line <- clean_metro_line(metro_linha)
future_metro_line <- clean_metro_line(metro_linha_futura)

# Cleans subway station data
station_metro <- clean_metro_station(metro_estacao)
future_station_metro <- clean_metro_station(metro_estacao_futura)
# Bind station data 
station <- bind_rows(station_metro, future_station_metro)

# Convert data to 4326
station <- st_transform(station, crs = 4326)
metro_line <- st_transform(metro_line, crs = 4326)
future_metro_line <- st_transform(future_metro_line, crs = 4326)

# Export locally
st_write(station, here("static/data/spo_metro_stations.gpkg"))
st_write(metro_line, here("static/data/spo_metro_line.gpkg"))
st_write(future_metro_line, here("static/data/spo_metro_line_future.gpkg"))