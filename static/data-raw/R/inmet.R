library(tidyverse)
library(here)
library(rvest)

url <- "https://portal.inmet.gov.br/dadoshistoricos"

download_links <- read_html(url) |> 
  html_elements(xpath = "//article[@class='post-preview']/a") |> 
  html_attr("href")

dir.create(here("static", "data-raw", "data", "inmet"), recursive = TRUE)

dest_dir <- here("static", "data-raw", "data", "inmet", "zipped_files")
dest_file <- here(dest_dir, basename(download_links))

dir.create(dest_dir, recursive = TRUE)

pb <- txtProgressBar(min = 1, max = length(download_links), style = 3)

for (i in seq_along(donwload_links)) {
  
  current_year <- gsub("\\.zip", "", basename(download_links)[[i]])
  
  glue::glue("Downloading data for year: {current_year}")
  
  download.file(
    download_links[[i]],
    destfile = dest_file[[i]],
    mode = "wb",
    quiet = TRUE
    )
  setTxtProgressBar(pb, i)
  
}

data_dir <- here("static", "data-raw", "data", "inmet", "files")
dir.create(data_dir, recursive = TRUE)

pb <- txtProgressBar(min = 1, max = length(dest_file), style = 3)

for (i in seq_along(dest_file)) {
  
  current_year <- gsub("\\.zip", "", basename(dest_file)[[i]])
  message(glue::glue("Unzipping data for year: {current_year}"))
  utils::unzip(dest_file[[i]], exdir = data_dir)
  setTxtProgressBar(pb, i)
}

# Find all csv files

path_files <- list.files(data_dir, pattern = "\\.CSV$", recursive = TRUE, full.names = TRUE)

files <- lapply(path_files[1:100], data.table::fread, skip = 9, na = c("-9999"))

station_dir <- here("static", "data-raw", "data", "inmet", "dim_stations")

dim_station_aut <- data.table::fread(
  here(station_dir, "CatalogoEstaçõesAutomáticas.csv")
)

dim_station_man <- data.table::fread(
  here(station_dir, "CatalogoEstaçõesConvencionais.csv")
)

dim_station <- data.table::rbindlist(list(dim_station_aut, dim_station_man))

dim_station <- janitor::clean_names(dim_station)

dim_station[, dt_inicio_operacao := lubridate::dmy(dt_inicio_operacao)]

code_station <- str_extract(
  basename(path_files),
  pattern = "[A-Z][0-9]{3}|[0-9]{5}"
)

import_inmet <- function(path) {
  
  dat <- import_csv(path)
  
  
}

import_csv <- function(path) {
  
  dat <- data.table::fread(
    path,
    skip = 8,
    colClasses = c("Date", "character", rep("numeric", 18)),
    na = "-9999",
    encoding = "Latin-1"
  )
  
  return(dat)
  
}

find_station <- function(path) {
  
}

clean_inmet <- function(dat) {
  
  dat <- janitor::clean_names(dat)
  
  col_names <- c(
    "data_ymd",
    "hora_utc",
    "precipitacao_total_mm",
    "pressao_atm_hora_mB",
    "pressao_atm_max_mB",
    "pressao_atm_min_mB",
    "radiacao_global_kj_m2", 
    "temp_bulbo_hora",
    "temp_orvalho_hora",
    "temp_max",
    "temp_min",
    "temp_orvalho_max",
    "temp_orvalho_min",
    "umidade_rel_max",
    "umidade_rel_min",
    "umidade_rel_hora",
    "vento_direcao",
    "vento_rajada_max",
    "vento_velocidade",
    "drop_col"
  )
  
  data.table::setnames(dat, names(dat), col_names)
  
  dat[, data_ymd_hms := lubridate::ymd_hm(paste(data_ymd, hora_utc))]
  dat[, data_hora := lubridate::hour(data_ymd_hms)]
  dat[, data_dia := lubridate::day(data_ymd)]
  dat[, data_mes := lubridate::month(data_ymd)]
  
  dat <- cbind(dat, dat_station)
  
}

dat_station <- dim_station[
  cd_estacao == code_station[[1]],
  .(dc_nome, cd_estacao)
  ]

dat <- data.table::fread(
  path_files[1],
  skip = 8,
  colClasses = c("Date", "character", rep("numeric", 18)),
  na = "-9999",
  encoding = "Latin-1"
  )

dat <- janitor::clean_names(dat)

col_names <- c(
  "data_ymd",
  "hora_utc",
  "precipitacao_total_mm",
  "pressao_atm_hora_mB",
  "pressao_atm_max_mB",
  "pressao_atm_min_mB",
  "radiacao_global_kj_m2", 
  "temp_bulbo_hora",
  "temp_orvalho_hora",
  "temp_max",
  "temp_min",
  "temp_orvalho_max",
  "temp_orvalho_min",
  "umidade_rel_max",
  "umidade_rel_min",
  "umidade_rel_hora",
  "vento_direcao",
  "vento_rajada_max",
  "vento_velocidade",
  "drop_col"
)

data.table::setnames(dat, names(dat), col_names)

dat[, data_ymd_hms := lubridate::ymd_hm(paste(data_ymd, hora_utc))]
dat[, data_hora := lubridate::hour(data_ymd_hms)]
dat[, data_dia := lubridate::day(data_ymd)]
dat[, data_mes := lubridate::month(data_ymd)]

dat <- cbind(dat, dat_station)