library(pdftools)
library(tabulizer)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

read_table_passengers <- function(path) {
  
  table <- tabulizer::extract_tables(path)
  return(table)
  
}

clean_table_passengers <- function(x) {
  
  dat <- x[[1]]
  
  col_names <- dat[1, -1]
  col_variable <- dat[-1, 1]
  data_body <- dat[-1, -1]

  tbl <- tibble::as_tibble(data_body)
  names(tbl) <- col_names
  tbl <- dplyr::mutate(tbl, variable = col_variable)
  
  tbl |> 
    tidyr::pivot_longer(
      -variable,
      names_to = "metro_line",
      values_to = "passengers",
      values_transform = as_numeric
    )
  
}

import_table_passengers <- function(path) {

  dat <- read_table_passengers(path)
  clean_dat <- clean_table_passengers(dat)
  return(clean_dat)
  
}

library(here)

fld <- here("static/data/raw/metro_sp")

get_pdf_files <- function(base_dir, fld, full = TRUE) {
  
  list.files(
    here::here(base_dir, fld),
    recursive = TRUE,
    pattern = "\\.pdf$",
    full.names = full
  )
  
}

folders <- c("2017", "2018", "Demanda - 2019", "Demanda - 2020_0")

pdf_files <- get_pdf_files(fld, folders, FALSE)
pdf_paths <- get_pdf_files(fld, folders)

# pdf_paths <- list.files(here(fld, "2018"), recursive = TRUE, pattern = "\\.pdf$", full.names = TRUE)

pdf_transporte <- pdf_files[stringr::str_detect(pdf_files, "ransportados")]
path_transporte <- pdf_paths[stringr::str_detect(pdf_paths, "ransportados")]

month_abb <- c(
  "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
)

month_find <- c(
  "Janeiro", "Fevereiro", "Maráo", "Abril", "Maio", "Junho", 
  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
)

files <- tibble(path_name = path_transporte)

tbl_files <- files |> 
  dplyr::mutate(
    file_name = basename(path_name),
    month = stringr::str_extract(file_name, paste(month_find, collapse = "|")),
    month = dplyr::if_else(month == "Maráo", "Março", month),
    month = factor(month, levels = month_abb),
    year = stringr::str_extract(file_name, "20[0-9][0-9]"),
    year = as.numeric(year),
    date = readr::parse_date(
      glue::glue("{year}-{month}-01"),
      format = "%Y-%B-%d",
      locale = readr::locale("pt")
      )
    ) |> 
  dplyr::arrange(month) |> 
  dplyr::arrange(year)

safe_import <- safely(import_table_passengers)

test = tbl_files |> 
  group_by(date) |> 
  mutate(dat = map(path_name, safe_import))

extract_results <- function(dat) {
  
  if (is.null(dat$error)) {
    dat$result
  } else {
    NA
  }
  
}

library(ggplot2)

lines <- c("Azul", "Verde", "Vermelha", "Lilás", "Prata", "Rede")

test |> 
  mutate(tables = map(dat, extract_results)) |> 
  filter(!is.na(tables)) |> 
  unnest(tables) |> 
  mutate(
    clean_metro_line = str_extract(metro_line, paste(lines, collapse = "|"))) |> 
  filter(variable == "Total") |> 
  ggplot(aes(x = date, y = passengers)) +
  geom_col() + 
  facet_wrap(vars(clean_metro_line), scales = "free_y")
  


if (is.null(test$dat[[1]]$error)) {
  test$dat[[1]]$result
}



test = tabulizer::extract_tables("/Users/viniciusoike/Documents/GitHub/restateinsight/static/data/raw/metro_sp/Passageiros Transportados por Linha - 2021.pdf")

test

library(readr)

df <- data.frame(
  group_1 = c("a", "b"),
  value_1 = c(1, 2),
  group_2 = c("c", "d"),
  value_2 = c(3, 4),
  group_3 = c("e", "f"),
  value_3 = c(5, 6)
)

df <- data.frame(
  station_1 = c("Jabaquara", "Conceição"),
  passengers_1 = c(39, 13),
  station_3 = c("Vila Prudente", "Tamanduateí"),
  passengers_3 = c(33, 34),
  station_2 = c("Corinthians-Itaquera", "Artur Alvim"),
  passengers_2 = c(47, 33),
  station_15 = c("Vila Prudente", "Oratório"),
  passengers_15 = c(25, 3)
)

df |> 
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "key"),
    names_sep = "\\d"
  )

dat 

tgt <- data.frame(
  group = c("a", "b", "c", "d"),
  value = c(1, 2, 3, 4)
)

dat |> 
  pivot_longer(
    cols = everything(),
    names_to = c(".value", "station"),
    names_pattern = "(.*)_(.*)",
    values_transform = as.numeric
  )
