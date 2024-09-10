library(readr)
library(dplyr)
library(tidyr)
library(here)
library(purrr)
library(stringr)


# 2021-2023 ---------------------------------------------------------------

import_csv <- function(year, linhas, variable = "transport") {
  
  raw_data <- read_csv(year, linhas, variable)
  clean_data <- clean_csv(raw_data, year, linhas)
  
  return(clean_data)
  
}

read_csv <- function(year, linhas, variable) {

  stopifnot(any(year %in% 2021:2023))
  stopifnot(any(linhas %in% c(1, 2)))
  stopifnot(any(variable %in% c("transport", "entrance")))
  
  base_fld <- here::here("static/data/raw/metro_sp")
  path_csv <- list.files(base_fld, pattern = "\\.csv$", full.names = TRUE)
  
  if (variable == "stations") {
    path_csv <- path_csv[stringr::str_detect(path_csv, "Estação")]
  } else if (variable == "transport") {
    path_csv <- path_csv[stringr::str_detect(path_csv, "Transport")]
  } else if (variable == "entrance") {
    path_csv <- path_csv[stringr::str_detect(path_csv, "Passageiros por Linha")]
  }
  
  path_csv <- path_csv[stringr::str_detect(path_csv, as.character(year))]
  
  skip <- ifelse(linhas == 1, 6, 25)
  
  dat <- readr::read_delim(
    path_csv,
    skip = skip,
    n_max = ifelse(year == 2023, 9, 12),
    col_names = FALSE,
    delim = ";",
    locale = readr::locale(grouping_mark = "."),
    show_col_types = FALSE
  )
  
  return(dat)
  
}

clean_csv <- function(dat, year, linhas) {
  
  dat <- dat |> 
    select(where(~!all(is.na(.x))))
  
  cnames <- c("month", "total", "mdu", "msa", "mdo", "max")
  
  if (linhas == 1) {
    metro_lines <- c("azul", "verde")
  } else {
    metro_lines <- c("vermelha", "prata")
  }
  
  cnames <- paste(rep(metro_lines, each = 6), cnames, sep = "_")
  
  names(dat) <- cnames
  
  new_names <- c("month" = "azul_month", "month" = "vermelha_month")
  rm_cols <- c("verde_month", "prata_month")
  
  clean_dat <- dat |> 
    select(-any_of(rm_cols)) |> 
    rename(any_of(new_names)) |> 
    mutate(
      month = stringr::str_remove(month, "\\*"),
      year = year,
      date = parse_date(
        glue::glue("{year}-{month}-01"),
        format = "%Y-%b-%d",
        locale = locale("pt")
      )
    ) |> 
    select(date, matches(glue::glue("^{metro_lines[1]}|^{metro_lines[2]}"))) |> 
    tidyr::pivot_longer(-date) |> 
    tidyr::separate(name, into = c("metro_line", "metric"), sep = "_")
  
  return(clean_dat)
  
}

read_station_csv <- function(path, metro_line) {
  
  skipr <- c("azul" = 5, "verde" = 35, "vermelha" = 56, "prata" = 80)
  nstation <- c("azul" = 23, "verde" = 14, "vermelha" = 18, "prata" = 11)
  
  skip <- unname(skipr[metro_line])
  n_max <- unname(nstation[metro_line])
  
  read_delim(
    path,
    skip = skip,
    n_max = n_max,
    show_col_types = FALSE,
    locale = locale(encoding = "ISO-8859-1", grouping_mark = ".")
  )
  
}

clean_station_csv <- function(dat, year) {
  
  clean_dat <- dat |> 
    janitor::clean_names() |> 
    select(where(~!all(is.na(.x)))) |> 
    select(-media) |> 
    pivot_longer(
      -estacao,
      names_to = "month_abb",
      values_to = "avg_passenger",
      values_transform = as.numeric) |> 
    mutate(
      estacao = stringr::str_remove_all(estacao, "¹|2|²"),
      estacao = stringr::str_trim(estacao, side = "both"),
      year = year,
      date = parse_date(
        glue::glue("{year}-{month_abb}-01"),
        format = "%Y-%b-%d",
        locale = locale("pt")
      )
    ) |> 
    select(date, estacao, avg_passenger)
  
  return(clean_dat)
  
}

import_station_csv <- function(year) {
  
  stopifnot(any(year %in% 2021:2023))
  
  base_fld <- here::here("static/data/raw/metro_sp")
  path_csv <- list.files(base_fld, pattern = "\\.csv$", full.names = TRUE)
  path_csv <- path_csv[stringr::str_detect(path_csv, "Entrada de Passageiros por Estação")]
  path_csv <- path_csv[stringr::str_detect(path_csv, as.character(year))]
  
  lines <- c("azul", "verde", "vermelha", "prata")
  
  dat <- purrr::map(lines, \(x) read_station_csv(path_csv, x))
  clean_dat <- purrr::map2(dat, year, clean_station_csv)
  names(clean_dat) <- lines
  out <- bind_rows(clean_dat, .id = "metro_line")
  
  return(out)
  
}

files <- tidyr::expand_grid(
  year = 2021:2023,
  linhas = 1:2,
  variable = c("entrance", "transport")
  )

clean_files <- files |> 
  mutate(dat = purrr::pmap(list(year, linhas, variable), import_csv)) |> 
  tidyr::unnest(dat) |> 
  select(-linhas)

write_csv(clean_files, here::here("static/data/metro_sp_2021_2023.csv"))

years <- 2021:2023
station <- bind_rows(purrr::map(years, import_station_csv))

write_csv(station, here::here("static/data/metro_sp_stations_2021_2023.csv"))

path <- "/Users/viniciusoike/Documents/GitHub/restateinsight/static/data/raw/metro_sp/Demanda - 2020_0/Entrada de Passageiros por Estaá∆o - MÇdia Dias Èteis - Agosto 2020.csv"

read_station_month_csv <- function(path) {
  
  base_fld <- here::here("static/data/raw/metro_sp")
  path_csv <- list.files(base_fld, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  path_csv <- path_csv[stringr::str_detect(path_csv, "Entrada de Passageiros por Esta")]
  path_csv <- path_csv[stringr::str_detect(path_csv, as.character(year))]
  
  dat <- read_delim(
    path,
    delim = ";",
    skip = 6,
    n_max = 23,
    col_names = FALSE,
    na = "- ",
    locale = locale(encoding = "ISO-8859-1", grouping_mark = ".")
  )
  
}

clean_station_month_csv <- function(dat) {
  
  cnames <- c("name-station", "passengers")
  cnames <- paste(cnames, rep(c("azul", "vermelha", "verde", "prata"), each = 2), sep = "_")
  
  dat <- select(dat, where(~!all(is.na(.x))))
  names(dat) <- cnames
  
  dat |> 
    mutate(across(starts_with("passengers"), as.numeric)) |>
    pivot_longer(
      cols = everything(),
      names_to = c(".value", "metro_line"),
      names_pattern = "(.*)_(.*)",
      names_transform = ~stringr::str_replace(.x, "-", "_")
    ) |> 
    mutate(
      name_station = stringr::str_remove_all(name_station, "\\d"),
      name_station = stringr::str_trim(name_station)
    )
  
}

# 2018-2020 ----------------------------------------------------------------

mes <- lubridate::month(1:12, label = TRUE, abbr = FALSE, locale = "pt_BR")

## Passageiros transportados por linha -------------------------------------

path_files <- list.files(
  here("static/data/raw/metro_sp"),
  pattern = "\\.csv$",
  full.names = TRUE
  )

path_passengers_line <- path_files[stringr::str_detect(path_files, "Passageiros Transportados")]

df_line <- tibble(
  path = path_passengers_line
)

df_line <- df_line |> 
  mutate(
    name = stringr::str_extract(path, paste(mes, collapse = "|")),
    name = if_else(is.na(name), "Março", name),
    name = factor(name, levels = mes)
  ) |> 
  arrange(name)

read_csv_passengers <- function(path) {
  
  dat <- read_delim(
    path,
    skip = 4,
    name_repair = janitor::make_clean_names,
    na = c("-", "0\xb3"),
    show_col_types = FALSE,
    locale = locale(encoding = "ISO-8859-1", grouping_mark = ".")
  )
  
  dat <- dat |> 
    mutate(across(2:last_col(), as.numeric)) |> 
    select(where(~!all(is.na(.x))))
  
  return(dat)
  
}

stack_passengers <- function(ls, unite = TRUE, year = 2018) {

  x <- c(
    "Total", "Média dos dias úteis", "Média dos Sábados", "Média dos Domingos",
    "Máxima Diária"
  )
  #> Stack tables
  tbl <- bind_rows(ls, .id = "month")
  
  if (unite) {
    
    col_names <- names(tbl)
    unite_cols <- col_names[str_detect(col_names, "^linha_5_lil")]
    
    if (length(unite_cols) == 0) {
      
      warning("No columns to unite found.")
      
    } else {
      
      tbl <- tbl |> 
        unite(
          "linha_5_lilas",
          unite_cols,
          na.rm = TRUE
        ) 
    }
    
  }
  
  #> Manually replace the 'demanda_milhares' column and convert to long
  tbl <- tbl |> 
    filter(!is.na(linha_1_azul)) |> 
    distinct() |> 
    mutate(variable = rep(x, 12), year = year) |> 
    select(-demanda_milhares) |> 
    pivot_longer(
      cols = -c(variable, year, month),
      names_to = "metro_line",
      values_transform = as.numeric
    )
  
  #> Parse date and select columns
  tbl <- tbl |> 
    mutate(
      date = glue::glue("{year}-{month}-01"),
      date = parse_date(date, format = "%Y-%B-%d", locale = locale("pt"))
    ) |> 
    select(date, year, variable, metro_line, value)
  
  return(tbl)
  
}

import_passengers <- function(year = 2018, type = "transport") {

  stopifnot(any(type %in% c("transport", "entry")))
  
  fld <- case_when(
    year == 2017 ~ "2017",
    year == 2018 ~ "2018",
    year == 2019 ~ "Demanda - 2019",
    year == 2020 ~ "Demanda - 2020_0"
  )
  
  # fld <- ifelse(year == 2018, "2018", "Demanda - 2019")
  
  path_files <- list.files(
    here(str_glue("static/data/raw/metro_sp/{fld}")),
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  mes <- lubridate::month(1:12, label = TRUE, abbr = FALSE, locale = "pt_BR")
  
  pat <- ifelse(
    type == "transport",
    "Transportados por Linha",
    "Passageiros por Linha"
    )
  
  path <- path_files[str_detect(path_files, pat)]
  
  df_line <- tibble(
    path = path
  )
  
  df_line <- df_line |> 
    mutate(
      name = stringr::str_extract(path, paste(mes, collapse = "|")),
      name = if_else(is.na(name), "Março", name),
      name = factor(name, levels = mes)
    ) |> 
    arrange(name)
  
  files <- map(df_line$path, read_csv_passengers)
  files <- rlang::set_names(files, df_line$name)
  passengers_line <- stack_passengers(files, year = year)
  
  return(passengers_line)
  
}

## Entrada de passageiros por estacao --------------------------------------

path = path_files[stringr::str_detect(path_files, "Entrada de Passageiros por Esta")]

df18 <- tibble(
  path = path
)

df18 <- df18 |> 
  mutate(
    name = stringr::str_extract(path, paste(mes, collapse = "|")),
    name = if_else(is.na(name), "Março", name),
    name = factor(name, levels = mes),
    encoding = if_else(name %in% c("Junho", "Julho"), "UTF-8", "ISO-8859-1")
  ) |> 
  arrange(name)

read_csv_stations_18 <- function(path, encoding) {
  
  read_delim(
    path,
    skip = 5,
    delim = ";",
    na = c("- ", "-", " - "),
    locale = locale(encoding = encoding, grouping_mark = "."),
    name_repair = janitor::make_clean_names,
    show_col_types = FALSE
  )
  
}

files <- map2(df18$path, df18$encoding, read_csv_stations_18)

clean_csv_stations <- function(dat) {
  
  dat <- dat |> 
    select(where(~!all(is.na(.x)))) |> 
    mutate(across(starts_with("entrada"), as.numeric))
  
  col_names <- names(dat)
  col_names <- ifelse(str_detect(col_names, "^est"), "name_station", col_names)
  col_names <- janitor::make_clean_names(col_names)
  
  names(dat) <- col_names
  
  str_fix <- function(x) {
    y <- stringr::str_trim(x)
    y <- stringr::str_remove_all(y, "\\d")
    return(y)
  }
  
  dat <- dat |> 
    pivot_longer(
      cols = everything(),
      names_to = c(".value", "metro_line"),
      names_pattern = "(.*)_(.*)",
      names_transform = ~stringr::str_replace(.x, "-", "_")
    )
  
  # x <- names(dat)
  # x <- ifelse(str_detect(x, "^est"), "name_station", x)
  # names(dat) <- x
  # print(names(dat))
  dat <- dat |> 
    #rename_with(~str_replace(.x, "^est", "name_station"))
    mutate(name_station = str_fix(name_station)) |> 
    select(name_station, passengers = entradas) |> 
    filter(!is.na(name_station))
  
  return(dat)
  
}

import_stations <- function(y) {
  
  stopifnot(any(y %in% 2018:2020))
  
  fld <- case_when(
    y == 2018 ~ "2018",
    y == 2019 ~ "Demanda - 2019",
    y == 2020 ~ "Demanda - 2020_0"
  )
  
  path_files <- list.files(
    here(str_glue("static/data/raw/metro_sp/{fld}")),
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  mes <- lubridate::month(1:12, label = TRUE, abbr = FALSE, locale = "pt_BR")
  
  path <- path_files[str_detect(path_files, "Passageiros por Esta")]
  
  df_station <- tibble(
    path = path
  )
  
  df_station <- df_station |> 
    mutate(
      name = stringr::str_extract(path, paste(mes, collapse = "|")),
      name = if_else(is.na(name), "Março", name),
      name = factor(name, levels = mes),
      encoding = if_else(name %in% c("Junho", "Julho"), "UTF-8", "ISO-8859-1")
    ) |> 
    arrange(name)

  
  if (y == 2018) {
    files <- map2(df_station$path, df_station$encoding, read_csv_stations_18)
  } else {
    files <- map(df_station$path, \(x) read_csv_stations(x, encoding = "ISO-8859-1"))
  }
  
  # files <- map2(df_station$path, df_station$encoding, read_csv_stations_18)
  cleaned_files <- map(files, clean_csv_stations_18)
  cleaned_files <- rlang::set_names(cleaned_files, df_station$name)
  stations <- bind_rows(cleaned_files, .id = "month")
  
  stations <- stations |> 
    mutate(
      year = y,
      date = str_glue("{year}-{month}-01"),
      date = parse_date(date, format = "%Y-%B-%d", locale = locale("pt"))
    ) |> 
    select(date, year, name_station, passengers)
  
  return(stations)
  
  
}

import_sta

# Entrada de passageiros por linha ----------------------------------------

path_entry <- path_files[str_detect(path_files, "Passageiros por Linha")]

df_entry_line <- tibble(
  path = path
)

df_entry_line <- df_entry_line |> 
  mutate(
    name = stringr::str_extract(path, paste(mes, collapse = "|")),
    name = if_else(is.na(name), "Março", name),
    name = factor(name, levels = mes)
  ) |> 
  arrange(name)

files <- map(df_entry_line$path, read_csv_passengers_18)
files <- map(files, clean_passengers_18)
files <- rlang::set_names(files, df_entry_line$name)

passengers_entry <- stack_passengers_18(files, unite = FALSE)

passengers_entry <- passengers_entry |> 
  mutate(
    metro_line = if_else(
      str_detect(metro_line, "linha_5_lil"),
      "linha_5_lilas",
      metro_line)
    ) |> 
  filter(!is.na(value))


# 2018-2020 --------------------------------------------------------------------

transport_18 <- import_passengers(year = 2018, type = "transport")
entry_18 <- import_passengers(year = 2018, type = "entry")
transport_19 <- import_passengers(year = 2019, type = "transport")
entry_19 <- import_passengers(year = 2019, type = "entry")
transport_20 <- import_passengers(year = 2020, type = "transport")
entry_20 <- import_passengers(year = 2020, type = "entry")

transport <- bind_rows(list(transport_18, transport_19, transport_20))
entry <- bind_rows(list(entry_18, entry_19, entry_20))

metro <- bind_rows(
  list(passengers_transport = transport, passengers_entry = entry),
  .id = "name"
  )

stations <- map(2018:2020, import_stations)
stations <- bind_rows(stations)

write_csv(metro, "static/data/metro_sp_2018_2020.csv") 
write_csv(stations, "static/data/metro_sp_stations_2018_2020.csv") 

import_stations(2019)
