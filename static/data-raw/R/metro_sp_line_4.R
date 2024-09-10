library(rvest)
library(xml2)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)
library(pdftools)

url <- "https://www.viaquatro.com.br/linha-4-amarela/passageiros-transportados"

page <- read_html(url)

pdf_links <- page %>%
  rvest::html_elements(xpath = "//article/ul/li/a") %>%
  html_attr("href")

pdf_names <- page %>%
  rvest::html_elements(xpath = "//article/ul/li/a") %>%
  html_attr("title")

pdf_links <- pdf_links[str_detect(pdf_links, "\\.pdf$")]

params <- tibble(
  link = pdf_links,
  name = pdf_names
)

mes <- lubridate::month(1:12, label = TRUE, abbr = FALSE, locale = "pt_BR")

params <- params %>%
  mutate(
    # Find variable name
    variable = str_extract(name, ".+(?= - [A-Z0-9])"),
    # Remove excess whitespace
    variable = str_replace_all(variable, "  ", " "),
    # Get the date (either in 20xx or %B%Y format)
    x1 = str_trim(str_extract(name, "(?<= - )[A-Z0-9].+")),
    # Extract year number
    year = as.numeric(str_extract(x1, "[0-9]{4}")),
    # Extract month label (in portuguese)
    month_label = str_extract(x1, "[[:alpha:]]+"),
    # Convert to date
    ts_date = if_else(
      is.na(month_label),
      as.Date(str_c(year, "/01/01")),
      parse_date(paste(year, month_label, "01", sep = "/"), format = "%Y/%B/%d", locale = locale("pt"))
    )
  )

# params <- params %>%
#   separate(name, into = c("variable", "x1", "x2"), sep = " - ", remove = FALSE) %>%
#   mutate(
#     year = case_when(
#       str_length(x1) == 4 ~ as.numeric(x1),
#       str_length(x2) == 4 ~ as.numeric(x2),
#       TRUE ~ NA
#     ),
#     date = case_when(
#       str_detect(x1, paste(mes, collapse = "|")) ~ x1,
#       str_detect(x2, paste(mes, collapse = "|")) ~ x2,
#       TRUE ~ NA
#     ),
#     date = str_trim(date),
#     date = str_remove(date, " de"),
#     ts_date = parse_date(paste(date, "01", sep = ' '), format = "%B %Y %d", locale = locale("pt")),
#     year = if_else(is.na(year), lubridate::year(ts_date), year)
#   )

params <- params |> 
  arrange(ts_date) |> 
  arrange(variable)

#> Baixar todos os pdfs do site

#> Define a pasta onde os arquivos serão baixados
fld <- here::here("static/data/raw/metro_sp/linha_4/")
#> A 'base' da url do site
baseurl <- "https://www.viaquatro.com.br"
#> Define uma barra de progresso para acompanhar o resultado
pb <- txtProgressBar(max = nrow(params), style = 3)

#> Loop nas linhas da tabela 'params'
for (i in 1:nrow(params)) {
  
  #> Pega a coluna 'name' da coluna atual e transforma o nome idiomático
  name_file <- janitor::make_clean_names(params[["name"]][i])
  #> Adiciona extensão .pdf
  name_file <- paste0(name_file, ".pdf")
  #> Define o path para baixar o arquivo
  destfile <- here::here(fld, name_file)
  
  #> Verifica se o arquivo já existe na pasta de destino. Caso contrário baixa o pdf.
  if (file.exists(destfile)) {
    message(glue::glue("File {name_file} already exists."))
    i <- i + 1
  } else {
    message(glue::glue("Downloading file {name_file}."))
    #> Url para baixar o pdf
    url <- paste0(baseurl, params[["link"]][i])
    #> Faz o dowload do arquivo
    download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
    #> Por precaução define um breve timeout aleatório (1 seg + random)
    Sys.sleep(1 + runif(1))
  }

  #> Atualiza a barra de progresso
  setTxtProgressBar(pb, i)
  
}

read_pdf <- function(path) {
  
  tbl <- pdftools::pdf_text(path)
  tbl <- stringr::str_split(tbl, "\n")
  tbl <- tibble::tibble(text = tbl[[1]])
  
  if (all(tbl == "")) {
    warning("No text elements found!")
  }
  
  return(tbl)
  
}

get_numbers <- Vectorize(function(text) {
  
  num <- stringr::str_extract(text, "([0-9].+)|([0-9])")
  num <- stringr::str_remove(num, "\\.")
  num <- as.numeric(stringr::str_replace(num, ",", "."))
  return(num)
  
})

clean_pdf_passenger <- function(dat) {
  
  mes <- lubridate::month(1:12, label = TRUE, abbr = FALSE, locale = "pt_BR")
  
  cat <- c(
    "Total", "Média dos Dias Úteis", "Média dos Sábados", "Média dos Domingos",
    "Máxima Diária"
  )
  
  pat <- paste(str_glue("({cat})"), collapse = "|")
  
  tbl <- dat |> 
    mutate(
      variable = str_remove_all(text, "\\d"),
      variable = str_trim(variable),
      variable = str_replace_all(variable, "  ", ""),
      month = str_extract(text, paste(mes, collapse = "|")),
      metric = str_extract(text, pat),
      value = get_numbers(text)
    )
  
  tbl_date <- tbl |> 
    filter(!is.na(month)) |> 
    mutate(
      date = str_glue("{value}-{month}-01"),
      date = parse_date(date, format = "%Y-%B-%d", locale = locale("pt"))
    ) |> 
    select(date, year = value)
  
  tbl_value <- tbl |> 
    filter(!is.na(value), !is.na(metric)) |> 
    select(metric, value) |> 
    mutate(value = as.numeric(value))
  
  tbl <- cbind(tbl_date, tbl_value)
  
  return(tbl)
  
}

clean_pdf_station <- function(dat) {

  name_stations <- c(
    'Vila Sônia', 'São Paulo - Morumbi', "Butantã", "Pinheiros", "Faria Lima",
    "Fradique Coutinho", "Oscar Freire", "Paulista", "Higienópolis - Mackenzie",
    "República", "Luz")
  
  pat <- paste(str_glue("({name_stations})"), collapse = "|")
  
  tbl <- dat |> 
    mutate(
      month = str_extract(text, paste(mes, collapse = "|")),
      name_station = str_extract(text, pat),
      value = get_numbers(text)
    )
  
  tbl_date <- tbl |> 
    filter(!is.na(month)) |> 
    mutate(
      date = str_glue("{value}-{month}-01"),
      date = parse_date(date, format = "%Y-%B-%d", locale = locale("pt"))
    ) |> 
    select(date, year = value)
  
  tbl_value <- tbl |> 
    filter(!is.na(value), !is.na(name_station)) |> 
    select(name_station, value) |> 
    mutate(value = as.numeric(value))
  
  tbl <- cbind(tbl_date, tbl_value)
  
  return(tbl)
  
}

import_pdf <- function(path, type) {
  
  stopifnot(any(type %in% c("station", "passenger_entrance", "passenger_transported")))
  
  file <- read_pdf(path)
  
  if (nrow(file) == 1) {
    return(NA)
  }
  
  if (nrow(file) > 11) {
    clean_file <- clean_pdf_station(file)
  } else {
    clean_file <- clean_pdf_passenger(file)
  }
  
  return(clean_file)
  
}

#> Define a pasta onde os arquivos serão baixados
fld <- here::here("static/data/raw/metro_sp/linha_4")

path_pdfs <- list.files(fld, "\\.pdf$", full.names = TRUE)

params <- tibble(
  path = path_pdfs
)

params <- params |> 
  mutate(
    name_file = basename(path),
    type = case_when(
      str_detect(name_file, "^entrada_de_passageiros_pelas") ~ "passenger_entrance",
      str_detect(name_file, "estac") ~ "station",
      str_detect(name_file, "transportados") ~ "passenger_transported",
      TRUE ~ "station"
    )
  )

pdfs <- params |> 
  mutate(file = map2(path, type, import_pdf))

valid_files <- pdfs |> 
  filter(map_lgl(file, is.data.frame))

tbl_passengers <- valid_files |> 
  filter(type == "passenger_transported") |> 
  reframe(bind_rows(file)) |> 
  arrange(date) |> 
  mutate(name_station = "Total")

tbl_onboarding <- valid_files |> 
  filter(type == "passenger_entrance") |> 
  reframe(bind_rows(file)) |> 
  arrange(date)

# This pdf is wrong!
tbl_onboarding_station <- tbl_onboarding |> 
  filter(!is.na(name_station)) |> 
  mutate(metric = "Média dos Dias Úteis")

tbl_onboarding <- tbl_onboarding |> 
  filter(is.na(name_station)) |> 
  mutate(name_station = "Total")

tbl1 <- bind_rows(
  list(
    passenger_transported = tbl_passengers,
    passenger_entrance = tbl_onboarding
  ),
  .id = "variable")

tbl_station <- valid_files |> 
  filter(type == "station") |>
  reframe(bind_rows(file)) |> 
  arrange(date)

write_csv(tbl1, here("static/data/metro_sp_line_4.csv"))
write_csv(tbl_station, here("static/data/metro_sp_line_4_stations.csv"))