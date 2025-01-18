library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(here)
library(readr)
library(purrr)

year = 2018:2024
url = "https://www.viamobilidade.com.br/nos/passageiros-transportados/linha-5-lilas?periodo={year}"
url = str_glue(url)

get_links <- function(url) {
  
  pdf_links <- url |> 
    xml2::read_html(encoding = "UTF-8") |> 
    rvest::html_elements(xpath = '//*[@id="page"]/div/div[4]/ul/li/a') |> 
    rvest::html_attr("href") |> 
    rvest::repair_encoding(from = "UTF-8")
  
  pdf_names <- url |> 
    xml2::read_html(encoding = "UTF-8") |> 
    rvest::html_elements(xpath = '//*[@id="page"]/div/div[4]/ul/li') |> 
    rvest::html_text() |> 
    rvest::repair_encoding(from = "UTF-8")
  
  params <- tibble(
    link = pdf_links,
    name = pdf_names
  )
  
  mes <- lubridate::month(1:12, label = TRUE, abbr = FALSE, locale = "pt_BR")
  mes <- c(mes, tolower(mes))
  
  params <- params %>%
    mutate(
      name = str_remove(name, "Download"),
      name = str_trim(name)
    ) %>%
    separate(name, into = c("variable", "x1", "x2"), sep = " - ", remove = FALSE) %>%
    mutate(
      year = case_when(
        str_length(x1) == 4 ~ as.numeric(x1),
        str_length(x2) == 4 ~ as.numeric(x2),
        TRUE ~ NA
      ),
      date = case_when(
        str_detect(x1, paste(mes, collapse = "|")) ~ x1,
        str_detect(x2, paste(mes, collapse = "|")) ~ x2,
        TRUE ~ NA
      ),
      date = str_trim(date),
      date = str_remove(date, " de"),
      ts_date = parse_date(paste(date, "01", sep = ' '), format = "%B %Y %d", locale = locale("pt")),
      year = if_else(is.na(year), lubridate::year(ts_date), year)
    )
  
  params <- params |> 
    arrange(ts_date) |> 
    arrange(variable) |> 
    distinct()
  
  return(params)
  
}

#> Encontra todos os links disponíveis e retorna um tibble
links <- map(url, get_links)
links <- bind_rows(links)

links <- links |> 
  mutate(
    name_file = basename(link),
    name_file = str_remove(name_file, "pdf"),
    link = URLencode(link)
    # link = str_replace_all(link, " ", "%20"),
    # link = str_replace(link, "Ã§Ã£", "çã"),
    # link = str_replace(link, "Ã§", "ç"),
    # link = str_replace(link, "Ãµ", "õ"),
    # link = if_else(str_detect(link, "​ - "), URLencode(link), link)
    )

to_yearmonth <- function(date) {
  
  stopifnot(inherits(date, "Date"))
  
  yy <- lubridate::year(date)
  mm <- stringr::str_pad(lubridate::month(date), width = 2, side = "left", pad = "0")
  
  return(stringr::str_c(yy, mm))
  
}

str_simplify <- function(x) {
  
  y <- stringi::stri_trans_general(x, id = "latin-ascii")
  y <- stringr::str_replace_all(y, " ", "_")
  y <- stringr::str_to_lower(y)
  
  return(y)
  
}

links <- links |> 
  mutate(
    name_file_simplified = if_else(
      x1 == "Média dia útil",
      paste(to_yearmonth(ts_date), str_simplify(variable), str_simplify(x1), sep = "_"),
      paste(to_yearmonth(ts_date), str_simplify(variable), sep = "_")
    ),
    name_file_simplified = paste0(name_file_simplified, ".pdf")
  )

#> Baixar todos os pdfs do site

#> Define a pasta onde os arquivos serão baixados
fld <- here::here("static/data/raw/metro_sp/linha_5")
#> A 'base' da url do site
baseurl <- "https://www.viamobilidade.com.br"
#> Define uma barra de progresso para acompanhar o resultado
pb <- txtProgressBar(max = nrow(links), style = 3)
errors <- c()
#> Loop nas linhas da tabela 'links'
for (i in 1:nrow(links)) {
  
  #> Pega a coluna 'name' da coluna atual e transforma o nome idiomático
  # name_file <- janitor::make_clean_names(links[["name_file"]][i])
  #> Adiciona extensão .pdf
  # name_file <- paste0(name_file, ".pdf")
  #> Define o path para baixar o arquivo
    name_file <- links[["name_file_simplified"]][[i]]
    destfile <- here::here(fld, name_file)
  
  #> Verifica se o arquivo já existe na pasta de destino. Caso contrário baixa o pdf.
  if (file.exists(destfile)) {
    message(glue::glue("File {name_file} already exists."))
    i <- i + 1
  } else {
    #> Url para baixar o pdf
    url <- paste0(baseurl, links[["link"]][i])
    #> Faz o dowload do arquivo
    message(glue::glue("Downloading {name_file}."))
    try(x <- download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE))
    
    if (inherits(x, "try-error")) {
      errors <- c(errors, url)
      }
    
    #> Por precaução define um breve timeout aleatório (1 seg + random)
    Sys.sleep(1 + runif(1))
  }
  
  #> Atualiza a barra de progresso
  setTxtProgressBar(pb, i)
  
}

# Verifica se todos os download foram feitos

if (length(errors) != 0) {
  warning("Check 'errors'")
}

if (length(list.files(fld, pattern = "pdf$")) == nrow(links)) {
  warning("Some files may have failed to be downloaded")
}

#> Importa os pdfs

#> Importa o pdf e tenta transformar numa tabela
read_pdf <- function(path) {
  
  tbl <- pdftools::pdf_text(path)
  tbl <- stringr::str_split(tbl, "\n")
  tbl <- tibble::tibble(text = tbl[[1]])
  
  if (all(tbl == "")) {
    warning("No text elements found!")
  }
  
  return(tbl)
  
}

#> Tenta capturar todos os números de um string (supondo que os números estejam
#> à direita do texto)
get_numbers <- Vectorize(function(text) {

  num <- stringr::str_extract(text, "([0-9].+)|([0-9])")
  num <- stringr::str_replace(num, "\\.", "")
  num <- stringr::str_replace(num, ",", ".")
  num <- as.numeric(num)
  return(num)

})

#> Limpa a tabela com dados de estações
clean_pdf_station <- function(dat) {

  #> Encontra o nome das estações no texto
  name_stations <- c(
    "Capão Redondo", "Campo Limpo", "Vila das Belezas", "Giovanni Gronchi",
    "Santo Amaro", "Largo Treze", "Adolfo Pinheiro", "Alto da Boa Vista",
    "Borba Gato", "Brooklin", "Campo Belo", "Eucaliptos", "Moema",
    "AACD - Servidor", "Hospital São Paulo", "Santa Cruz", "Chácara Klabin"
  )
  
  pat <- paste(str_glue("({name_stations})"), collapse = "|")
  
  #> Vetor para encontrar o nome do mês no texto
  mes <- lubridate::month(1:12, label = TRUE, abbr = FALSE, locale = "pt_BR")
  mes <- paste(mes, collapse = "|")
  
  #> Encontra os padrões no texto e separa em colunas distintas
  tbl <- dat |> 
    mutate(
      #> Encontra o mês
      month = str_extract(text, mes),
      name_station = str_extract(text, pat),
      value = get_numbers(text)
    )
  
  #> Cria uma tabela 1x2 somente com a data e ano
  tbl_date <- tbl |> 
    filter(!is.na(month)) |> 
    mutate(
      date = str_glue("{value}-{month}-01"),
      date = parse_date(date, format = "%Y-%B-%d", locale = locale("pt"))
    ) |> 
    select(date, year = value)
  
  #> Cria uma tabela com o nome das estações e a variável numérica
  tbl_value <- tbl |> 
    filter(!is.na(value), !is.na(name_station)) |> 
    select(name_station, value) |> 
    mutate(value = as.numeric(value))
  
  tbl <- cbind(tbl_date, tbl_value)
  
  return(tbl)
  
}

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

import_pdf <- function(path, type) {

  if (!missing(type)) {
    stopifnot(
      any(type %in% c("station", "passenger_entrance", "passenger_transported"))
      )
  }
  
  file <- read_pdf(path)
  
  if (nrow(file) == 1) {
    return(NA)
  }
  
  if (missing(type)) {
    check_station <- any(
      stringr::str_detect(file$text, "Demanda de passageiros por estação")
    )
    
    type <- dplyr::case_when(
      any(stringr::str_detect(file$text, "Demanda de passageiros")) ~ "station",
      any(stringr::str_detect(file$text, "Entrada de passageiros")) ~ "passenger_entrance",
      any(stringr::str_detect(file$text, "Passageiros transportados")) ~ "passenger_transported"
    )
    
  } else {
    check_station <- ifelse(type == "station", TRUE, FALSE)
  }
  
  if (check_station) {
    clean_file <- clean_pdf_station(file)
  } else {
    clean_file <- clean_pdf_passenger(file)
  }
  
  clean_file <- dplyr::mutate(clean_file, source = type)
  
  return(clean_file)
  
}

safe_import_pdf <- safely(import_pdf)

#> Define a pasta onde os arquivos serão baixados
fld <- here::here("static/data/raw/metro_sp/linha_5")

path_pdfs <- list.files(fld, "\\.pdf$", full.names = TRUE)

params <- tibble(
  path = path_pdfs
)

pdfs <- params |> 
  mutate(file = map(path, safe_import_pdf))

pdfs |> 
  mutate(is_error = map_lgl(file, \(x) !is.null(x$error))) |> 
  count(is_error)

#> Resolver uma exceção
path_1 <- pdfs |> 
  mutate(is_error = map_lgl(file, \(x) !is.null(x$error))) |> 
  filter(is_error) |> 
  pull(path)

tbl <- read_pdf(path_1) 

cat <- c(
  "Total", "Média dos Dias Úteis", "Média dos Sábados", "Média dos Domingos",
  "Máxima Diária"
)

pat <- paste(str_glue("({cat})"), collapse = "|")

tbl <- tbl |> 
  mutate(
    variable = str_remove_all(text, "\\d"),
    variable = str_trim(variable),
    variable = str_replace_all(variable, "  ", ""),
    #> O problema é que 'julho' está escrito em minúsculo
    month = str_extract(text, "julho"),
    month = str_to_title(month),
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

pdfs <- pdfs |> 
  mutate(is_error = map_lgl(file, \(x) !is.null(x$error))) |> 
  filter(!is_error) |> 
  mutate(
    dat = map(file, pluck, "result"),
    type = map_chr(dat, \(x) unique(x$source))
    )

tbl_station <- pdfs |> 
  filter(type == "station") |> 
  reframe(bind_rows(dat))

tbl_passengers <- pdfs |> 
  filter(type != "station") |> 
  reframe(bind_rows(dat))

tbl_station <- tbl_station |> 
  select(-source) |> 
  select(date, year, name_station, value)

tbl_passengers <- tbl_passengers |> 
  rename(variable = source) |> 
  mutate(name_station = "Total") |> 
  select(variable, date, year, metric, value, name_station)

write_csv(tbl_passengers, here("static/data/metro_sp_line_5.csv"))
write_csv(tbl_station, here("static/data/metro_sp_line_5_stations.csv"))