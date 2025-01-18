# Downloads zip, csv, and pdf files from METRO SP
# https://transparencia.metrosp.com.br/
# Script should be followed by metro_sp_csvs.R and/or metro_sp_pdfs.R

library(dplyr)
library(stringr)

import::from(rvest, read_html, html_elements, html_attr)
import::from(stringi, stri_trans_general)
import::from(here, here)

str_simplify <- function(x) {
  
  y <- stringi::stri_trans_general(x, id = "latin-ascii")
  y <- stringr::str_replace_all(y, " ", "_")
  y <- stringr::str_to_lower(y)
  
  return(y)
  
}

url <- "https://transparencia.metrosp.com.br/dataset/demanda"

page <- read_html(url)

link_download <- page %>%
  html_elements(xpath = "//*[@id='data-and-resources']/div/div/ul/li/div/span/a") %>%
  html_attr(name = "href")

# Subset only https links
link_download = link_download[str_detect(link_download, "^https")]

# link_download = link_download[str_detect(link_download, "\\.csv$|\\.pdf$")]

link_title = page %>%
  html_elements(xpath = "//*[@id='data-and-resources']/div/div/ul/li/div/a") %>%
  html_attr(name = "title")

params = tibble(
  url = link_download,
  title = link_title
)

params <- params |> 
  mutate(
    name_file = str_remove_all(title, " -"),
    name_file = str_simplify(name_file),
    name_file = str_remove_all(name_file, "/"),
    type_file = str_extract(url, "\\.[a-z]{3}$"),
    year = as.numeric(str_extract(url, "(?<=20)[0-9]{4}")),
    year = if_else(is.na(year), as.numeric(str_extract(name_file, "[0-9]{4}")), year)
  )

zipped_files <- params |> 
  filter(type_file == ".zip") |> 
  arrange(year)

fld <- here("static/data/raw/metro_sp/metro")

# Only needs to be downloaded once

# for (i in seq_along(zipped_files$url)) {
#   
#   download.file(
#     zipped_files$url[[i]],
#     destfile = here(fld, paste0(zipped_files$name_file[[i]], ".zip"))
#   )
#   
# }

# Easier to unzip manually or using terminal
list.files(fld, pattern = "\\.csv$", recursive = TRUE)

csv_files <- params |> 
  filter(type_file == ".csv") |> 
  arrange(year)

for (i in seq_along(csv_files$url)) {
  
  download.file(
    csv_files$url[[i]],
    destfile = here(fld, "csv", paste0(csv_files$name_file[[i]], ".csv"))
  )
  
}

pdf_files <- params |> 
  filter(type_file == ".pdf") |> 
  arrange(year)

for (i in seq_along(pdf_files$url)) {
  
  download.file(
    pdf_files$url[[i]],
    destfile = here(fld, "pdf", paste0(pdf_files$name_file[[i]], ".pdf"))
  )
  
}
