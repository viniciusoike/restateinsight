#> Very hard extraction
#> csv files are in a very bad format
#> even some of the pdf files store the tables as low-resolution images

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(here)
library(pdftools)

fld <- here("static/data/raw/metro_sp")

csv_files <- list.files(fld, pattern = "\\.csv$", full.names = TRUE)
csv_names <- janitor::make_clean_names(basename(csv_files))
csv_names <- str_remove(csv_names, "_csv$")

# Import csv files
lapply(csv_files, read_csv)

path_pdf <- here("static/data/raw/metro_sp/2017/1-Entrada de Passageiros por Estaá∆o - MÇdia dos Dias Èteis - Janeiro - 2017.pdf")

