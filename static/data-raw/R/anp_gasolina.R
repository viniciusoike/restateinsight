library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(here)

#> Get demand for Gasolina C
dat <- read_excel(
  here("static/data/raw/vendas-combustiveis-m3.xls.xlsx"),
  range = "B518:N530"
  )

clean_dat <- dat |> 
  rename(month_label = `Mês`) |> 
  pivot_longer(-month_label, names_to = "year", values_to = "demand") |> 
  mutate(
    date = parse_date(
      str_glue("{year}-{month_label}-01"),
      format = "%Y-%B-%d",
      locale = locale("pt")
      )
  ) |> 
  select(
    date, demand
  ) |> 
  arrange(date)

write_csv(clean_dat, here("static/data/br_anp_gasolina.csv"))