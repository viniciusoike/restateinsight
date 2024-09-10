#> Número de passagens (bilhetagem eletrônica) EMTU São Paulo RMSP
#> https://www.emtu.sp.gov.br/dadosAbertosEmtu/relatorios/bilhetagem.htm#

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(here)
library(janitor)

cn <- c("month", "vt", "comum", "escolar", "empresarial", "senior", "especial", "total")

dat <- read_excel(
  here("static/data/raw/emtu_passagens.xlsx"),
  skip = 1,
  col_names = cn
  )

clean_dat <- dat |> 
  mutate(
    date = if_else(
      nchar(month) == 5,
      excel_numeric_to_date(as.numeric(month)),
      parse_date(month, format = "%b/%Y", locale = locale("pt"))
      )
  ) |> 
  select(-month) |> 
  pivot_longer(-date, names_to = "type", values_to = "count")

readr::write_csv(clean_dat, here("static/data/emtu_passagens.csv"))