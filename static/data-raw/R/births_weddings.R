library(here)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(sidrar)

# Import data -------------------------------------------------------------

parse_table <- function(json) {
  parsed <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)

  # First row is actually the header
  col_names <- as.character(parsed[1, ])
  parsed_data <- parsed[-1, ]

  tbl <- tibble::as_tibble(parsed_data)
  names(tbl) <- janitor::make_clean_names(col_names)

  return(tbl)
}

## Births by state ---------------------------------------------------------

# fmt: skip
categories <- c(
  5371,5372,5373,5374,5375,5377,5378,5379,5380,5381,5383,5384,5385,5386,5387,
  5389,5390,5391,5392,5393,5395,5396,5397,5398,5399,5401,5402,5403,5404,5405,
  5407,5408,5409,5410,5411,5412
)

categories <- paste(categories, collapse = ",")

months <- paste(5325:5336, collapse = ",")
base_url <- "https://apisidra.ibge.gov.br/values/"

base_apicall <- "{base_url}t/2612/n3/all/v/allxp/p/{year_chunk}/c235/{months}/c2/4,5/c237/0/c238/0/c240/{categories}"

# Split into groups of 2 years
years <- 2003:2024
year_chunks <- split(years, rep(1:11, each = 2))

births <- vector("list", length(year_chunks))

for (i in seq_along(year_chunks)) {
  year_chunk <- year_chunks[[i]]
  print(year_chunk)
  year_chunk <- paste(year_chunk, collapse = ",")
  apicall <- str_glue(base_apicall)
  req <- httr::GET(apicall)
  json <- httr::content(req, as = "text", encoding = "UTF-8")

  tbl <- parse_table(json)

  births[[i]] <- tbl
}

nascimentos <- bind_rows(births)

nascimentos |>
  head(10000) |>
  count(idade_da_mae_na_ocasiao_do_parto) |>
  print(n = 50)

tbl_nascimento <- nascimentos |>
  rename(
    code_state = unidade_da_federacao_codigo,
    age_mother = idade_da_mae_na_ocasiao_do_parto
  ) |>
  mutate(
    date = readr::parse_date(
      str_glue("{ano}-{mes_do_nascimento}-01"),
      format = "%Y-%B-%d",
      locale = readr::locale("pt")
    ),
    month = lubridate::month(date),
    year = as.numeric(ano),
    code_state = as.numeric(code_state),
    total_births = as.numeric(valor),
    age_mother = as.numeric(str_remove(age_mother, " ano.+"))
  ) |>
  select(date, year, month, age_mother, code_state, total_births)

# Median and average age of mother by state by year

tbl_nasc_medias <- tbl_nascimento |>
  filter(!is.na(total_births)) |>
  tidyr::uncount(weights = total_births) |>
  summarise(
    med_age = median(age_mother),
    avg_age = mean(age_mother),
    .by = c("code_state", "date")
  )

tbl_nasc_median <- tbl_nascimento |>
  summarise(
    total_ano = sum(total_births, na.rm = TRUE),
    .by = c("code_state", "year", "age_mother")
  ) |>
  tidyr::uncount(weights = total_ano) |>
  summarise(med_age = median(age_mother), .by = c("code_state", "year"))

library(ggplot2)

ggplot(tbl_nasc_medias, aes(x = date, y = avg_age)) +
  geom_line() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  facet_wrap(vars(code_state)) +
  theme_minimal()

ggplot(tbl_nasc_median, aes(x = year, y = med_age)) +
  geom_step() +
  facet_wrap(vars(code_state))

## Population projections by state -----------------------------------------

apicall <- "https://apisidra.ibge.gov.br/values/t/7358/n3/all/v/all/p/all/c2/6794/c287/93086,93087,93088,93089,93090,93091,93092,93093,93094,93095/c1933/all"

req <- httr::GET(apicall)

pop <- jsonlite::fromJSON(
  httr::content(req, as = "text"),
  simplifyDataFrame = TRUE
)

# First row is actually the header
col_names <- as.character(pop[1, ])
pop_data <- pop[-1, ]

pop <- tibble::as_tibble(pop_data)
names(pop) <- janitor::make_clean_names(col_names)

tbl_pop <- pop |>
  rename(
    year = ano_2,
    code_state = unidade_da_federacao_codigo,
    name_state = unidade_da_federacao
  ) |>
  mutate(
    age = as.numeric(str_extract(idade, "[:digit:]+")),
    valor = as.numeric(valor)
  )

tbl_pop_adult <- pop |>
  rename(
    year = ano_2,
    code_state = unidade_da_federacao_codigo,
    name_state = unidade_da_federacao
  ) |>
  mutate(
    age = as.numeric(str_extract(idade, "[:digit:]+")),
    valor = as.numeric(valor)
  ) |>
  filter(sexo == "Total", age >= 15, age <= 65, year >= 2003) |>
  summarise(
    pop_adult = sum(valor, na.rm = TRUE),
    .by = c("code_state", "name_state", "year")
  )

## Weddings ----------------------------------------------------------------

categories <- c(107092:107116, 5616:5621)
categories <- paste(categories, collapse = ",")
years <- 2003:2024

apicall <- str_glue(
  "https://apisidra.ibge.gov.br/values/t/2759/n3/all/v/allxp/p/{years}/c236/allxt/c247/5622/c248/5625/c245/0/c246/{categories}"
)

# Break into chunks
year_chunks <- list(
  2003:2005,
  2006:2008,
  2009:2011,
  2012:2014,
  2015:2017,
  2018:2020,
  2021:2023,
  2024
)
weddings <- vector("list", length(year_chunks))

for (i in seq_along(year_chunks)) {
  year_chunk <- year_chunks[[i]]
  print(year_chunk)
  year_chunk <- paste(year_chunk, collapse = ",")
  apicall <- str_glue(
    "https://apisidra.ibge.gov.br/values/t/2759/n3/all/v/allxp/p/{year_chunk}/c236/allxt/c247/5622/c248/5625/c245/0/c246/{categories}"
  )
  req <- httr::GET(apicall)
  json <- httr::content(req, as = "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)

  # First row is actually the header
  col_names <- as.character(json[1, ])
  json_data <- json[-1, ]

  tbl <- tibble::as_tibble(json_data)
  names(tbl) <- janitor::make_clean_names(col_names)

  weddings[[i]] <- tbl
}

tbl_casamentos <- bind_rows(weddings)

tbl_casamentos <- tbl_casamentos |>
  rename(
    code_state = unidade_da_federacao_codigo,
    name_state = unidade_da_federacao,
    age_male = grupo_de_idade_do_homem,
    age_female = grupo_de_idade_da_mulher
  ) |>
  mutate(
    date = readr::parse_date(
      paste0(ano, mes_do_registro, "01"),
      format = "%Y%B%d",
      locale = readr::locale("pt")
    ),
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE, locale = "pt_BR"),
    valor = as.numeric(valor)
  ) |>
  select(
    date,
    year,
    month,
    age_male,
    age_female,
    name_state,
    total_weddings = valor
  )

## Join data ---------------------------------------------------------------

# 1 - Nascimentos por UF por Ano
# 2 - Casamentos por UF por Ano por grupo de idade da mulher
# 3 - População adulta por UF por Ano

tbl_nascimento


tbl_casa_state <- tbl_casamentos %>%
  group_by(date, year, month, name_state) %>%
  summarise(
    weddings_month = sum(total_weddings, na.rm = TRUE),
    .groups = "drop"
  )

data <- inner_join(tbl_nascimento, tbl_casa_state, by = c("date", "code_state"))
data <- inner_join(data, tbl_pop_adult, by = c("year", "code_state"))

state_id <- geobr::read_state(showProgress = FALSE)
state_id <- sf::st_drop_geometry(state_id)
state_id <- mutate(
  state_id,
  name_state = str_replace(name_state, "Espirito Santo", "Espírito Santo")
)

data <- data %>%
  mutate(name_state = str_to_title(name_state)) %>%
  left_join(state_id, by = "name_state")

data <- data %>%
  mutate(
    b_rate = total_births / (pop_adult / 100000),
    w_rate = weddings_month / (pop_adult / 100000)
  )

# Export ------------------------------------------------------------------

temp <- list(
  data = data,
  weddings = tbl_casamentos,
  births = tbl_nascimento,
  pop = tbl_pop
)

qs::qsave(temp, here("static/data/births_weddings.qs"))
