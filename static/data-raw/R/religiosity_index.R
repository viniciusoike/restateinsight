library(tabulizer)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr, include.only = "pivot_longer")
library(countries, include.only = "country_name")


# (opcional: baixa os dados)
#url = "https://pewresearch ..."
#download.file(url, destfile = tempfile(fileext = "pdf"))

# Importa a tabela
path = here::here("static/data/raw/globalReligion-tables.pdf")
tables = extract_tables(path)

# Limpeza -------------------------------------------------------

# Nome das colunas

header = tables[[1]][1:2, ]
# Junta as colunas e 'limpa' os nomes
col_names = apply(header, 2, \(x) make_clean_names(str_c(x, collapse = "_")))
# Empilha os dados e define nome das colunas
table_data = bind_rows(lapply(tables, as.data.frame))
table_data = table_data[3:nrow(table_data), ]
names(table_data) = col_names

# Converte colunas para numérico
table_data = table_data |> 
  as_tibble() |> 
  mutate(across(2:last_col(), ~as.numeric(str_remove_all(.x, "[%<>,]"))))

# Remove as últimas sete linhas
table_regions <- slice_tail(table_data, n = 7)
table_data <- slice(table_data, 1:(nrow(table_data) - 7))

# Nome dos países

# Correção manual
test = table_data |> 
  mutate(
    name = country_name(country, to = "name_en", fuzzy_match = TRUE),
    iso3c = country_name(country, to = "ISO3", fuzzy_match = TRUE)
  )

errors = test |>
  get_dupes(name) |> 
  select(name, country)

correction = c(
  "c", "k", "n", "n", "p", "u", "m", "m", "p", "", "m", "", "l", "e", "p", "f",
  "i", "n", "z", "g", "u", "i", "o", "r", "r", "p", "s", "g", "u", "f", "u"
)

fix_table = errors |> 
  mutate(country_fixed = str_c(str_to_upper(correction), country)) |> 
  select(country, country_fixed)

# Matching dos nomes
table_data = table_data |> 
  left_join(fix_table, by = 'country', relationship = "many-to-many") |> 
  mutate(
    temp_name = case_when(
      country == "ambia" & country_population == 1730000 ~ "Gambia",
      country == "ambia" & country_population > 1730000 ~ "Zambia",
      is.na(country_fixed) ~ country,
      TRUE ~ country_fixed
    ),
    # faz o matching do nomes dos países
    name = country_name(temp_name, to = "name_en", fuzzy_match = TRUE),
    # resolve alguns casos extremos "Channel Islands"
    name = if_else(is.na(name), temp_name, name),
    # encontra o ISO3 code de cada país
    iso3c = country_name(name, to = "ISO3", fuzzy_match = FALSE)
  ) |> 
  select(name, iso3c, country_population:percent_jewish)

# Tidy

# Converte os dados para long
tab_religion = table_data |> 
  pivot_longer(
    cols = percent_christian:percent_jewish,
    names_to = "religion",
    values_to = "share"
  ) |> 
  mutate(religion = str_remove(religion, "(percent_)|(_religion)"))

data.table::fwrite(tab_religion, "static/data/religion_by_country.csv")
