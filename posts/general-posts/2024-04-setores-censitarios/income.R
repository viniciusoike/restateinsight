library(arrow)
library(censobr)

code_city <- 4106902

#> Importa o shapefile dos setores censitários
tracts <- geobr::read_census_tract(
  code_tract = code_city,
  year = 2010,
  simplified = FALSE,
  showProgress = FALSE
)

#> Importa a tabela Basico do Censo (2010)
tract_basico <- read_tracts(
  year = 2010,
  dataset = "Basico", 
  showProgress = FALSE
)
#> Importa a tabela DomicilioRenda do Censo (2010)
tract_income <- read_tracts(
  year = 2010,
  dataset = "DomicilioRenda", 
  showProgress = FALSE
)

#> Seleciona as colunas relevantes
tract_basico <- tract_basico |>
  dplyr::filter(code_muni == code_city) |> 
  dplyr::select('code_tract', 'V002')

tract_income <- tract_income |> 
  dplyr::filter(code_muni == code_city) |> 
  dplyr::select('code_tract','V003')

#> Juntas as bases e importa os dados
tracts_df <- dplyr::left_join(tract_basico, tract_income)
tracts_df <- dplyr::collect(tracts_df)
#> Calcula a renda per capita
tracts_df <- dplyr::mutate(tracts_df, income_pc = V003 / V002)
#> Junta os dados com o shapefile
city_tracts <- dplyr::left_join(tracts, tracts_df, by = "code_tract")

city_tracts <- city_tracts |> 
  dplyr::mutate(decile = dplyr::ntile(income_pc, 10))

sf::st_write(city_tracts, here::here("static/data/census/example_income.gpkg"))
