info_sidra(2759)


meso <- geobr::read_meso_region(year = 2020)

ggplot(meso) +
  geom_sf(lwd = 0.15)

micro <- geobr::read_micro_region(year = 2020)

ggplot(micro) +
  geom_sf(lwd = 0.15)

query <- "https://apisidra.ibge.gov.br/values/t/2759/n8/all/v/allxp/p/last%201/c236/0/c247/0/c248/0/c245/5604,5605,5606,5607,5608,5609,107118,107119,107120,107121,107122,107123,107124,107125,107126,107127,107128,107129,107130,107131,107132,107133,107134,107135,107136,107137,107138,107139,107140,107141,107142/c246/0"

test = get_sidra(api = query)



info_sidra(2759)

get_sidra(
  2759,
  period = "2022",
  variable = 221,
  classific = "c247",
  category = list(c(107092:107116, 5616:5621)),
  geo = "")


# Importar os dados da API
req <- httr::GET(url = query)
json <- httr::content(req, as = "text", encoding = "UTF-8")
json <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)
