library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

metro1 <- read_csv(here("static/data/metro_sp_2018_2020.csv"))
metro2 <- read_csv(here("static/data/metro_sp_2021_2023.csv"))
metro4 <- read_csv(here("static/data/metro_sp_line_4.csv"))
metro5 <- read_csv(here("static/data/metro_sp_line_5.csv"))

dim_metric <- tibble(
  metric_label = unique(metro1$variable),
  metric = unique(metro2$metric)
)

dim_metric <- dim_metric |> 
  mutate(
    metric_label = str_to_title(metric_label),
    metric_label = str_replace(metric_label, "Dos", "dos")
  )

dim_metro_line <- tibble(
  metro_line = c("azul", "verde", "vermelha", "amarela", "lilas", "prata"),
  metro_line_num = c(1L, 2L, 3L, 4L, 5L, 15L)
)

sel_cols <- c(
  "year", "date", "variable", "metric", "metric_label", "metro_line",
  "metro_line_num", "value"
  )

metro1 <- metro1 |> 
  filter(metro_line != "rede") |> 
  mutate(metro_line = str_remove(metro_line, "linha_")) |> 
  separate(metro_line, into = c("metro_line_num", "metro_line")) |> 
  mutate(
    metro_line_num = as.integer(metro_line_num),
    variable = str_to_title(variable),
    variable = str_replace(variable, "Dos", "dos")
    )

metro1 <- metro1 |> 
  rename(metric_label = variable) |> 
  left_join(dim_metric, by = "metric_label")

metro1 <- metro1 |> 
  rename(variable = name) |> 
  mutate(variable = if_else(
    str_detect(variable, "transport"), "transport", "entrance")
    )

metro1 <- metro1 |> 
  select(all_of(sel_cols))

metro2 <- metro2 |> 
  left_join(dim_metro_line, by = "metro_line") |> 
  left_join(dim_metric, by = "metric") |> 
  select(all_of(sel_cols))

metro4 <- metro4 |> 
  rename(metric_label = metric) |> 
  mutate(
    variable = if_else(
      str_detect(variable, "transport"), "transport", "entrance"
      ),
    metro_line_num = 4L
  ) |> 
  left_join(dim_metro_line, by = "metro_line_num") |> 
  left_join(dim_metric, by = "metric_label") |> 
  select(all_of(sel_cols))

metro5 <- metro5 |> 
  rename(metric_label = metric, variable = source) |> 
  mutate(
    variable = if_else(
      str_detect(variable, "transport"), "transport", "entrance"
    ),
    metro_line_num = 5L
  ) |> 
  left_join(dim_metro_line, by = "metro_line_num") |> 
  left_join(dim_metric, by = "metric_label") |> 
  select(all_of(sel_cols))

metro <- rbind(metro1, metro2, metro4, metro5)

readr::write_csv(metro, here("static/data/metro_sp.csv"))

metro_stations1 <- read_csv(here("static/data/metro_sp_stations_2018_2020.csv"))
metro_stations2 <- read_csv(here("static/data/metro_sp_stations_2021_2023.csv"))


metro4_stations <- read_csv(here("static/data/metro_sp_line_4_stations.csv"))
metro5_stations <- read_csv(here("static/data/metro_sp_line_5_stations.csv"))

dim_stations <- list(
  
  name_station_1 = c(
    "Tucuruvi", "Parada Inglesa", "Jardim São Paulo-Ayrton Senna",
    "Santana", "Carandiru", "Portuguesa-Tietê", "Armênia", "Tiradentes",
    "Luz", "São Bento", "Sé", "Japão-Liberdade", "São Joaquim", "Vergueiro",
    "Paraíso", "Ana Rosa", "Vila Mariana", "Santa Cruz", "Praça da Árvore",
    "Saúde", "São Judas", "Conceição", "Jabaquara"
  ),
  
  name_station_2 = c(
    "Vila Madalena", "Sumaré", "Clínicas", "Consolação", "Trianon-Masp",
    "Brigadeiro", "Paraíso", "Ana Rosa", "Chácara Klabin", "Santos-Imigrantes",
    "Alto do Ipiranga", "Sacomã", "Tamanduateí", "Vila Prudente"
    ),
  
  name_station_3 = c(
    "Palmeiras-Barra Funda", "Marechal Deodoro", "Santa Cecília", "República",
    "Anhangabaú", "Sé", "Pedro II", "Brás", "Bresser-Mooca", "Belém", "Tatuapé",
    "Carrão", "Penha", "Vila Matilde", "Guilhermina-Esperança",
    "Patriarca", "Artur Alvim", "Corinthians-Itaquera"
  ),
  
  name_station_4 = c(
    'Vila Sônia', 'São Paulo - Morumbi', "Butantã", "Pinheiros", "Faria Lima",
    "Fradique Coutinho", "Oscar Freire", "Paulista", "Higienópolis - Mackenzie",
    "República", "Luz"
    ),
  
  name_station_5 = c(
    "Capão Redondo", "Campo Limpo", "Vila das Belezas", "Giovanni Gronchi",
    "Santo Amaro", "Largo Treze", "Adolfo Pinheiro", "Alto da Boa Vista",
    "Borba Gato", "Brooklin", "Campo Belo", "Eucaliptos", "Moema",
    "AACD - Servidor", "Hospital São Paulo", "Santa Cruz", "Chácara Klabin"
  ),
  
  name_station_15 = c(
    "Vila Prudente", "Oratório", "São Lucas", "Camilo Haddad", "Vila Tolstói",
    "Vila União", "Jardim Planalto", "Sapopemba", "Fazenda do Juta",
    "São Mateus", "Jardim Colonial"
  )
  
)

dim_stations <- dim_stations |> 
  tibble::enframe() |> 
  unnest(value) |> 
  mutate(metro_line_num = as.numeric(str_remove(name, "name_station_"))) |> 
  select(metro_line_num, name_station = value)

sel_cols <- c(
  "year", "date", "metro_line_num", "metro_line", "name_station", "passengers"
  )

dim_station_grouped <- dim_stations |> 
  group_by(name_station) |> 
  reframe(metro_line_num = paste(metro_line_num, collapse = "/"))

metro_stations1 |> 
  left_join(dim_station_grouped, by = "name_station") |> 
  filter(is.na(metro_line_num))

metro4_stations <- metro4_stations |> 
  rename(passengers = value) |> 
  mutate(metro_line_num = 4L) |> 
  left_join(dim_metro_line, by = "metro_line_num") |> 
  select(all_of(sel_cols))

metro5_stations <- metro5_stations |> 
  rename(passengers = value) |> 
  mutate(metro_line_num = 5L) |> 
  left_join(dim_metro_line, by = "metro_line_num") |> 
  select(all_of(sel_cols))

left_join(filter(dim_stations, metro_line_num == 4), by = "name_station")

# library(ggplot2)
# 
# metro <- metro |> 
#   mutate(
#     metro_line = factor(
#       metro_line,
#       levels = c("azul", "verde", "vermelha", "lilas", "prata"),
#       labels = c("1-Azul", "2-Verde", "3-Vermelha", "5-Lilás", "15-Prata")
#       )
#   )
# 
# ggplot(metro, aes(x = date, y = value, color = metro_line)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(rows = vars(metric), cols = vars(variable), scales = "free_y") +
#   scale_color_manual(
#     values = c("#377EB8", "#4DAF4A", "#E41A1C", "#984EA3", "#999999")
#   )
# 
# RColorBrewer::brewer.pal(9, "Set1")
# [1] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33"
# [7] "#A65628" "#F781BF"