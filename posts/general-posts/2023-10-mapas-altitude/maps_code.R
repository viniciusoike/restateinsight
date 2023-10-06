library(magrittr, include.only = "%>%")
library(ggplot2)

sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
showtext::showtext_auto()

cities_brazil = geobr::read_municipality(year = 2020)
states_brazil = geobr::read_state()

cities_brazil <- cities_brazil |> 
  sf::st_drop_geometry() |> 
  dplyr::left_join(
    sf::st_drop_geometry(states_brazil),
    by = dplyr::join_by(code_state, abbrev_state, name_state, code_region, name_region)
    )

get_border = function(city) {

  if (is.numeric(city) && nchar(city == 7)) {
    code_muni = city
    
    if (!any(code_muni %in% cities_brazil$code_muni)) {
      stop("City code not found.")
    }
    
  }
  
  if (is.character(city)) {

    code_muni = cities_brazil |> 
      dplyr::filter(name_muni == city) |> 
      dplyr::pull(code_muni)
    
    if (length(code_muni) == 0) {
      stop("City not found. Please check the spelling.")
    }
    
    if (length(code_muni) > 1) {
      stop("Duplicated city name. Please insert the 7-digit IBGE code.")
    }
    
  }
  
  #> Importa o shape do município
  border = geobr::read_municipality(code_muni, showProgress = FALSE)
  
  return(border)
  
}

get_state <- function(city) {
  
  if (is.numeric(city) && nchar(city == 7)) {
    code = city
    
    if (!any(code_muni %in% cities_brazil$code_muni)) {
      stop("City code not found.")
    }
    
  }
  
  if (is.character(city)) {
    
    code = cities_brazil |> 
      dplyr::filter(name_muni == city) |> 
      dplyr::pull(code_muni)
    
    if (length(code) == 0) {
      stop("City not found. Please check the spelling.")
    }
    
    if (length(code) > 1) {
      stop("Duplicated city name. Please insert the 7-digit IBGE code.")
    }
    
  }
  
  cities_brazil |> 
    dplyr::filter(code_muni == code) |> 
    dplyr::pull(name_state) |> 
    unique()
  
}

get_streets = function(city, border) {
  
  #> Encontra o nome da Unidade Federativa
  nome_uf = get_state(city)
  #> Monta o nome do local
  name_place = stringr::str_glue("{city}, {nome_uf}, Brazil")
  #> Monta a query
  place = osmdata::opq(bbox = osmdata::getbb(name_place))
  
  #> Importa todas as principais vias da cidade
  streets = osmdata::add_osm_feature(
    place,
    key = "highway",
    value = c("primary", "secondary", "tertiary", "residential")
  )
  
  #> Converte o dado
  streets = streets %>%
    osmdata::osmdata_sf() %>%
    .$osm_lines %>%
    dplyr::select(osm_id, name) %>%
    sf::st_transform(crs = 4674)
  
  #> Enconrtra a intersecção entre as estradas e o limites do município
  streets_border = sf::st_intersection(streets, border)
  #> Retorna o objeto streets_border
  return(streets_border)
  
}


get_elevation = function(border, z = 8) {
  #> Importa os dados de altitude
  altitude = elevatr::get_elev_raster(border, z = z, clip = "bbox")
  #> Converte para 'vector'
  altitude = raster::rasterToPolygons(altitude)
  altitude = sf::st_as_sf(altitude)
  names(altitude)[1] = "elevation"
  
  #> Converte o CRS e intersecta com os limites do município
  altitude = sf::st_transform(altitude, crs = 4674)
  altitude = suppressWarnings(sf::st_intersection(altitude, border))
  altitude = dplyr::filter(altitude, sf::st_is_valid(altitude))
  
  return(altitude)
  
}

add_jenks_breaks = function(shp, k = 7, round = FALSE) {
  #> Classifica os dados de altitude em k grupos segundo o algo. de Jenks
  jbreaks = BAMMtools::getJenksBreaks(shp$elevation, k = k)
  #> Arredonda os números para chegar numa legenda menos quebrada
  if (round) {
    jbreaks[1] = floor(jbreaks[1])
    jbreaks[length(jbreaks)] = ceiling(jbreaks)
    jbreaks[2:(length(jbreaks) - 1)] = round(jbreaks)
  }
  #> Cria a coluna 'jenks_group' que classifica cada valor num grupo
  shp = shp |> 
    dplyr::mutate(
      jenks_group = findInterval(elevation, jbreaks, rightmost.closed = TRUE),
      jenks_group = factor(jenks_group, labels = get_jenks_labels(jbreaks))
    )
  
  #> Verifica se todas as observações tem um grupo
  check = any(is.na(shp$jenks_group))
  if (check) {
    warning("Some observations have failed to be grouped")
  }
  
  #> Transforma os groups em legendas
  labels = get_jenks_labels(jbreaks)
  
  #> Retorna o output numa lista
  out = list(shp = shp, labels = labels)
  return(out)
  
}

get_jenks_labels = function(x) {
  labels = paste(x, x[-1], sep = "–")
  labels[1] = paste("<", x[2])
  labels <- labels[1:(length(labels) - 1)]
  return(labels)
}

get_streets_altitude = function(altitude, streets) {
  
  stopifnot(any(colnames(altitude) %in% "jenks_group"))
  
  #> Encontra todos os grupos
  groups = levels(altitude$jenks_group)
  
  #> Define uma função auxiliar
  
  #> Esta função filtra o grid de altitude e faz a sua interseção
  #> com o shape das princiapis vias
  join_streets = function(group) {
    
    poly = altitude %>%
      dplyr::filter(jenks_group == group) %>%
      sf::st_union(.) %>%
      sf::st_as_sf() %>%
      sf::st_make_valid()
    
    joined = suppressWarnings(sf::st_intersection(streets, poly))
    
    return(joined)
    
  }
  #> Aplica a função acima em todos os grupos em paralelo
  street_levels = furrr::future_map(groups, join_streets)
  #> "Empilha" o objeto num único spatial data.frame
  out = dplyr::bind_rows(street_levels, .id = "level")
  
  return(out)
  
}

map_plot = function(shp, labels, title, showtext = TRUE) {
  
  cores = viridis::plasma(n = length(labels) + 1)
  cores = cores[-length(cores)]
  
  font = ifelse(showtext == TRUE, "Roboto Condensed", "sans")
  
  plot =
    ggplot(data = shp) +
    geom_sf(aes(color = level, fill = level), linewidth = 0.2) +
    scale_color_manual(
      name = "Altitude",
      labels = labels,
      values = cores
    ) +
    scale_fill_manual(
      name = "Altitude",
      labels = labels,
      values = cores
    ) +
    guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1)) +
    ggtitle(title) +
    ggthemes::theme_map() +
    coord_sf() +
    theme(
      plot.title = element_text(
        size = 30,
        hjust = 0.5,
        family = font
      ),
      legend.title = element_text(
        size = 20,
        family = font,
        color = "gray10"
      ),
      legend.text = element_text(
        size = 14,
        family = font,
        color = "gray10"
      ),
      legend.position = "top",
      legend.direction = "horizontal",
      plot.background = element_rect(color = NA, fill = "#f6eee3"),
      panel.background = element_rect(color = NA, fill = "#f6eee3"),
      legend.background = element_rect(color = NA, fill = "#f6eee3")
    )
  
  return(plot)
  
}

map_altitude = function(city, k = 6, z = 7) {
  
  #> Importa o shape do limite do município
  message("Importando os limites do município: ", city)
  city_border = get_border(city)
  #> Importa as principais vias da cidade e junta com o limite do muni
  message("Importando as vias.")
  city_street = get_streets(city, city_border)
  #> Importa a altitude da cidade
  message("Importando a altitude.")
  city_elevation = suppressMessages(get_elevation(city_border, z = z))
  #> Classifica a altitude em grupos
  message("Classificando e juntando os shapefiles.")
  jenks = add_jenks_breaks(city_elevation, k = k)
  city_elevation = jenks[["shp"]]
  labels = jenks[["labels"]]
  #> Junta a altitude (agrupada) com as vias
  city_street_elevation = get_streets_altitude(city_elevation, city_street)
  
  #> Monta o mapa final
  message("Gerando o mapa final.")
  plot = map_plot(city_street_elevation, labels = labels, title = city)
  message("Feito.")
  #> Retorna o output numa lista
  out = list(
    shp = city_street_elevation,
    streets = city_street,
    elevation = city_elevation,
    plot = plot
  )
  
  return(out)
  
}

cur = map_altitude("Curitiba", k = 8, z = 11)
scs = map_altitude("São Caetano Do Sul", k = 4, z = 7)
osa = map_altitude("Osasco", k = 7, z = 12)
poa = map_altitude("Porto Alegre", k = 7, z = 13)
bhe = map_altitude("Belo Horizonte", k = 7, z = 13)
ftl = map_altitude("Fortaleza", z = 13)

maps <- list(scs, osa, poa, bhe, ftl)
names(maps) <- c("sao_caetano", "osasco", "porto_alegre", "belo_horizonte", "fortaleza")
for (i in seq_along(maps)) {
  
  file_name <- names(maps)[i]
  fld <- here::here(glue::glue("static/maps/elevation_xx_{file_name}.svg"))
  ggplot2::ggsave(fld, plot = maps[[i]]$plot, width = 14, height = 16)
  
}

ggsave("static/maps/elevation_xx_curitiba.svg",
       cur$plot,
       width = 14,
       height = 16
       )
