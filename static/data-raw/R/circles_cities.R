# Population density along streets within 6 km of downtown ----
# Builds the data for the post "posts/general-posts/2026-09-circulos-densidade".
# For each city, it saves the streets inside a 6 km circle colored by the
# population density of the census tract they cross, plus OSM features for
# the background (water, green areas, parking) and population summaries.
#
# Sources: IBGE Censo 2022 (censobr, geobr) and OpenStreetMap (osmdata).
# Outputs:
#   static/data/raw/circles/osm_<city>.qs   raw OSM downloads (cache)
#   static/data/circles_cities.qs           list with one element per city

library(sf)
library(dplyr)
import::from(here, here)
import::from(purrr, map, set_names)

sf_use_s2(FALSE)

# Parameters ----------------------------------------------------------------

radius <- 6000

dir_osm <- here("static/data/raw/circles")
dir.create(dir_osm, showWarnings = FALSE, recursive = TRUE)

centers <- tibble::tribble(
  ~code_muni , ~name_city       , ~lat       , ~lng       ,
     3550308 , "São Paulo"      , -23.561289 , -46.655672 ,
     3304557 , "Rio de Janeiro" , -22.905087 , -43.185802 ,
     5300108 , "Brasília"       , -15.797508 , -47.875682 ,
     2304400 , "Fortaleza"      ,  -3.727428 , -38.528984 ,
     2927408 , "Salvador"       , -12.978168 , -38.512008 ,
     3106200 , "Belo Horizonte" , -19.925923 , -43.937128 ,
     1302603 , "Manaus"         ,  -3.129947 , -60.020726 ,
     4106902 , "Curitiba"       , -25.437640 , -49.269854 ,
     2611606 , "Recife"         ,  -8.066555 , -34.879667 ,
     5208707 , "Goiânia"        , -16.666486 , -49.252130 ,
     4314902 , "Porto Alegre"   , -30.036564 , -51.216034
)

centers <- mutate(
  centers,
  code_state = as.numeric(substr(code_muni, 1, 2)),
  slug = janitor::make_clean_names(name_city)
)

highway_types <- c(
  "motorway",
  "trunk",
  "primary",
  "secondary",
  "tertiary",
  "residential"
)

osm_features <- list(
  water = list(natural = c("water", "bay"), waterway = "riverbank"),
  green = list(
    landuse = c("grass", "forest"),
    natural = c("wood", "scrub"),
    leisure = "park"
  ),
  parking = list(amenity = "parking", highway = "pedestrian", man_made = "pier")
)

# Census data ---------------------------------------------------------------

census <- censobr::read_tracts(2022, "Basico")

census <- census |>
  dplyr::select(
    code_tract,
    code_muni,
    code_urban_concentration,
    name_urban_concentration,
    code_situacao,
    pop = V0001
  ) |>
  dplyr::collect() |>
  as_tibble()

pop_muni <- census |>
  summarise(pop = sum(pop, na.rm = TRUE), .by = code_muni)

pop_urban <- census |>
  summarise(pop = sum(pop, na.rm = TRUE), .by = code_urban_concentration)

# Functions -----------------------------------------------------------------

# Circle drawn in a local azimuthal equidistant projection, so it stays round
make_circle <- function(lat, lng, dist = radius) {
  crs_local <- sprintf("+proj=aeqd +lat_0=%f +lon_0=%f +units=m", lat, lng)

  circle <- st_sfc(st_point(c(lng, lat)), crs = 4326) |>
    st_transform(crs_local) |>
    st_buffer(dist = dist, nQuadSegs = 90)

  return(st_sf(geometry = circle))
}

overpass_urls <- c(
  "https://maps.mail.ru/osm/tools/overpass/api/interpreter",
  "https://overpass-api.de/api/interpreter",
  "https://overpass.private.coffee/api/interpreter"
)

# Overpass servers often time out under load, sometimes returning an empty
# response instead of an error: retry, rotating servers
fetch_osm <- function(query, extract, attempts = 12) {
  for (i in seq_len(attempts)) {
    options(
      osmdata.base_url = overpass_urls[(i - 1) %% length(overpass_urls) + 1]
    )
    out <- tryCatch(extract(osmdata::osmdata_sf(query)), error = identity)
    if (!inherits(out, "error") && nrow(out) > 0) {
      return(out)
    }
    msg <- if (inherits(out, "error")) {
      conditionMessage(out)
    } else {
      "empty response"
    }
    cli::cli_warn("Overpass request failed (attempt {i}): {msg}")
    Sys.sleep(15)
  }
  cli::cli_abort("Overpass request failed after {attempts} attempts.")
}

extract_polygons <- function(osm) {
  osm <- osmdata::unique_osmdata(osm)
  polys <- list(osm$osm_polygons, osm$osm_multipolygons)
  polys <- Filter(\(x) !is.null(x) && nrow(x) > 0, polys)
  polys <- lapply(polys, \(x) st_sf(geometry = st_geometry(x)))

  if (length(polys) == 0) {
    return(st_sf(geometry = st_sfc(crs = 4326)))
  }

  out <- st_make_valid(bind_rows(polys))

  return(out)
}

extract_streets <- function(osm) {
  osm <- osmdata::unique_osmdata(osm)
  lines <- osm$osm_lines

  if (is.null(lines) || !"highway" %in% names(lines)) {
    return(st_sf(highway = character(), geometry = st_sfc(crs = 4326)))
  }

  out <- dplyr::select(lines, highway)
  out <- dplyr::filter(out, highway %in% highway_types)

  return(out)
}

get_osm_polygons <- function(bbox, features) {
  query <- osmdata::opq(bbox = bbox, timeout = 300)
  query <- osmdata::add_osm_features(query, features = features)
  out <- fetch_osm(query, extract_polygons)

  return(out)
}

get_osm_streets <- function(bbox) {
  query <- osmdata::opq(bbox = bbox, timeout = 300)
  query <- osmdata::add_osm_feature(
    query,
    key = "highway",
    value = highway_types
  )
  out <- fetch_osm(query, extract_streets)

  return(out)
}

# Downloads OSM data once per city and caches it to disk
import_osm <- function(slug, circle) {
  path <- file.path(dir_osm, paste0("osm_", slug, ".qs"))

  if (file.exists(path)) {
    return(qs::qread(path))
  }

  bbox <- circle |>
    st_transform(4326) |>
    st_bbox()

  bbox <- as.numeric(bbox)

  cli::cli_inform("Downloading OSM data for {.val {slug}}.")
  osm <- lapply(osm_features, \(features) get_osm_polygons(bbox, features))
  osm$streets <- get_osm_streets(bbox)

  qs::qsave(osm, path)

  return(osm)
}

clip_to_circle <- function(shp, circle) {
  shp <- st_transform(shp, st_crs(circle))
  shp <- st_make_valid(shp)
  out <- suppressWarnings(st_intersection(shp, circle))

  return(out)
}

# Land = tracts that aren't water bodies (code_situacao 9); sea = the rest.
# Opening (shrink, then grow) removes thin slivers left between tracts.
get_land <- function(tracts, circle) {
  land <- tracts |>
    dplyr::filter(code_situacao != 9) |>
    clip_to_circle(circle) |>
    st_union() |>
    st_buffer(-25) |>
    st_buffer(25)

  land <- st_intersection(land, st_geometry(circle))

  return(land)
}

get_sea <- function(land, circle) {
  sea <- st_difference(st_geometry(circle), land)
  sea <- st_sf(geometry = sea)

  return(sea)
}

# Tracts touching the circle, with density computed on the full tract area
get_tracts <- function(code_state, circle) {
  tracts <- geobr::read_census_tract(
    code_state,
    year = 2022,
    simplified = FALSE,
    showProgress = FALSE
  )

  tracts <- tracts |>
    dplyr::select(code_tract) |>
    st_transform(st_crs(circle)) |>
    st_make_valid()

  tracts <- tracts[lengths(st_intersects(tracts, circle)) > 0, ]

  tracts <- tracts |>
    mutate(
      code_tract = as.numeric(code_tract),
      area_km2 = as.numeric(st_area(tracts)) / 1e6
    ) |>
    left_join(
      dplyr::select(census, code_tract, code_situacao, pop),
      by = "code_tract"
    ) |>
    mutate(
      pop = coalesce(pop, 0),
      code_situacao = coalesce(code_situacao, 0),
      density = pop / area_km2
    )

  return(tracts)
}

summarise_population <- function(tracts, circle, land, code_muni, name_city) {
  pop_circle <- suppressWarnings(st_interpolate_aw(
    dplyr::select(tracts, pop),
    circle,
    extensive = TRUE
  ))

  urban <- census |>
    dplyr::filter(code_muni == !!code_muni) |>
    distinct(code_urban_concentration, name_urban_concentration) |>
    dplyr::slice(1)

  out <- tibble(
    code_muni = code_muni,
    name_city = name_city,
    pop_circle = pop_circle$pop,
    land_km2 = as.numeric(sum(st_area(land))) / 1e6,
    pop_muni = pop_muni$pop[which(pop_muni$code_muni == code_muni)],
    name_urban = urban$name_urban_concentration,
    pop_urban = pop_urban$pop[
      which(
        pop_urban$code_urban_concentration == urban$code_urban_concentration
      )
    ]
  )

  return(out)
}

build_city <- function(code_muni, code_state, name_city, slug, lat, lng) {
  cli::cli_h2(name_city)

  circle <- make_circle(lat, lng)

  ## Census tracts ----
  tracts <- get_tracts(code_state, circle)
  land <- get_land(tracts, circle)
  summary <- summarise_population(tracts, circle, land, code_muni, name_city)
  tracts_circle <- clip_to_circle(dplyr::select(tracts, density), circle)

  ## OSM ----
  osm <- import_osm(slug, circle)
  features <- lapply(osm[names(osm_features)], clip_to_circle, circle = circle)

  streets <- clip_to_circle(osm$streets, circle)
  streets <- dplyr::select(streets, highway)
  # Streets on a tract boundary appear once per tract; the plot draws the
  # densest copy on top
  streets <- suppressWarnings(st_intersection(streets, tracts_circle))
  streets <- st_collection_extract(streets, "LINESTRING")

  sea <- get_sea(land, circle)

  out <- list(
    circle = circle,
    streets = streets,
    water = features$water,
    green = features$green,
    parking = features$parking,
    sea = sea,
    summary = summary
  )

  return(out)
}

# Build ---------------------------------------------------------------------

circles <- centers |>
  dplyr::select(code_muni, code_state, name_city, slug, lat, lng) |>
  purrr::pmap(build_city) |>
  set_names(centers$slug)

# Export --------------------------------------------------------------------

qs::qsave(circles, here("static/data/circles_cities.qs"))
