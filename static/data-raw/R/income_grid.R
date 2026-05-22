library(sf)
library(dplyr)
library(ggplot2)
import::from(here, here)
import::from(censobr, read_tracts)
import::from(geobr, read_census_tract, read_municipality)
import::from(stringr, str_remove)

# Loads and joins 2010 census income and household data for a municipality
get_income_tracts <- function(code, year = 2010, geo = FALSE) {
  if (year == 2022) {
    .cols_rename <- c(
      "code_tract" = "cd_setor",
      "renda_resp" = "rendimento_responsavel_dom_part_ocu",
      "dom_ocu" = "dom_part_ocupados"
    )

    if (!exists("setores22")) {
      setores22 <- qs::qread(here("static/data/census_income_tracts.qs"))
    }

    out <- setores22 |>
      rename(all_of(.cols_rename)) |>
      mutate(
        code_muni = as.numeric(stringr::str_sub(code_tract, 1, 7)),
        dom_ocu = as.numeric(dom_ocu),
        # Valor do rendimento nominal médio mensal das pessoas responsáveis
        # com rendimentos por domicílios particulares permanentes ocupados
        renda_resp = as.numeric(renda_resp),
        renda_total = dom_ocu * renda_resp
      )

    out <- out |>
      dplyr::filter(code_muni == !!code) |>
      dplyr::select(c("code_tract", "dom_ocu", "renda_total", "geom"))

    return(out)
  }

  income <- read_tracts(2010, "ResponsavelRenda") |>
    dplyr::filter(code_muni == as.character(code)) |>
    # Total do rendimento nominal mensal das pessoas responsáveis
    # Pessoas responsáveis com rendimento positivo
    dplyr::select(code_tract, n = "V021", renda_total = "V022") |>
    dplyr::mutate(
      renda_total = as.numeric(renda_total),
      n = as.numeric(n)
    ) |>
    dplyr::collect()

  hholds <- read_tracts(2010, "Basico") |>
    dplyr::filter(code_muni == as.character(code)) |>
    dplyr::select(code_tract, dom_ocu = "V001") |>
    dplyr::mutate(dom_ocu = as.numeric(dom_ocu)) |>
    dplyr::collect()

  out <- dplyr::left_join(income, hholds, by = "code_tract")

  if (geo) {
    tracts <- geobr::read_census_tract(
      year = 2010,
      code_tract = code,
      simplified = FALSE,
      showProgress = FALSE
    )

    tracts <- tracts |>
      dplyr::mutate(code_tract = as.character(code_tract))

    out <- dplyr::left_join(tracts, out, by = "code_tract")
  }

  return(out)
}

build_grid <- function(
  code,
  city_border = NULL,
  year = 2022,
  cell_size = 500
) {
  stopifnot(year %in% c(2010, 2022))

  if (is.null(city_border)) {
    city_border <- geobr::read_municipality(
      year = year,
      code_muni = code,
      showProgress = FALSE,
      simplified = FALSE
    )
  }

  grid <- city_border |>
    sf::st_transform(crs = 31983) |>
    sf::st_make_grid(cellsize = cell_size, square = TRUE) |>
    sf::st_as_sf() |>
    dplyr::mutate(gid = dplyr::row_number())

  grid <- st_filter(
    grid,
    st_transform(city_border, crs = 31983),
    .predicate = st_intersects
  )

  return(grid)
}


prep_data <- function(dat) {
  dat <- dat |>
    dplyr::mutate(gid = dplyr::row_number())

  tab_class <- dat |>
    sf::st_drop_geometry() |>
    dplyr::filter(renda_total > 0) |>
    dplyr::mutate(class = factor(dplyr::ntile(renda_total, 10))) |>
    dplyr::select(c("gid", "class"))

  out <- dplyr::left_join(dat, tab_class, by = "gid")

  return(out)
}

prep_data <- function(dat) {
  dat <- dat |>
    dplyr::mutate(
      renda_media = dplyr::if_else(dom_ocu > 0, renda_total / dom_ocu, NA_real_)
    )

  validos <- dat |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(renda_media) & renda_media > 0 & dom_ocu > 0)

  breakpoints <- Hmisc::wtd.quantile(
    validos$renda_media,
    weights = validos$dom_ocu,
    probs = seq(0.1, 0.9, by = 0.1)
  )

  dat <- dat |>
    dplyr::mutate(
      class = dplyr::case_when(
        is.na(renda_media) | renda_media <= 0 ~ NA_integer_,
        TRUE ~ findInterval(renda_media, breakpoints) + 1L
      ),
      class = factor(class, levels = 1:10)
    )

  return(dat)
}

build_income_grid <- function(
  income,
  grid,
  k = 5,
  WGS = TRUE
) {
  .cols_income <- c("renda_total", "dom_ocu", "geometry", "geom")

  income_sub <- income |>
    dplyr::filter(dom_ocu > k) |>
    dplyr::select(dplyr::any_of(.cols_income)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), as.numeric))

  income_utm <- income_sub |>
    sf::st_transform(crs = 31983) |>
    sf::st_make_valid()

  grid_utm <- grid |>
    sf::st_transform(crs = 31983) |>
    sf::st_make_valid()

  out <- st_interpolate_aw(
    x = income_utm,
    to = grid_utm,
    extensive = TRUE,
    na.rm = TRUE
  )

  if (WGS) {
    out <- sf::st_transform(out, crs = 4326)
  }

  return(out)
}
# V005 (Basico)
# Valor do rendimento nominal médio mensal das pessoas
# responsáveis por domicílios particulares permanentes (com e sem
# rendimento)
income <- get_income_tracts(4314902, geo = TRUE)
grid_poa_2010 <- build_grid(4314902, 2010)

income_grid_poa <- build_income_grid(income, grid_poa_2010, k = 16)
income_poa_2010 <- prep_data(income_grid_poa)

mapview::mapview(income, zcol = "dom_ocu")

ggplot(income) +
  geom_sf(aes(fill = renda_total / dom_ocu), lwd = 0.05)


setores22 <- qs::qread(here("static/data/census_income_tracts.qs"))
income22 <- get_income_tracts(4314902, year = 2022)
grid_poa_2022 <- build_grid(4314902)
income_grid_poa_22 <- build_income_grid(income22, grid_poa_2022)
income_poa_2022 <- prep_data(income_grid_poa_22)

ggplot(income_poa_2010) +
  geom_sf(aes(fill = class), color = "gray90", lwd = 0.15) +
  scale_fill_brewer(palette = "Spectral", na.value = "gray30") +
  ggthemes::theme_map() +
  labs(title = 2010) +
  theme(legend.position = "none")

ggplot(income_poa_2022) +
  geom_sf(aes(fill = class), color = "gray90", lwd = 0.15) +
  scale_fill_brewer(palette = "Spectral", na.value = "gray30") +
  ggthemes::theme_map() +
  labs(title = 2022) +
  theme(legend.position = "none")


# Example usage -----------------------------------------------------------

.cols_rename <- c(
  "code_tract" = "cd_setor",
  "renda_resp" = "rendimento_responsavel_dom_part_ocu",
  "dom_ocu" = "dom_part_ocupados"
)

setores22 <- setores22 |>
  rename(all_of(.cols_rename)) |>
  mutate(
    code_muni = as.numeric(stringr::str_sub(code_tract, 1, 7)),
    dom_ocu = as.numeric(dom_ocu),
    # Valor do rendimento nominal médio mensal das pessoas responsáveis
    # com rendimentos por domicílios particulares permanentes ocupados
    renda_resp = as.numeric(renda_resp),
    renda_total = dom_ocu * renda_resp
  )


grid_spo <- build_income_grid(setores22, code_muni = 3550308)

plot_income_grid(grid_spo, code_muni = 3550308, year_select = "2022")
plot_income_grid(grid_spo, code_muni = 3550308, year_select = "2010")

geobr::read_statistical_grid(year = 2022, 4314902)


build_grid <- function(code, year = 2022) {
  stopifnot(year %in% c(2010, 2022))

  grid <- geobr::read_statistical_grid(year = year, code_muni = code)

  # Padronizar nome do id
  if ("id_unico" %in% names(grid)) {
    grid <- dplyr::rename(grid, gid = id_unico)
  } else {
    grid <- dplyr::mutate(grid, gid = dplyr::row_number())
  }

  return(grid)
}
