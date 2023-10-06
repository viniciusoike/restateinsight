library(aopdata)
library(tmap)
library(tmaptools)
library(dplyr)
library(sf)
library(purrr)
library(modelr)
library(tidyr)

tmap_mode(mode = "view")

poa <- aopdata::read_population("poa", geometry = TRUE)

tm_shape(poa) +
  tm_fill(col = "P001", style = "fisher", n = 7, alpha = 0.5, id = "id_hex") +
  tm_basemap(server = "CartoDB.Positron")

# Get a hex to serve as CBD
hex_cbd <- "89a901288cbffff"

# Simplifying assumption: all hexagons are equal size
# Compute the area (km2) of the CBD hexagon
hex_area <- poa %>%
  filter(id_hex == hex_cbd) %>%
  st_transform(crs = 32722) %>%
  st_area() %>%
  units::set_units(value = "km^2")

# Get the centroid coordinates of the CBD hexagon
cbd <- poa %>%
  filter(id_hex == hex_cbd) %>%
  st_centroid() %>%
  st_transform(crs = 32722)

# Compute the distance (km) of each hexagon to the CBD hexagon
poa <- poa %>%
  st_transform(crs = 32722) %>%
  mutate(
    d = st_distance(., cbd),
    d = units::set_units(d, "km")) %>%
  st_transform(crs = 4674)

# Plot to check consistency
tm_shape(poa) +
  tm_fill(col = "d", style = "fisher", n = 7, alpha = 0.5, id = "id_hex") +
  tm_basemap(server = "CartoDB.Positron")

# Compute population density, select columns, and remove NAs
xreg <- poa %>%
  mutate(
    density = P001 / as.numeric(hex_area),
    d = as.numeric(d)) %>%
  select(density, d) %>%
  filter(density > 0) %>%
  na.omit()

fit_poa <- lm(log(density) ~ d, data = xreg)
summary(fit_poa)

# Repeat for Curitiba and Fortaleza

cur <- aopdata::read_population("cur", geometry = TRUE)

tm_shape(cur) +
  tm_fill(col = "P001", style = "fisher", n = 7, alpha = 0.5, id = "id_hex") +
  tm_basemap(server = "CartoDB.Positron")

# Get a hex to serve as CBD
hex_cbd <- "89a83136e6bffff"

# Simplifying assumption: all hexagons are equal size
# Compute the area (km2) of the CBD hexagon
hex_area <- cur %>%
  filter(id_hex == hex_cbd) %>%
  st_transform(crs = 32722) %>%
  st_area() %>%
  units::set_units(value = "km^2")

# Get the centroid coordinates of the CBD hexagon
cbd <- cur %>%
  filter(id_hex == hex_cbd) %>%
  st_centroid() %>%
  st_transform(crs = 32722)

# Compute the distance (km) of each hexagon to the CBD hexagon
cur <- cur %>%
  st_transform(crs = 32722) %>%
  mutate(
    d = st_distance(., cbd),
    d = units::set_units(d, "km")) %>%
  st_transform(crs = 4674)

# Plot to check consistency
tm_shape(cur) +
  tm_fill(col = "d", style = "fisher", n = 7, alpha = 0.5, id = "id_hex") +
  tm_basemap(server = "CartoDB.Positron")

# Compute population density, select columns, and remove NAs
xreg <- cur %>%
  mutate(
    density = P001 / as.numeric(hex_area),
    d = as.numeric(d)) %>%
  select(density, d) %>%
  filter(density > 0) %>%
  na.omit()

fit_cur <- lm(log(density) ~ d, data = xreg)

summary(fit_cur)

fortaleza <- aopdata::read_population("for", geometry = TRUE)

tm_shape(fortaleza) +
  tm_fill(col = "P001", style = "fisher", n = 7, alpha = 0.5, id = "id_hex") +
  tm_basemap(server = "CartoDB.Positron")

# Get a hex to serve as CBD
hex_cbd <- "8980104cccbffff"

# Simplifying assumption: all hexagons are equal size
# Compute the area (km2) of the CBD hexagon
hex_area <- fortaleza %>%
  filter(id_hex == hex_cbd) %>%
  st_transform(crs = 32722) %>%
  st_area() %>%
  units::set_units(value = "km^2")

# Get the centroid coordinates of the CBD hexagon
cbd <- fortaleza %>%
  filter(id_hex == hex_cbd) %>%
  st_centroid() %>%
  st_transform(crs = 32722)

# Compute the distance (km) of each hexagon to the CBD hexagon
fortaleza <- fortaleza %>%
  st_transform(crs = 32722) %>%
  mutate(
    d = st_distance(., cbd),
    d = units::set_units(d, "km")) %>%
  st_transform(crs = 4674)

# Plot to check consistency
tm_shape(fortaleza) +
  tm_fill(col = "d", style = "fisher", n = 7, alpha = 0.5, id = "id_hex") +
  tm_basemap(server = "CartoDB.Positron")

# Compute population density, select columns, and remove NAs
xreg <- fortaleza %>%
  mutate(
    density = P001 / as.numeric(hex_area),
    d = as.numeric(d)) %>%
  select(density, d) %>%
  filter(density > 0) %>%
  na.omit()

fit_fortaleza <- lm(log(density) ~ d, data = xreg)

models <- tibble(
  name_city = c("Porto Alegre", "Curitiba", "Fortaleza"),
  model = list(fit_poa, fit_cur, fit_fortaleza)
)

models %>%
  mutate(coefs = map(model, coef)) %>%
  unnest(coefs)

plot(density ~ d, data = filter(xreg, density > 100, density < 10000))


# metro area --------------------------------------------------------------

library(geobr)
library(sidrar)

info_sidra(4714)

city_density <- get_sidra(4714, variable = 614, geo = "City")

city_density <- city_density %>%
  janitor::clean_names() %>%
  select(code_muni = municipio_codigo, density = valor) %>%
  mutate(code_muni = as.numeric(code_muni))

metro <- read_metro_area()

rmpoa <- metro %>%
  filter(name_metro == "RM Porto Alegre")

rmpoa <- rmpoa %>%
  left_join(city_density, by = "code_muni")

cbd <- rmpoa %>%
  filter(name_muni == "Porto Alegre") %>%
  st_transform(crs = 32722) %>%
  st_centroid()

rmpoa <- rmpoa %>%
  st_transform(crs = 32722) %>%
  mutate(
    distance = st_distance(., cbd),
    distance = units::set_units(distance, "km")
    ) %>%
  st_transform(crs = 4674)

tm_shape(rmpoa) +
  tm_fill(col = "density", alpha = 0.8, style = "jenks", n = 5) +
  tm_borders()

fit <- lm(log(density) ~ distance, data = rmpoa)
summary(fit)
plot(log(density) ~ distance, data = rmpoa)
abline(coef(fit))

plot(density ~ distance, data = rmpoa)
lines(exp(coef(fit)[1]) * exp(seq(1:60) * coef(fit)[2]))

# Many outliers. Consider the north corridor only!
north <- c(
  4314902, 4304606, 4307708, 4320008, 4318705, 4313409, 4307609, 4310801, 4306403,
  4319901, 4313060, 4300877, 4303905, 4313409, 4309209, 4313103, 4300604, 4303103)

rmpoa <- mutate(rmpoa, is_north = factor(ifelse(code_muni %in% north, 1L, 0L)))

tm_shape(rmpoa) +
  tm_fill(col = "is_north", alpha = 0.8) +
  tm_borders()

npoa <- filter(rmpoa, is_north == 1L)
# Much better fit!
fit <- lm(log(density) ~ distance, data = npoa)
summary(fit)
plot(log(density) ~ distance, data = npoa)
abline(coef(fit))

plot(density ~ distance, data = npoa)
lines(exp(coef(fit)[1]) * exp(seq(1:60) * coef(fit)[2]))





