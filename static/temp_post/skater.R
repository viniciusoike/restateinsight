# https://citygeographics.org/2022/12/15/tracking-gentrification-in-london-and-manchester-using-the-2021-census-occupational-class-data/

# https://www.ucl.ac.uk/bartlett/casa/publications/2023/oct/casa-working-paper-234

# https://citygeographics.org/2023/10/23/can-the-green-belt-be-developed-sustainably-to-ease-londons-housing-crisis/

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(srvyr)
library(sf)
library(tidypod)
sf::sf_use_s2(FALSE)
library(here)

pod <- haven::read_sav(here("static/data/OD_2017_v1.sav"))

pod <- pod |> 
  filter(!is.na(qt_auto)) |> 
  mutate(
    is_zero_car = factor(if_else(qt_auto == 0, 1L, 0L))
  )

pod <- design_pod(pod, id = "domi")
pod <- as_survey(pod)

tbl_total_auto <- pod |> 
  rename(code_zone = zona) |> 
  group_by(code_zone) |> 
  survey_count(qt_auto) |> 
  ungroup()

tbl_cars <- tbl_total_auto |> 
  mutate(share = n / sum(n) * 100, .by = "code_zone") |> 
  filter(qt_auto == 0) |> 
  select(code_zone, share_no_car = share)

tbl_timetravel <- pod |> 
  filter(modoprin %in% c(9, 10)) |> 
  rename(code_zone = zona) |> 
  group_by(code_zone) |> 
  summarise(avg_time = survey_mean(duracao))

zones <- tidypod::zones

tbl_zone_area <- zones |> 
  st_drop_geometry() |> 
  select(code_zone, area_ha)

tbl_zone_pop <- tbl_pod |> 
  select(code_zone, population)

tbl_zone_pop <- left_join(tbl_zone_pop, tbl_zone_area, by = "code_zone")

tbl_pop <- tbl_zone_pop |> 
  mutate(pop_density = population / area_ha) |> 
  select(code_zone, population, pop_density)

transport <- read_csv(here("data/sp-pod/fact_travel_transport_mode.csv"))

tbl_transport_mode <- transport |> 
  mutate(
    group_mode = case_when(
      transport_mode %in% c("A pé", "Bicicleta") ~ "walk",
      str_detect(transport_mode, "Táxi|automóvel") ~ "car",
      transport_mode %in% c("Metrô", "Trem", "Ônibus") ~ "transit",
      TRUE ~ "other"
    )
  )

tbl_agg_mode <- tbl_transport_mode |> 
  summarise(
    total_mode = sum(ntravel),
    .by = c("code_zone", "group_mode")
  ) |> 
  mutate(share_mode = total_mode / sum(total_mode) * 100, .by = "code_zone")

tbl_agg_mode <- tbl_agg_mode |> 
  filter(group_mode != "other") |> 
  pivot_wider(
    id_cols = "code_zone",
    names_from = "group_mode",
    values_from = "share_mode"
  )

tbl_travels <- tbl_agg_mode |> 
  left_join(tbl_cars, by = "code_zone") |>
  left_join(tbl_timetravel, by = "code_zone") |> 
  left_join(tbl_pop, by = "code_zone") |> 
  filter(population > 0)

scale_zero_one <- function(x) {
  
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
}

tbl_skater <- tbl_travels |> 
  select(-population, -avg_time_se, -avg_time) |> 
  mutate(pop_density = log(pop_density)) |> 
  mutate(across(-code_zone, ~as.numeric(scale_zero_one(.x)))) |> 
  filter(if_any(everything(), ~!is.na(.x))) |> 
  mutate(car = 1 - car)

tbl_skater <- tbl_skater %>%
  rowwise() %>%
  mutate(index = mean(c_across(transit:pop_density)) * 100)

GGally::ggpairs(tbl_skater[, -1])

spzones <- zones |> 
  filter(code_zone %in% unique(tbl_travels$code_zone)) |> 
  select(code_zone) |> 
  st_make_valid() |> 
  as_Spatial()

library(spdep)

nbs <- poly2nb(spzones)
costs <- nbcosts(nbs, data = tbl_skater[, -1])
tree <- mstree(nb2listw(nbs, costs, style = "B", zero.policy = TRUE))

clus6 <- skater(tree[, 1:2], data = tbl_skater[, -1], ncuts = 5)
clus7 <- skater(tree[, 1:2], data = tbl_skater[, -1], ncuts = 6)
clus8 <- skater(tree[, 1:2], data = tbl_skater[, -1], ncuts = 7)
clus10 <- skater(tree[, 1:2], data = tbl_skater[, -1], ncuts = 9)

tbl_cluster <- tbl_travels |> 
  mutate(
    cluster6 = factor(clus6$groups),
    cluster7 = factor(clus7$groups),
    cluster8 = factor(clus8$groups),
    cluster10 = factor(clus10$groups)
  )

sf_cluster <- left_join(select(zones, code_zone, name_zone), tbl_cluster, by = "code_zone")

sf_cluster <- left_join(select(zones, code_zone, name_zone), tbl_skater, by = "code_zone")

library(ggplot2)
library(tmap)
library(tmaptools)
tmap_mode(mode = "view")

tm_shape(sf_cluster) +
  tm_fill(
    col = "index",
    breaks = c(10, 40, 50, 60, 70, 80, 100),
    palette = "RdBu", 
    id = "name_zone",
    popup.vars = c("share_no_car", "car", "walk"),
    popup.format = list(digits = 2)
  ) +
  tm_borders() +
  tm_basemap(server = "CartoDB.Positron")


tm_shape(sf_cluster) +
  tm_fill(
    col = "share_no_car",
    breaks = c(0, 15, 30, 45, 60, 75, 90),
    palette = "RdBu", 
    id = "name_zone",
    popup.vars = c("share_no_car", "car", "walk"),
    popup.format = list(digits = 2)
  ) +
  tm_borders() +
  tm_basemap(server = "CartoDB.Positron")

tm_shape(sf_cluster) +
  tm_fill(
    col = "car",
    breaks = c(0, 15, 25, 30, 35, 45, 65),
    palette = "-RdBu", 
    id = "name_zone",
    popup.vars = c("share_no_car", "car", "walk"),
    popup.format = list(digits = 2)
  ) +
  tm_borders() +
  tm_basemap(server = "CartoDB.Positron")

ggplot(sf_cluster) +
  geom_sf(aes(fill = cluster8)) +
  scale_fill_viridis_d()

cols <- c("walk", "transit", "car", "share_no_car", "pop_density")

tbl_summary <- tbl_cluster |> 
  summarise(
    across(cols, ~weighted.mean(.x, population)),
    .by = "cluster8"
  ) |> 
  arrange(share_no_car)

tbl_pop <- tbl_cluster |> summarise(total_pop = sum(population), .by = "cluster8")

tbl_summary <- left_join(tbl_summary, tbl_pop)

tbl_equiv <- tbl_summary |> 
  mutate(group_id = factor(rank(-share_no_car))) |> 
  select(cluster8, group_id)

sf_cluster <- sf_cluster |> 
  left_join(tbl_equiv, by = "cluster8")

ggplot(sf_cluster) +
  geom_sf(aes(fill = group_id)) +
  scale_fill_brewer(type = "div", direction = -1, palette = "RdBu")

tbl_summary |> 
  left_join(tbl_equiv) |> 
  arrange(group_id) |> 
  select(-cluster8)


tbl_skater