library(geobr)
library(sf)
library(dplyr)

code_cur <- 4106902
border <- read_municipality(code_cur, simplified = FALSE)
sg <- read_statistical_grid("PR")

cur_sg <- st_join(sg, border)
cur_sg <- dplyr::filter(cur_sg, !is.na(code_muni))

qs::qsave(cur_sg, here::here("static/data/statistical_grid_cur.qs"))
