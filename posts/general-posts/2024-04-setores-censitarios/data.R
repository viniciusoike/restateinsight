library(here)
library(dplyr)
library(sf)

file_dir <- "/Volumes/T7 Touch/github/tidyibge/data-raw/dados_setores_censitarios"

dat <- data.table::fread(
  paste0(file_dir, "/Agregados_preliminares_por_setores_censitarios_BR.zip")
)

setores <- sf::st_read(paste0(file_dir, "/BR_Malha_Preliminar_2022.gpkg"))

dict <- tibble(
  name_variable = paste0("v000", 1:7),
  desc_variable = c(
    "Total de Pessoas", "Total de Domicilios", "Total de Domicilios Particulares",
    "Total de Domicilios Coletivos", "Media de moradores em domicilios particulares ocupados",
    "Percentual de domicilios imputados", "Total de domicilios particulares ocupados"),
  short_name = c("pop", "dom", "dom_prt", "dom_col", "pop_dom", "share_dom_imput", "dom_prt_ocup")
)

rename_cols <- dict$name_variable
names(rename_cols) <- dict$short_name

vl_names <- c(
  "code_tract" = "CD_SETOR",
  "area_km2" = "AREA_KM2",
  "code_region" = "CD_REGIAO",
  "name_region" = "NM_REGIAO",
  "code_state" = "CD_UF",
  "name_state" = "NM_UF",
  "code_muni" = "CD_MUN",
  "name_muni" = "NM_MUN",
  "code_district" = "CD_DIST",
  "name_district" = "NM_DIST",
  "code_subdistrict" = "CD_SUBDIST",
  "name_subdistrict" = "NM_SUBDIST",
  "code_microregion" = "CD_MICRO",
  "name_microregion" = "NM_MICRO",
  "code_mesoregion" = "CD_MESO",
  "name_mesoregion" = "NM_MESO",
  "code_immediate" = "CD_RGI",
  "name_immediate" = "NM_RGI",
  "code_intermediate" = "CD_RGINT",
  "name_intermediate" = "NM_RGINT",
  "code_urban_concentration" = "CD_CONCURB",
  "name_urban_concentration" = "NM_CONCURB"
)

rename_cols <- c(rename_cols, vl_names)

dat <- dat %>%
  rename(all_of(rename_cols)) %>%
  select(
    code_tract, code_muni, name_muni, code_region, name_region, code_state,
    name_state, code_district, name_district, code_district, name_district,
    pop:dom_prt_ocup
  ) |> 
  mutate(across(pop:dom_prt_ocup, as.numeric))

setores <- setores %>%
  rename(any_of(rename_cols)) %>%
  mutate(across(pop:dom_prt_ocup, as.numeric))

setores2000 <- read_census_tract(
  code_tract = "all",
  year = 2000,
  showProgress = FALSE
  )

setores2010 <- read_census_tract(
  code_tract = "all",
  year = 2010,
  showProgress = FALSE
  )

sapply(list(setores2000, setores2010, setores), nrow)

sapply(list(setores2000, setores2010, setores), \(x) length(unique(x$code_muni)))

tab_census <- tribble(
  ~year, ~n_tract, ~n_city, ~pop,
  2000, 254855, 1058, 169590693,
  2010, 316545, 5565, 190755799,
  2022, 454047, 5571, 203080756
)

spo_setores_2010 <- filter(setores2010, code_muni == 3550308)
spo_setores_2022 <- filter(setores, code_muni == 3550308)
cur_setores_2010 <- filter(setores2010, code_muni == 4106902)
cur_setores_2022 <- filter(setores, code_muni == 4106902)

st_write(spo_setores_2010, here("static/data/census/spo_setores_2010.gpkg"))
st_write(spo_setores_2022, here("static/data/census/spo_setores_2022.gpkg"))
st_write(cur_setores_2010, here("static/data/census/cur_setores_2010.gpkg"))
st_write(cur_setores_2022, here("static/data/census/cur_setores_2022.gpkg"))

distritos <- st_read(here("/Volumes/T7 Touch/github/tidyibge/data/SP_Malha_Preliminar_Distrito_2022.gpkg"))

spo_distritos <- distritos %>%
  filter(CD_MUN == 3550308)

st_write(spo_distritos, here("static/data/census/spo_distritos_2022.gpkg"))
