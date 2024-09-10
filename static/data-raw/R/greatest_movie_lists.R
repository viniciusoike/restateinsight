library(tidyverse)
library(rvest)

get_imdb_list <- function(url) {
  
  page = read_html(url)
  
  info_film = page |>
    html_elements(xpath = '//span[@class="lister-item-header"]') |>
    html_text2()
  
  rating = page |>
    html_elements(xpath = '//div[@class="col-imdb-rating"]') |>
    html_text2()
  
  film = tibble(info_film = info_film, rating = rating)
  
  film = film |> 
    mutate(
      rank = as.integer(str_extract(info_film, "^[0-9]{1,3}(?=\\.)")),
      name_film = str_extract(info_film, "(?<=[0-9]{1,3}\\. ).+(?= \\([0-9]{4})"),
      year = as.integer(str_extract(info_film, "(?<=\\()[0-9]{4}")),
      rating = as.numeric(rating)
    )
  
  return(film)
  
}

import_letterboxd_list <- function(url) {
  
  letterboxd <- read_html(url)
  
  films = letterboxd |> 
    html_elements(xpath = "//div[@class='film-detail-content']") |> 
    html_text2()
  
  df_films = films |> 
    stringr::str_split(pattern = "\n", simplify = TRUE) |> 
    as_tibble()
  
  clean_letterboxd <- df_films |> 
    mutate(
      rank = as.integer(str_extract(V1, "[0-9]?+\\.")),
      year = as.integer(str_extract(V1, "[0-9]{4}")),
      name_film = str_extract(V1, "(?<=\\.).+(?= [0-9]{4})"),
      year_list = 2024L
    ) |> 
    rename(info_film = V1, rating_stars = V3) |> 
    select(info_film, rank, name_film, year, year_list)
  
  return(clean_letterboxd)
  
}

## AFI 100 Years 100 Films -------------------------------------------------
# 
# https://www.afi.com/afis-100-years-100-movies-10th-anniversary-edition/

url = "https://www.afi.com/afis-100-years-100-movies-10th-anniversary-edition/"

raw_entries <- read_html(url) |>
  html_elements(xpath = "//h6") |>
  html_text()
  
afi <- tibble(
  raw = raw_entries
)

clean_afi <- afi |> 
  mutate(
    rank = as.integer(str_extract(raw, "\\d+\\.")),
    name_film = str_extract(raw, "(?<=\\. )([A-z0-9].+)(?= \\()"),
    year = as.integer(str_extract(raw, "[0-9]{4}")),
    list_year = 2007L
  ) |> 
  distinct() |> 
  select(rank, name_film, year)

# From IMDB

url = sprintf(
  "https://www.imdb.com/list/ls000093307/?sort=list_order,asc&st_dt=&mode=simple&page=%s&ref_=ttls_vw_smp",
  1
)

imdb_afi = get_imdb_list(url)


## BFI The Greatest Films of All Time --------------------------------------
# 
# https://www.bfi.org.uk/sight-and-sound/greatest-films-all-time

url = "https://www.bfi.org.uk/sight-and-sound/greatest-films-all-time"

page = read_html(url)

name_film = page |>
  html_elements(xpath = "//article/a/h1") |>
  html_text()

year_country = page |>
  html_elements(xpath = "//article/a/p[1]") |>
  html_text()

director = page |>
  html_elements(xpath = "//article/a/p[2]") |>
  html_text()

rank = page |>
  html_elements(xpath = "//article/a/div/p[1]") |>
  html_text()

bfi = tibble(
  rank = rank,
  name_film = name_film,
  year_country = year_country,
  director = director
)

clean_bfi = bfi |> 
  mutate(
    rank = as.integer(str_remove(rank, "=")),
    name_film = str_trim(str_to_title(name_film)),
    director = str_remove(director, "Directed by "),
    list_year = 2021L
  ) |> 
  separate(
    col = "year_country",
    into = c("year", "country"),
    sep = " ",
    extra = "merge"
    ) |> 
  mutate(
    year = as.integer(year))

# From IMDB: https://www.imdb.com/list/ls566332623/

url = sprintf(
  "https://www.imdb.com/list/ls566332623/?sort=list_order,asc&st_dt=&mode=simple&page=%s&ref_=ttls_vw_smp",
  1:4
)

imdb_bfi = map(url, get_imdb_list)

imdb_bfi = bind_rows(imdb_bfi)
imdb_bfi = mutate(imdb_bfi, list_year = 2022L)

## IMDB's top 250 ----------------------------------------------------------
# 
# https://www.imdb.com/chart/top/

url = "https://www.imdb.com/chart/top/"

page = read_html(url)

name_film = page |>
  html_elements(xpath = "//a/h3") |>
  html_text()

info_film = read_html(url) |>
  html_elements(xpath = "//div/div/div[2]") |>
  html_text()

info_film = info_film[nchar(info_film) > 1]
info_film = info_film[str_detect(info_film, "^[0-9]")]

name_film = name_film[str_detect(name_film, "^[0-9]{1,3}\\.")]

rating = page |> 
  html_elements(xpath = '//span[@data-testid="ratingGroup--imdb-rating"]') |> 
  html_text()

imdb = tibble(
  name_year = name_film,
  info_film = info_film,
  raw_rating = rating
)

clean_imdb = imdb |> 
  mutate(
    rank = as.integer(str_extract(name_year, "^[0-9]{1,3}(?=\\.)")),
    name_film = str_extract(name_year, "(?<=[0-9]{1,3}\\. ).+"),
    year = as.integer(str_extract(info_film, "^[0-9]{1,4}")),
    film_time = str_extract(info_film, "(?<=[0-9]{4})(.+)(m)"),
    film_time = if_else(
      is.na(film_time),
      str_extract(info_film, "(?<=[0-9]{4})[0-9]?+h"),
      film_time),
    mpaa_rating = str_extract(info_film, "(?<=m).+"),
    mpaa_rating = if_else(
      is.na(mpaa_rating),
      str_remove(info_film, str_c(year, film_time)),
      mpaa_rating
    ),
    rating = as.numeric(str_extract(raw_rating, "[0-9]\\.[0-9]")),
    num_votes = str_extract(raw_rating, "(?<=\\().+(?=\\))"),
    year_list = 2024L
  )

clean_imdb = clean_imdb |> 
  select(rank, name_film, year, film_time, rating, year_list)

imdb_top250 = clean_imdb

## Ebert's Great Movies ----------------------------------------------------

# Extracted from https://www.imdb.com/list/ls051203792/

url = sprintf(
  "https://www.imdb.com/list/ls051203792/?sort=list_order,asc&st_dt=&mode=simple&page=%s&ref_=ttls_vw_smp",
  1:4
  )

ebert = map(url, get_imdb_list)
imdb_ebert = bind_rows(ebert)
# Couldn't find year of publication. Used the year of the most recent film
# included in the list
imdb_ebert = mutate(imdb_ebert, list_year = 2009L)


## Empire's 500 Greatest Movies --------------------------------------------
# 
# Extracted from https://www.imdb.com/list/ls003073623/

url = sprintf(
  "https://www.imdb.com/list/ls003073623/?sort=list_order,asc&st_dt=&mode=simple&page=%s&ref_=ttls_vw_smp",
  1:5
)

empire = map(url, get_imdb_list)
imdb_empire = bind_rows(empire)
imdb_empire = mutate(imdb_empire, year_list = 2008L)


## Variety Top 100 ---------------------------------------------------------

url = "https://variety.com/lists/best-movies-of-all-time/the-graduate-1967-2/"

info_film = read_html(url) |> 
  html_elements(xpath = '//h2') |>
  html_text()
  
info_film = info_film[1:100]

variety = tibble(info_film = info_film)

clean_variety = variety |> 
  mutate(
    name_film = str_extract(info_film, ".+(?= \\([0-9]{4}\\))"),
    year = as.integer(str_extract(info_film, "(?<=\\()[0-9]{4}(?=\\))")),
    year_list = 2022L
  )

# From IMDB
# 
# https://www.imdb.com/list/ls566858343/

url = "https://www.imdb.com/list/ls566858343/?sort=list_order,asc&st_dt=&mode=simple&page=1&ref_=ttls_vw_smp"

imdb_variety = get_imdb_list(url)


# TimeOut -----------------------------------------------------------------
# https://www.timeout.com/film/best-movies-of-all-time

# From IMDB
# 
# https://www.imdb.com/list/ls538972757/

url = "https://www.imdb.com/list/ls538972757/?sort=list_order,asc&st_dt=&mode=simple&page=1&ref_=ttls_vw_smp"

imdb_timeout = get_imdb_list(url)


# Letterboxd --------------------------------------------------------------

url = str_glue(
  "https://letterboxd.com/dave/list/official-top-250-narrative-feature-films/detail/page/{1:3}/"
)

list_letterboxd = purrr::map(url, import_letterboxd_list)
clean_letterboxd = bind_rows(list_letterboxd)

# Criticker ---------------------------------------------------------------

url = sprintf(
  "https://letterboxd.com/kalltkaffe/list/top-250-from-critickercom/detail/page/%s/",
  1:3
)

list_criticker = map(url, import_letterboxd_list)
clean_criticker = bind_rows(list_criticker)

# Rolling Stones Top 150 Sci-fi -------------------------------------------

url = sprintf(
  "https://www.imdb.com/list/ls522190382/?sort=list_order,asc&st_dt=&mode=simple&page=%s&ref_=ttls_vw_smp",
  1:2
)

rolling_stone = map(url, get_imdb_list)
imdb_rs_scifi = bind_rows(rolling_stone)
imdb_rs_scifi = mutate(imdb_rs_scifi, year_list = 2008L)

# BBC ---------------------------------------------------------------------

## Top 100 Movies Female Directors -----------------------------------------

url = "https://www.imdb.com/list/ls091195303/?sort=list_order,asc&st_dt=&mode=simple&page=1&ref_=ttls_vw_smp"

imdb_bbc_women = get_imdb_list(url)


## Greatest Films 21st Century ---------------------------------------------

url = "https://www.imdb.com/list/ls025871744/?sort=list_order,asc&st_dt=&mode=simple&page=1&ref_=ttls_vw_smp"

imdb_bbc_21st = get_imdb_list(url)

## Greatest American Films -------------------------------------------------

url = "https://www.imdb.com/list/ls074254619/?sort=list_order,asc&st_dt=&mode=simple&page=1&ref_=ttls_vw_smp"

imdb_bbc_american = get_imdb_list(url)

## Greatest Foreign Films --------------------------------------------------

url = "https://www.imdb.com/list/ls045753117/?sort=list_order,asc&st_dt=&mode=simple&page=1&ref_=ttls_vw_smp"

imdb_bbc_foreign = get_imdb_list(url)


# Top 100 Brazilian Movies ------------------------------------------------
#
# Extracted from https://abraccine.org/2015/11/27/abraccine-organiza-ranking-dos-100-melhores-filmes-brasileiros/

url = "https://abraccine.org/2015/11/27/abraccine-organiza-ranking-dos-100-melhores-filmes-brasileiros/"

info_film = read_html(url) |> 
  html_element(xpath = '//*[@id="post-3322"]/div/p[9]') |> 
  html_text() |> 
  str_split(pattern = "\n")

abracine = tibble(info_film = unlist(info_film))

clean_abracine = abracine |> 
  mutate(
    rank = as.integer(str_extract(info_film, "^[0-9]{1,3}(?=\\.)")),
    name_film = str_extract(info_film, "(?<=\\. ).+(?= \\()"),
    year = as.integer(str_extract(info_film, "(?<=\\()[0-9]{4}(?=\\))")),
    director = str_extract(info_film, "(?<=, de ).+"),
    list_year = 2015L
  ) |> 
  select(rank, name_film, year, director)


# Join lists --------------------------------------------------------------

# dim_title = data.table::fread("/Volumes/T7 Touch/bases-de-dados/imdb/title.basics.tsv.gz")
# 
# ratings = data.table::fread("/Volumes/T7 Touch/bases-de-dados/imdb/title.ratings.tsv.gz")
# 
# dim_title <- dim_title |> 
#   filter(titleType == "movie") |> 
#   mutate(startYear = as.integer(startYear))
# 
# clean_empire <- clean_empire |> 
#   left_join(dim_title, by = c("name_film" = "primaryTitle", "year" = "startYear")) |> 
#   left_join(ratings, by = "tconst")

films <- list(
  afi = imdb_afi,
  bfi = imdb_bfi,
  ebert = imdb_ebert,
  empire = imdb_empire,
  variety = imdb_variety,
  timeout = imdb_timeout,
  bbc_american = imdb_bbc_american,
  bbc_women = imdb_bbc_women,
  bbc_foreign = imdb_bbc_foreign,
  bbc_21st = imdb_bbc_21st,
  imdb = imdb_top250,
  rolling_stones_scifi = imdb_rs_scifi,
  letterboxd = clean_letterboxd,
  criticker = clean_criticker
  )

dat <- bind_rows(films, .id = "source")

qs::qsave(dat, here::here("static/data/greatest_movies.qs"))

# Viz ---------------------------------------------------------------------

# imdb_bfi |> 
#   bind_rows() |> 
#   left_join(dim_title, by = c("name_film" = "primaryTitle", "year" = "startYear")) |> 
#   left_join(ratings, by = "tconst") |> 
#   filter(is.na(averageRating))
# 
# clean_bfi |> 
#   mutate(decade = year - (year %% 10)) |> 
#   ggplot(aes(x = decade)) +
#   geom_bar()
# 
# clean_imdb |> 
#   mutate(decade = year - (year %% 10)) |> 
#   ggplot(aes(x = decade)) +
#   geom_bar()
# 
# ggplot(clean_empire, aes(x = year)) +
#   geom_histogram(bins = 12)
# 
# 
# ggplot(clean_bfi, aes(x = year)) +
#   geom_histogram(bins = 12, color = "white")
# 
# ggplot(clean_imdb, aes(x = year)) +
#   geom_histogram(bins = 15)
# 
# ggplot(clean_variety, aes(x = year)) +
#   geom_histogram(bins = 11, color = "white") +
#   geom_hline(yintercept = 0) +
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank()
#   )
# 
# hist(clean_variety$year, breaks = "fd")
# 
# ggplot(clean_afi, aes(x = year)) +
#   geom_histogram(bins = 15)
# 
# ggplot(clean_empire, aes(x = year)) +
#   geom_histogram(bins = 15)
# 
# ggplot(clean_ebert, aes(x = year)) +
#   geom_histogram(bins = 15)
# 
# 
# 
# ggplot(clean_abracine, aes(x = year)) +
#   geom_histogram(bins = 15)
# 
# clean_empire |> 
#   mutate(
#     decade = year - (year %% 10)
#   ) |> 
#   ggplot(aes(x = decade, y = averageRating, group = decade)) +
#   geom_violin()
# 
# 
# clean_empire |> 
#   mutate(
#     decade = year - (year %% 10)
#   ) |> 
#   ggplot(aes(x = decade, y = averageRating, group = decade)) +
#   geom_count()
# 
# clean_empire |> 
#   filter(averageRating < 7) |> 
#   arrange(averageRating)


# Colors
# imdb = "#deb522"
# variety = "#1a282f", "#546464"
# bfi = "#000000"
# empire = "#ff0000"
# 
# imdb_afi <- imdb_afi[[1]]
# 
# imdb_bfi <- bind_rows(imdb_bfi)
# 
# dat <- list(imdb = clean_imdb, empire = imdb_empire, afi = imdb_afi, bfi = imdb_bfi, variety = imdb_variety, timeout = imdb_timeout)
# 
# dat <- bind_rows(dat, .id = "source")
# 
# dat <- dat |> 
#   mutate(
#     source = factor(
#       source,
#       levels = c("afi", "bfi", "timeout", "variety", "empire", "imdb"),
#       labels = c("AFI", "BFI", "TimeOut", "Variety", "Empire", "IMDB"))
#   )
# 
# highlight_low = dat |> 
#   group_by(source) |> 
#   slice_min(rating, n = 3)
# 
# ggplot(dat, aes(source, rating, color = source)) +
#   geom_jitter(width = 0.4, alpha = 0.75) +
#   ggrepel::geom_text_repel(data = highlight_low, aes(label = name_film), size = 2) +
#   guides(color = "none") +
#   theme_minimal()
# 
# 
# 
# library(ggridges)
# 
# ggplot(dat, aes(year, source, fill = source)) +
#   geom_density_ridges(alpha = 0.5) +
#   scale_x_continuous(limits = c(1900, 2024)) +
#   guides(fill = "none") +
#   theme_minimal()
