library(rvest)
library(stringr)
# Change for a static version of this data
url <- "https://250.took.nl/compare/full"
page <- xml2::read_html(url)
page_tables <- rvest::html_table(page)
tab <- page_tables[[9]]
wr = function(R, v, m = 25000, C) {
  (v / (v + m)) * R + (m / (v + m)) * C
}
imdb <- tab |> 
  janitor::clean_names() |> 
  dplyr::select(rank_imdb = rank, rating, title, votes) |> 
  dplyr::mutate(
    year = as.numeric(str_extract(title, "(?<=\\()[0-9]{1,4}(?=\\))")),
    votes = as.numeric(str_remove_all(votes, ",")),
    wr = wr(rating, votes, C = mean(rating)),
    rank = rank(-wr)
  )

qs::qsave(imdb, here::here("static/data/imdb_top_250.qs"))
