library(readr)
library(dplyr)
library(lubridate)

temp = read_csv2(here::here("static/data/temperatura_poa_2023.csv"))

temp = temp |> 
  janitor::clean_names() |> 
  mutate(
    date_ymd = parse_date(data, format = "%d/%m/%Y"),
    date_ymdhm = lubridate::as_datetime(paste(date_ymd, hora_utc), format = "%Y-%m-%d %H%M"),
    dia = lubridate::day(date_ymd),
    hora = lubridate::hour(date_ymdhm),
    mes = lubridate::month(date_ymd),
    qtr = lubridate::quarter(date_ymd),
    .before = everything()
    )

readr::write_rds(temp, here::here("static/data/temperatura_poa_2023.rds"))

# temp_daily = temp |> 
#   select(date_ymd, temp_ins_c) |> 
#   summarise(temp_avg = mean(temp_ins_c, na.rm = TRUE), .by = "date_ymd")
# 
# library(forecast)
# 
# x = ts(temp_daily$temp_avg, start = c(2023, 1), frequency = 365)
# 
# ggseasonplot(x, s = lubridate::month(temp_daily$date_ymd))
# 
# feasts::gg_season(x, period = "week")
# 
# y = tsibble::as_tsibble(x)
# y = tsibble::fill_gaps(y)
# 
# feasts::gg_season(y, period = "week")
# 
# temp_daily$month = lubridate::month(temp_daily$date_ymd)
# temp_daily$week = lubridate::week(temp_daily$date_ymd)
# temp_daily$day = lubridate::week(temp_daily$date_ymd)
# 
# ggplot(dplyr::filter(temp_daily, month <= 3), aes(x = week))