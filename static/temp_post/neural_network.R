library(forecast)

autoplot(sunspot.month)

fit <- nnetar(sunspot.month, P = 7, lambda = "auto", size = 16, repeats = 30)

autoplot(forecast(fit, h = 24), include = 84)

sunspot.month

sbpe <- rbcb::get_series(239, start_date = as.Date("2003-01-01"), as = "tibble")

library(xts)

z <- xts(as.numeric(sbpe$`239`), order.by = as.POSIXct(sbpe$date))
plot(z)
fit <- nnetar(z, P = 1, size = 16, lambda = "auto")

forecast(fit)
autoplot(forecast(fit, h = 180), include = 360)

library(dplyr)
library(prophet)
library(forecast)

fit_prophet <- sbpe |> 
  janitor::clean_names() |> 
  filter(date >= as.Date("2018-01-01")) |> 
  mutate(y = BoxCox(x239, lambda = BoxCox.lambda(x239))) |> 
  select(ds = date, y) |> 
  prophet::prophet()

ccv <- cross_validation(
  fit_prophet,
  initial = 365 * 3,
  horizon = 90,
  units = "days"
)

df.p <- performance_metrics(ccv)
head(df.p)

plot_cross_validation_metric(ccv, metric = "mape")



future <- make_future_dataframe(fit_prophet, periods = 365)
fcast <- predict(fit_prophet, future)
plot(fit_prophet, fcast)


autoplot(forecast(aa, h = 180), include = 360)



library(rbcb)
library(tidyverse)

codes <- tribble(
  ~code, ~name_series,
  1402, "energia_br_comercial",
  1403, "energia_br_residencial",
  1404, "energia_br_industrial",
  1406, "energia_br_todos",
  24363, "ibcbr",
  24364, "ibcbr_dessaz",
  7326, "pib_variacao",
  22099, "pib_qtr",
  22109, "pib_qtr_dessaz",
  1393, "petroleo",
  1394, "glp",
  1395, "oleo_combustivel",
  1396, "oleo_diesel",
  1398, "outros_derivados",
  1399, "alcool_hidratado",
  1400, "alcool_anidro",
  1401, "alcool_total"
)

series <- lapply(codes$code, rbcb::get_series)
names(series) <- codes$name_series
series <- lapply(series, \(d) {
  d <- mutate(d, id = names(d)[2])
  names(d)[2] <- "value"
  return(d)
})

tbl <- bind_rows(series, .id = "name_series")

code_energy <- as.character(c(1402:1406))
code_gdp <- c("24363", "22099")
code_gdp_dessaz <- c("24364", "22109")

gdp_energy <- tbl |> 
  filter(
    id %in% c(code_energy, code_gdp),
    date >= as.Date("2002-01-01")
    )

energ <- get_series(1406, as = "ts")
ibc <- get_series(24364, as = "ts")
pib <- get_series(22109, as = "ts")

lenerg <- log(energ)
denerg <- diff(lenerg, 12)
ddenerg <- diff(denerg)

dibc <- diff(ibc)

lenerg <- window(lenerg, start = c(2003, 1), end = c(2014, 1))

level <- ts.intersect(lenerg, ibc, dframe = TRUE)

lm_engle <- lm(ibc ~ lenerg + time(ibc) + I(time(ibc)^2), data = level)
lm_engle <- lm(ibc ~ lenerg, data = level)
summary(lm_engle)

plot.ts(resid(lm_engle))
acf(resid(lm_engle))

aTSA::coint.test(level[, 1], level[, 2], d = 0, nlag = NULL, output = TRUE)


# set up error correction term
VECM_ECT <- resid(lm_engle)

library(dynlm)
# estimate both equations of the VECM using 'dynlm()'
VECM_EQ1 <- dynlm(d(ibc) ~ L(d(ibc), 1:2) + L(d(lenerg), 1:2) + L(VECM_ECT))
VECM_EQ2 <- dynlm(d(lenerg) ~ L(d(ibc), 1:2) + L(d(lenerg), 1:2) + L(VECM_ECT))

library(AER)
coeftest(VECM_EQ1, vcov. = NeweyWest(VECM_EQ1, prewhite = F, adjust = T))
coeftest(VECM_EQ2, vcov. = NeweyWest(VECM_EQ1, prewhite = F, adjust = T))

ecm <- aTSA::ecm(mts[, 1], mts[, 2])

sjd <- ca.jo(level, type = "trace", ecdet = "trend", K = 2)
summary(sjd)
plot(sjd)

ecm <- cajools(sjd)

summary(ecm)

mts <- ts.intersect(ddenerg, dibc, dframe = TRUE)

plot(dibc ~ ddenerg, data = mts)

model_lm <- lm(dibc ~ ddenerg, data = mts)

plot.ts(resid(model_lm))

acf(resid(model_lm))

library(urca)

sjd <- ca.jo(mts, ecdet = "const", K = 2)
plotres(sjd)
summary(sjd)
vars::VARselect(mts)
library(forecast)

stl_energ <- mstl(log(energ), robust = TRUE)

energ <- exp(stl_energ[, "Trend"])
# pib <- stl_pib[, "Trend"]
# ibc <- stl_ibc[, "Trend"]

# energ <- log(energ)
energ <- window(energ, start = c(2002, 1))
base_energ <- mean(window(energ, start = c(2005, 1), end = c(2005, 12)))
energ_index <- energ / base_energ * 100

base_ibc <- mean(window(ibc, start = c(2005, 1), end = c(2005, 12)))
ibc_index <- ibc / base_ibc * 100

base_pib <- mean(window(pib, start = c(2005, 1), end = c(2005, 4)))
pib_index <- pib / base_pib * 100

plot(energ_index)
lines(ibc_index, col = 'red')
lines(pib_index, col = "blue")



plot(ibc)

gdp_energy |> 
  filter(id %in% c(code_gdp, "1406")) |> 
  mutate(index = value / first(value) * 100, .by = "name_series") |> 
  ggplot(aes(date, index, color = name_series)) +
  geom_line()


series_nest <- gdp_energy |> 
  select(date, id, name_series, value) |> 
  group_by(name_series, id) |> 
  nest() |> 
  mutate(
    y = map(data, \(d) stats::ts(log(d$value), start = c(2002, 1), frequency = 12)),
    stl = map(y, forecast::mstl, robust = TRUE),
    decomp = map(stl, as.data.frame)
  )

series_trend <- series_nest |> 
  select(name_series, id, data, decomp) |> 
  unnest(cols = c(data, decomp)) |> 
  ungroup() |> 
  select(-Data) |> 
  rename_with(tolower)

ggplot(series_trend, aes(date, trend, color = name_series)) +
  geom_line() +
  facet_wrap(vars(name_series), scales = "free_y")

dat <- series_trend |> 
  select(name_series, id, date, value = trend)

base_index <- dat |> 
  mutate(ano = lubridate::year(date)) |> 
  filter(ano == 2010) |> 
  summarise(base_index = mean(value), .by = "name_series")

dat <- dat |> 
  left_join(base_index, by = "name_series") |> 
  mutate(index = value / base_index * 100)

ggplot(dat, aes(date, index, color = name_series)) +
  geom_line()

ggplot(filter(dat, id %in% c(code_gdp, "1406")), aes(date, index, color = name_series)) +
  geom_line()
  