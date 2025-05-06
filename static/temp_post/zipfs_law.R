library(tidyverse)

dat <- sidrar::get_sidra(4709, variable = 93, geo = "City")

pop_cities <- dat |> 
  janitor::clean_names() |> 
  select(
    code_muni = municipio_codigo,
    name_muni = municipio,
    pop = valor
  )

pop_cities <- pop_cities |> 
  filter(pop > 50000)

pop_cities <- pop_cities |> 
#  slice_max(pop, n = 100) |> 
  mutate(
    rank = rank(-pop),
    lrank = log(rank - 0.5),
    lpop = log(pop),
    comp = pop / max(pop)
    )

head(pop_cities)

a <- max(pop_cities$lpop)

model_lm <- lm(lrank ~ lpop - 1, data = pop_cities, offset = rep(a, nrow(pop_cities)))

broom::tidy(summary(model_lm))
confint(model_lm, level = 0.95)

bind_cols(observed = pop_cities$lrank, estimated = fitted(model_lm))

plot(model_lm)

library(ragg)

ggplot(pop_cities, aes(x = lpop, y = log(rank))) +
  geom_point() +
  geom_abline(slope = -1, intercept = a, linetype = 2) +
  geom_abline(slope = coef(model_lm), intercept = a) +
  theme_minimal(base_family = "Avenir")

head(dat)
