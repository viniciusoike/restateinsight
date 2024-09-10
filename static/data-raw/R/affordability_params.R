library(realestatebr)
library(dplyr)
library(tidyr)

loan <- function(p, ltv = 0.7, i, ic = 12, n = 360, method = "sac", comp = 0.3) {
  
  if (n < 1) {message("Prazo tem que ser maior que 1.")}
  
  # Savings
  S = (1 - ltv) * p
  # Debt (to be financed)
  D = p - S
  if (D < 0) {message("Valor financiado menor que 0.")}
  if (i > 1) {i = i / 100}
  taxa = (1 + i) ^ (1 / ic) - 1
  
  if (method == "sac") {
    
    # Vetor do valor da parcela (PMT)
    pmt = vector(mode = "numeric", length = n)
    # Vetor do valor da divida a saldar (D)
    divida = vector(mode = "numeric", length = n + 1)
    # Vetor do valor dos juros cobrados (i)
    juros = vector(mode = "numeric", length = n)
    # Valor inicial da divida
    divida[1] = D
    # No SAC o valor de amort eh constante
    amort = D/n
    
    for (i in 1:n) {
      
      divida[i + 1] = divida[i] - amort
      juros[i] = taxa * divida[i]
      pmt[i] = juros[i] + amort
      
    }
    
    renda_minima = pmt[1] / comp
    out = list(juros = juros, amort = amort, divida = divida,
               valor_inicial = p, renda = renda_minima,
               parcela = pmt)
    return(out)
  }
  
  if (method == "price") {
    
    # Vetor do valor da parcela (PMT)
    pmt = p * (taxa * (1 + taxa)^n) / ((1 + taxa)^n - 1)
    # Vetor do valor da divida a saldar (D)
    divida = vector(mode = "numeric", length = n)
    # Vetor do valor dos juros cobrados (i)
    juros = vector(mode = "numeric", length = n)
    # Vetor do valor da amortizacao (A)
    amort = vector(mode = "numeric", length = n)
    
    # Valor inicial da divida
    divida[1] = D
    
    for (i in 1:n) {
      # Computa os juros
      juros[i] = taxa * divida[i]
      amort[i] = pmt - juros[i]
      divida[i + 1] = divida[i] - amort[i]
    }
    renda_minima = pmt[1] / comp
    out = list(juros = juros, amort = amort, divida = divida,
               valor_inicial = p, renda = renda_minima)
    return(out)
  }
  
}

parcela_minima <- function(p, ltv = 0.7, i = 10, n = 360) {
  
  if (n < 1) {message("Prazo tem que ser maior que 1.")}
  
  # Savings
  S = (1 - ltv) * p
  # Debt (to be financed)
  D = p - S
  if (D < 0) {message("Valor financiado menor que 0.")}
  if (i > 1) {i = i / 100}
  taxa = (1 + i) ^ (1 / 12) - 1
  
  juros = D * taxa
  amort = D / n
  pmt = juros + amort
  
  return(pmt)
  
}

# Rendimento médio de todos os trabalhos, habitualmente recebido por mês,
# pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência,
# com rendimento de trabalho

# ipeadata
renda <- readxl::read_excel(here::here("static/data/raw/ipeadata_renda_media.xls"))

names(renda) <- c("date", "renda_media_habitual")

renda <- renda |> 
  mutate(date = readr::parse_date(date, "%Y.%m"))

# BCB using realestatebr
bcb <- get_bcb_realestate()
bcb_series <- get_bcb_series('credit')

d1 <- bcb |> 
  filter(series_info == "credito_contratacao_taxa_pf_sfh_br") |> 
  mutate(name_series = "taxa_pf_sfh") |> 
  select(date, name_series, value)

d2 <- bcb |> 
  filter(series_info == "credito_contratacao_ltv_pf_sfh_br") |> 
  mutate(name_series = "ltv_pf_sfh") |> 
  select(date, name_series, value)

d3 <- bcb |> 
  filter(series_info == "credito_contratacao_contratado_mediana_pf_sfh") |> 
  mutate(name_series = "pmed_pf_sfh") |> 
  select(date, name_series, value)

d4 <- bcb_series |> 
  filter(name_simplified == "prazo_fimob_pf_total") |> 
  mutate(name_series = "prazo_pf_total") |> 
  select(date, name_series, value)

dat <- bind_rows(list(d1, d2, d3, d4))

dat <- dat |> 
  mutate(date = lubridate::floor_date(date, unit = "month")) |> 
  pivot_wider(names_from = "name_series")

dat <- dat |> 
  left_join(renda, by = "date")

library(purrr)

pmap(dat, \(p, l, i, n) loan(p = pmed_pf_sfh, ltv = ltv_pf_sfh / 100, i = taxa_pf_sfh, n = prazo_pf_total))

params <- dat |> 
  na.omit() |> 
  rename(p = pmed_pf_sfh, ltv = ltv_pf_sfh, i = taxa_pf_sfh, n = prazo_pf_total) |> 
  mutate(
    ltv = ltv / 100,
    pmt = pmap_dbl(list(p, ltv, i, n), parcela_minima)
    )

params <- params |> 
  mutate(
    hai = renda_media_habitual * 0.3 / pmt * 100,
    pir = p / (renda_media_habitual * 12.5)
  )

qs::qsave(params, here::here("static/data/affordability_params.qs"))

# library(ggplot2)
# 
# ggplot(params, aes(date, hai)) +
#   geom_line() +
#   geom_point()