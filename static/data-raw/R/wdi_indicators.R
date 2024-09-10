library(WDI)

codes <- c(
  "life_expectancy" = "SP.DYN.LE00.IN",
  "infant_mortality" = "SP.DYN.IMRT.IN",
  "gdppc_ppp" = "NY.GDP.PCAP.PP.CD",
  "share_electricity" = "EG.ELC.ACCS.ZS",
  "fertility" = "SP.DYN.TFRT.IN",
  "urban_rate" = "SP.URB.TOTL.IN.ZS",
  "population" = "SP.POP.TOTL",
  "labor_female" = "SL.TLF.TOTL.FE.ZS"
)

dat <- WDI(indicator = codes)

readr::write_rds(dat, here::here("static/data/wdi_indicators.rds"))