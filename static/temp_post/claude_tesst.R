# Simple histogram example
data <- rnorm(100)
hist(data, main = "Random Normal Distribution", xlab = "Values")

# Plotting a line
plot(1:10, 1:10, type = "l", main = "Line Plot", xlab = "X", ylab = "Y")

library(ggplot2)

economics


# Load required packages
library(ggplot2)
library(dplyr)
library(forecast)
library(tseries)

# Explore unemployment data
economics |>
  ggplot(aes(x = date, y = unemploy)) +
  geom_line() +
  labs(
    title = "US Unemployment Over Time",
    x = "Date",
    y = "Unemployment (thousands)"
  )

# Create time series object
unemploy_ts <- ts(economics$unemploy, start = c(1967, 7), frequency = 12)

# Plot time series components
autoplot(unemploy_ts) +
  labs(title = "Unemployment Time Series", y = "Unemployment (thousands)")

# Check for stationarity
adf.test(unemploy_ts)

# Seasonal decomposition
decomp <- stl(unemploy_ts, s.window = "periodic")
autoplot(decomp) +
  labs(title = "Seasonal Decomposition of Unemployment")

# Fit SARIMA model
sarima_model <- auto.arima(
  unemploy_ts,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

# Model summary
summary(sarima_model)

# Forecast 24 months ahead
forecast_result <- forecast(sarima_model, h = 24)

# Plot forecast
autoplot(forecast_result) +
  labs(
    title = "SARIMA Forecast of US Unemployment",
    x = "Date",
    y = "Unemployment (thousands)"
  ) +
  theme_minimal()

# Check residuals
checkresiduals(sarima_model)

# Fit benchmark models
sarima_011_011 <- Arima(unemploy_ts, order = c(0, 1, 1), seasonal = c(0, 1, 1))
sarima_110_110 <- Arima(unemploy_ts, order = c(1, 1, 0), seasonal = c(1, 1, 0))

# Generate forecasts for all models
forecast_auto <- forecast(sarima_model, h = 24)
forecast_011_011 <- forecast(sarima_011_011, h = 24)
forecast_110_110 <- forecast(sarima_110_110, h = 24)

# Model comparison metrics
model_comparison <- data.frame(
  Model = c("Auto ARIMA", "SARIMA(0,1,1)(0,1,1)", "SARIMA(1,1,0)(1,1,0)"),
  AIC = c(AIC(sarima_model), AIC(sarima_011_011), AIC(sarima_110_110)),
  BIC = c(BIC(sarima_model), BIC(sarima_011_011), BIC(sarima_110_110)),
  RMSE = c(
    accuracy(sarima_model)["RMSE"],
    accuracy(sarima_011_011)["RMSE"],
    accuracy(sarima_110_110)["RMSE"]
  )
)

print(model_comparison)

# Plot all forecasts together
library(gridExtra)

p1 <- autoplot(forecast_auto) +
  labs(title = "Auto ARIMA Forecast") +
  theme_minimal()

p2 <- autoplot(forecast_011_011) +
  labs(title = "SARIMA(0,1,1)(0,1,1) Forecast") +
  theme_minimal()

p3 <- autoplot(forecast_110_110) +
  labs(title = "SARIMA(1,1,0)(1,1,0) Forecast") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 1)

# Check residuals for benchmark models
checkresiduals(sarima_011_011)
checkresiduals(sarima_110_110)

# Observations

# Claude Assistant has three modes: ask, edit, and agent
# Ask is like integrated chatbot, can only make suggestions
# Edit can acutally change files and implement changes
# Agent is more hands-free the AI takes over

# Important: can call specialists @shiny to help with shiny apps

# Important: Claude API is very expensive, use wisely
# always remeber to ask for Claude to be concise and terse in chat

# For bigger projects can add a custom context, but by deafult
# Claude already has access to files, console, enviornment variables, etc.
# so it already has a good context.

# Important: inline chat
# Can highlight a section of code and call cluade using cmd + i
# Claude will respond to the highlighted code

# Next steps: test RClaude in RStudio
# test together with Copilot
