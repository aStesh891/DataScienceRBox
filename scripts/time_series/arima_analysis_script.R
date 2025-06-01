# ARIMA Analysis Script
# Example workflow for time series analysis with two datasets

# Load required packages
library(forecast)
library(ggplot2)
library(readr)
library(zoo)

# =============================================================================
# EXAMPLE 1: AirPassengers (classic seasonal data)
# =============================================================================

message("\nAnalyzing AirPassengers dataset...\n")

# 1. Load and prepare data
data("AirPassengers")
ts_data <- AirPassengers

# 2. Visualize original series
autoplot(ts_data) + 
  ggtitle("AirPassengers: Original Series") +
  ylab("Passengers (thousands)") + 
  xlab("Year")

# 3. Determine differencing orders ------------------------------------------
d <- ndiffs(ts_data)  # Regular differencing
D <- nsdiffs(ts_data) # Seasonal differencing

message("Estimated d (regular differencing): ", d)
message("Estimated D (seasonal differencing): ", D)

# 4. Differencing and ACF/PACF analysis -------------------------------------
ts_diff <- diff(ts_data, differences = d)

if (D > 0) {
  ts_seasonal_diff <- diff(ts_diff, lag = 12, differences = D)
  ggtsdisplay(ts_seasonal_diff, 
              main = "ACF/PACF After Differencing")
} else {
  ggtsdisplay(ts_diff, 
              main = "ACF/PACF After Differencing")
}

# 5. Fit manual ARIMA model -------------------------------------------------
fit_manual <- Arima(ts_data, 
                    order = c(1, d, 1),
                    seasonal = list(order = c(1, D, 1), period = 12))

message("\nManual ARIMA Model Summary:")
print(summary(fit_manual))

message("\nResidual Diagnostics:")
checkresiduals(fit_manual)

# 6. Fit automatic ARIMA model ----------------------------------------------
fit_auto <- auto.arima(ts_data, 
                       seasonal = TRUE, 
                       stepwise = FALSE, 
                       approximation = FALSE)

message("\nAuto ARIMA Model Summary:")
print(summary(fit_auto))

message("\nResidual Diagnostics:")
checkresiduals(fit_auto)

# 7. Generate and visualize forecast ----------------------------------------
forecast_vals <- forecast(fit_auto, h = 24)

forecast_plot <- autoplot(forecast_vals) + 
  ggtitle("2-Year Passenger Forecast") +
  ylab("Passengers (thousands)") + 
  xlab("Year")

print(forecast_plot)

# =============================================================================
# EXAMPLE 2: Diesel Prices (financial data)
# =============================================================================

message("\nAnalyzing Diesel Prices dataset...\n")

# 1. Load and prepare data
df <- read.csv("data/processed/Diesel_ready.csv", sep = ";", header = TRUE)
colnames(df) <- c("date", "price")
df$date <- as.Date(df$date)

# Convert to time series (252 trading days/year)
ts_diesel <- ts(df$price, 
                start = c(lubridate::year(df$date[1]), 
                          lubridate::yday(df$date[1])),
                frequency = 252)

# 2. Visualize original series with custom formatting
autoplot(ts_diesel) +
  ggtitle("Diesel Prices: Original Series") +
  ylab("Price") + 
  xlab("Date") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# 3. Determine differencing orders
d <- ndiffs(ts_diesel)
D <- 0  # No seasonal differencing for financial data

message("Estimated d (regular differencing): ", d)
message("Seasonal differencing disabled for financial data")

# 4. Differencing and ACF/PACF analysis
ts_diff <- diff(ts_diesel, differences = d)
ggtsdisplay(ts_diff, main = "ACF/PACF After Differencing")

# 5. Fit manual ARIMA model (specific for financial data)
fit_manual <- Arima(ts_diesel, order = c(4, d, 2))  # Higher AR/MA terms

message("\nManual ARIMA Model Summary:")
print(summary(fit_manual))

message("\nResidual Diagnostics:")
checkresiduals(fit_manual)

# 6. Fit automatic ARIMA model
fit_auto <- auto.arima(ts_diesel, 
                       seasonal = FALSE,
                       stepwise = FALSE, 
                       approximation = FALSE)

message("\nAuto ARIMA Model Summary:")
print(summary(fit_auto))

message("\nResidual Diagnostics:")
checkresiduals(fit_auto)

# 7. Generate and visualize forecast
forecast_vals <- forecast(fit_auto, h = 30)  # 30 trading days (~1.5 months)

autoplot(forecast_vals) +
  ggtitle("Diesel Price Forecast (30 trading days)") +
  ylab("Price") + 
  xlab("Date") +
  theme(plot.title = element_text(hjust = 0.5))

# 8. Save results (optional)
# saveRDS(list(model = fit_auto, forecast = forecast_vals), 
#        "diesel_arima_results.rds")

