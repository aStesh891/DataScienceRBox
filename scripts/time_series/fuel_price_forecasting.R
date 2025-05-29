# =======================================================
# Fuel Price Forecasting: Diesel vs EUR/UAH Exchange Rate
# Analysis using VAR and ARIMAX models
# =======================================================

# === 1. Load Required Libraries ===
library(tidyverse)
library(lubridate)
library(forecast)
library(vars)
library(ggplot2)
library(plotly)
library(tseries)

# === 2. Data Loading and Preparation ===
load_and_prepare_data <- function() {
  # Load datasets with correct paths and column names
  diesel <- read_delim("data/processed/Diesel_ready.csv", delim = ";", 
                       col_names = c("date", "diesel_price"), skip = 1)
  
  euruah <- read_csv("data/processed/EUR_UAH_ready.csv", 
                     col_names = c("date", "eur_uah_rate"), skip = 1)
  
  # Convert date formats and ensure proper data types
  diesel <- diesel %>%
    mutate(date = ymd(date),
           diesel_price = as.numeric(diesel_price))
  
  euruah <- euruah %>%
    mutate(date = ymd(date),
           eur_uah_rate = as.numeric(eur_uah_rate))
  
  # Merge datasets and handle missing values
  merged_data <- full_join(diesel, euruah, by = "date") %>%
    arrange(date) %>%
    # Interpolate missing values for consistent time series
    mutate(across(c(diesel_price, eur_uah_rate), 
                  ~na.approx(., na.rm = FALSE))) %>%
    # Remove any remaining NA values at beginning/end
    na.omit()
  
  return(merged_data)
}

fuel_data <- load_and_prepare_data()

# === 3. Exploratory Data Analysis ===
plot_time_series <- function(data) {
  data_long <- data %>% 
    pivot_longer(-date, names_to = "variable", values_to = "value")
  
  ggplot(data_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = "Diesel Price and EUR/UAH Exchange Rate", 
         x = "Date", y = "Value") +
    theme_minimal() +
    scale_color_manual(values = c("diesel_price" = "darkblue", 
                                  "eur_uah_rate" = "darkred"),
                       labels = c("Diesel Price (UAH)", "EUR/UAH Rate"))
}

ggplotly(plot_time_series(fuel_data))

# === 4. Data Partitioning ===
split_time_series <- function(data, train_ratio = 0.8) {
  train_size <- floor(train_ratio * nrow(data))
  list(
    train = data[1:train_size, ],
    test = data[(train_size + 1):nrow(data), ]
  )
}

data_split <- split_time_series(fuel_data)

# === 5. Model Building ===

# Create time series objects
create_ts <- function(data, freq = 365) {  # daily frequency
  ts(data, frequency = freq)
}

# VAR Model Implementation
build_var_model <- function(train_data) {
  train_ts <- create_ts(train_data[, c("diesel_price", "eur_uah_rate")])
  lag_selection <- VARselect(train_ts, lag.max = 12, type = "const")
  best_lag <- lag_selection$selection["AIC(n)"]
  VAR(train_ts, p = best_lag, type = "const")
}

# ARIMAX Model Implementation
build_arimax_model <- function(train_data) {
  diesel_ts <- create_ts(train_data$diesel_price)
  euruah_ts <- create_ts(train_data$eur_uah_rate)
  auto.arima(diesel_ts, xreg = euruah_ts)
}

var_model <- build_var_model(data_split$train)
arimax_model <- build_arimax_model(data_split$train)

# === 6. Model Evaluation ===

# Forecasting functions
forecast_var <- function(model, test_data) {
  n_ahead <- nrow(test_data)
  forecast_result <- predict(model, n.ahead = n_ahead)
  list(
    forecast = forecast_result$fcst$diesel_price[, 1],
    se = var(forecast_result$fcst$diesel_price[, 2])
  )
}

forecast_arimax <- function(model, test_data) {
  eur_test_ts <- create_ts(test_data$eur_uah_rate)
  forecast(model, xreg = eur_test_ts)
}

# Calculate performance metrics
calculate_metrics <- function(actual, predicted) {
  list(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2))
  )
}

# Generate forecasts
var_forecast <- forecast_var(var_model, data_split$test)
arimax_forecast <- forecast_arimax(arimax_model, data_split$test)

# Evaluate models
metrics_var <- calculate_metrics(data_split$test$diesel_price, var_forecast$forecast)
metrics_arimax <- calculate_metrics(data_split$test$diesel_price, arimax_forecast$mean)

# Display results
cat("=== Model Performance Metrics ===\n")
cat("ARIMAX Model:\n")
cat("  MAE: ", round(metrics_arimax$MAE, 3), "\n")
cat("  RMSE:", round(metrics_arimax$RMSE, 3), "\n")
cat("VAR Model:\n")
cat("  MAE: ", round(metrics_var$MAE, 3), "\n")
cat("  RMSE:", round(metrics_var$RMSE, 3), "\n")

# === 7. Visualization ===

# Create forecast comparison dataframe
create_forecast_df <- function(test_data, actual, forecast, lower, upper, model_name) {
  data.frame(
    date = test_data$date,
    actual = actual,
    forecast = forecast,
    lower = lower,
    upper = upper,
    model = model_name
  )
}

# Prepare data for plotting
forecast_data <- bind_rows(
  create_forecast_df(
    data_split$test,
    data_split$test$diesel_price,
    as.numeric(arimax_forecast$mean),
    arimax_forecast$lower[, 2],
    arimax_forecast$upper[, 2],
    "ARIMAX"
  ),
  create_forecast_df(
    data_split$test,
    data_split$test$diesel_price,
    var_forecast$forecast,
    var_forecast$forecast - 1.96 * var_forecast$se,
    var_forecast$forecast + 1.96 * var_forecast$se,
    "VAR"
  )
)

# Interactive forecast plot
plot_forecasts <- function(data) {
  p <- ggplot(data, aes(x = date)) +
    geom_line(aes(y = actual), color = "black", linetype = "dotted") +
    geom_line(aes(y = forecast, color = model)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.2) +
    facet_wrap(~model, ncol = 1) +
    labs(title = "Diesel Price Forecasting", 
         subtitle = "ARIMAX vs VAR Model Comparison",
         y = "Diesel Price (UAH)", x = "Date") +
    theme_minimal() +
    scale_color_manual(values = c("ARIMAX" = "red", "VAR" = "blue")) +
    scale_fill_manual(values = c("ARIMAX" = "red", "VAR" = "blue"))
  
  ggplotly(p)
}

plot_forecasts(forecast_data)

# === 8. Model Diagnostics ===

# VAR Model Diagnostics
diagnose_var_model <- function(model) {
  cat("\n=== VAR Model Diagnostics ===\n")
  
  # Autocorrelation test
  serial_test <- serial.test(model, lags.pt = 16, type = "PT.asymptotic")
  cat("\nLjung-Box Autocorrelation Test:\n")
  print(serial_test)
  
  # Heteroskedasticity test
  arch_test <- arch.test(model, lags.multi = 5, multivariate.only = TRUE)
  cat("\nARCH Heteroskedasticity Test:\n")
  print(arch_test)
  
  # Normality test
  jb_test <- normality.test(model, multivariate.only = TRUE)
  cat("\nJarque-Bera Normality Test:\n")
  print(jb_test)
  
  # Stability check
  cat("\nModel Stability Check:\n")
  stability_check <- roots(model, modulus = TRUE)
  print(stability_check)
  if (all(stability_check < 1)) {
    cat("Model is stable (all roots inside unit circle).\n")
  } else {
    cat("Warning: Model is unstable - some roots outside unit circle.\n")
  }
}

# ARIMAX Model Diagnostics
diagnose_arimax_model <- function(model) {
  cat("\n=== ARIMAX Model Diagnostics ===\n")
  res <- residuals(model)
  
  # Autocorrelation test
  lb_test <- Box.test(res, lag = 16, type = "Ljung-Box", fitdf = length(model$coef))
  cat("\nLjung-Box Autocorrelation Test:\n")
  print(lb_test)
  
  # ARCH effects test
  arch_test <- ArchTest(res, lags = 12)
  cat("\nARCH Effects Test:\n")
  print(arch_test)
  
  # Normality test
  jb_test <- jarque.bera.test(res)
  cat("\nJarque-Bera Normality Test:\n")
  print(jb_test)
}

# Run diagnostics
diagnose_var_model(var_model)
diagnose_arimax_model(arimax_model)

# === 9. Residual Analysis ===
plot_model_residuals <- function(test_data, var_forecast, arimax_forecast) {
  residuals_df <- data.frame(
    date = test_data$date,
    ARIMAX = test_data$diesel_price - arimax_forecast$mean,
    VAR = test_data$diesel_price - var_forecast$forecast
  ) %>% 
    pivot_longer(-date, names_to = "Model", values_to = "Residual")
  
  p <- ggplot(residuals_df, aes(x = date, y = Residual, color = Model)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Model Residuals Comparison",
         subtitle = "Test Set Performance",
         x = "Date", y = "Residual") +
    theme_minimal()
  
  ggplotly(p)
}

plot_model_residuals(data_split$test, var_forecast, arimax_forecast)

# === 10. Model Selection Recommendation ===
recommend_best_model <- function(metrics_arimax, metrics_var) {
  cat("\n=== Final Recommendation ===\n")
  
  if (metrics_arimax$RMSE < metrics_var$RMSE && metrics_arimax$MAE < metrics_var$MAE) {
    cat("ARIMAX model shows superior forecasting performance.\n")
    cat("Recommended for diesel price forecasting.\n")
  } else if (metrics_var$RMSE < metrics_arimax$RMSE && metrics_var$MAE < metrics_arimax$MAE) {
    cat("VAR model demonstrates better accuracy.\n")
    cat("Recommended for diesel price forecasting.\n")
  } else {
    cat("Results are inconclusive:\n")
    cat("- ARIMAX: MAE =", round(metrics_arimax$MAE, 3), 
        ", RMSE =", round(metrics_arimax$RMSE, 3), "\n")
    cat("- VAR:    MAE =", round(metrics_var$MAE, 3), 
        ", RMSE =", round(metrics_var$RMSE, 3), "\n")
    cat("ARIMAX may be preferable when EUR/UAH is a strong external factor.\n")
  }
}

recommend_best_model(metrics_arimax, metrics_var)
