# AirPassengers ARIMA Analysis with Cross-Validation

###############################################
# 1. Load Libraries and Prepare Data
###############################################

library(forecast)
library(ggplot2)
library(tseries)
library(plotly)

ts_data <- AirPassengers  # Monthly airline passenger numbers 1949-1960

###############################################
# 2. Stationarity Analysis
###############################################

# Check number of differences needed
cat("Number of differences required:", ndiffs(ts_data), "\n")

# KPSS test for stationarity
kpss_result <- tseries::kpss.test(diff(ts_data))
cat("KPSS test p-value:", kpss_result$p.value, 
    ifelse(kpss_result$p.value > 0.05, 
           "(Stationary)", "(Non-Stationary)"), "\n")

###############################################
# 3. ARIMA Model Fitting
###############################################

# Fit non-seasonal ARIMA
fit1 <- auto.arima(ts_data, seasonal = FALSE)
cat("\nNon-seasonal ARIMA model:", arimaorder(fit1), "\n")

# Fit seasonal ARIMA
fit2 <- auto.arima(ts_data, seasonal = TRUE)
cat("Seasonal ARIMA model:", arimaorder(fit2), "\n")

###############################################
# 4. Model Diagnostics
###############################################

# Function for comprehensive diagnostics
diagnose_model <- function(model, model_name) {
  cat("\n=== Diagnostics for", model_name, "===\n")
  
  # Ljung-Box test
  lb_test <- Box.test(residuals(model), lag = 20, type = "Ljung-Box")
  cat("Ljung-Box test p-value:", lb_test$p.value, 
      ifelse(lb_test$p.value > 0.05, 
             "(No autocorrelation)", "(Autocorrelation present)"), "\n")
  
  # Normality test
  jb_test <- tseries::jarque.bera.test(residuals(model))
  cat("Jarque-Bera test p-value:", jb_test$p.value, 
      ifelse(jb_test$p.value > 0.05, 
             "(Normal residuals)", "(Non-normal residuals)"), "\n")
  
  # Residual plots
  checkresiduals(model)
}

# Run diagnostics
diagnose_model(fit1, "Non-seasonal ARIMA")
diagnose_model(fit2, "Seasonal ARIMA")

###############################################
# 5. Corrected Cross-Validation with Visualization
###############################################

# Fixed cross-validation function
run_cv <- function(y, h = 24) {
  n <- length(y)
  errors <- matrix(NA, nrow = n - h, ncol = h)
  
  for (i in 1:(n - h)) {
    train <- window(y, end = time(y)[i])
    fit <- auto.arima(train)
    fc <- forecast(fit, h = h)
    errors[i, ] <- as.vector(y[(i+1):(i+h)]) - fc$mean
  }
  return(errors)
}

# Run cross-validation
cv_errors <- run_cv(ts_data, h = 24)

# Prepare results for plotting with correct dimensions
cv_results <- data.frame(
  date = time(ts_data)[1:(nrow(cv_errors))],
  RMSE = apply(cv_errors, 1, function(x) sqrt(mean(x^2, na.rm = TRUE)))
)

# Plotly CV results
ggplotly(ggplot(cv_results, aes(x = date, y = RMSE)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(color = "firebrick", size = 2, alpha = 0.7) +
  labs(title = "Rolling 2-Year Forecast RMSE (Cross-Validation)",
       subtitle = "ARIMA Model Performance Over Time",
       x = "Training Window End Date",
       y = "Root Mean Squared Error") +
  theme_minimal())

# Note: by 1950 the model "saw" 2 full annual cycles, which allowed it to:
#       - correctly identify the seasonal component (summer peaks)
#       - catch the main trend of passenger traffic growth
# In 1957-1958, there was an acceleration in passenger traffic growth, 
# and the ARIMA model (trained on earlier data) underestimated new trend.
