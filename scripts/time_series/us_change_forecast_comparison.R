# =============================================================================
# US Consumption Forecast: VAR vs ARIMAX
# =============================================================================

suppressPackageStartupMessages({
  library(fpp3)        # us_change data (loads tidyverts ecosystem)
  library(vars)        # VAR model (we'll call vars:: explicitly)
  library(urca)        # Cointegration
  library(lmtest)      # Granger
  library(forecast)    # ARIMA + forecast
  library(tseries)     # ADF tests
  library(ggplot2)     # Plots
  library(gridExtra)   # Arrange plots
})

# ---------- Helper functions ----------
rmse <- function(actual, forecast) sqrt(mean((actual - forecast)^2, na.rm = TRUE))
mape <- function(actual, forecast) mean(abs((actual - forecast)/actual), na.rm = TRUE) * 100

# =============================================================================
# Data load and basic ts objects
# =============================================================================
data("us_change", package = "fpp3")

consumption_ts <- ts(us_change$Consumption, start = c(1970,1), frequency = 4)
income_ts      <- ts(us_change$Income,      start = c(1970,1), frequency = 4)
production_ts  <- ts(us_change$Production,  start = c(1970,1), frequency = 4)

# Build multivariate ts and also a plain matrix for VAR
multivariate_ts <- cbind(
  Consumption = consumption_ts,
  Income      = income_ts,
  Production  = production_ts
)

cat("Data dimensions:", dim(multivariate_ts), "\n")
cat("Classes: ", class(multivariate_ts), "\n")

# =============================================================================
# Statistical Tests (ADF, Johansen, Granger)
# =============================================================================
cat("=== Statistical Tests ===\n")
cat(sprintf("ADF Consumption: %.4f\n", tseries::adf.test(consumption_ts)$p.value))
cat(sprintf("ADF Income: %.4f\n", tseries::adf.test(income_ts)$p.value))
cat(sprintf("ADF Production: %.4f\n", tseries::adf.test(production_ts)$p.value))

jo_test <- urca::ca.jo(multivariate_ts, type = "trace", ecdet = "const", K = 2)
coint_rank <- sum(jo_test@teststat > jo_test@cval[, "5pct"])
cat("Cointegration rank (Johansen trace):", coint_rank, "\n")

gc_income      <- lmtest::grangertest(consumption_ts ~ income_ts, order = 2)
gc_production  <- lmtest::grangertest(consumption_ts ~ production_ts, order = 2)
gc_cons_income <- lmtest::grangertest(income_ts ~ consumption_ts, order = 2)

cat("Granger Income → Consumption p-value:", gc_income$`Pr(>F)`[2], "\n")
cat("Granger Production → Consumption p-value:", gc_production$`Pr(>F)`[2], "\n")
cat("Granger Consumption → Income p-value:", gc_cons_income$`Pr(>F)`[2], "\n")

# =============================================================================
# Train/Test split (consistent for ts objects)
# =============================================================================
h <- 8
N <- nrow(multivariate_ts)
train_len <- N - h

# Use window() to split ts, then convert training to matrix for vars
train_ts <- window(multivariate_ts, end = time(multivariate_ts)[train_len])
test_ts  <- window(multivariate_ts, start = time(multivariate_ts)[train_len + 1])

# Convert to plain numeric matrices (rows = time, cols = variables)
train_mat <- as.matrix(train_ts)
test_mat  <- as.matrix(test_ts)

cat("Train matrix dimensions:", dim(train_mat), "Test matrix dims:", dim(test_mat), "\n")

# =============================================================================
# Approach 1: VAR
# =============================================================================
cat("\n=== VAR model ===\n")

varsel <- vars::VARselect(train_mat, lag.max = 8, type = "const")
opt_lag <- as.integer(varsel$selection["AIC(n)"])
if (is.na(opt_lag) || opt_lag < 1) opt_lag <- 1
cat("Selected lag by AIC:", opt_lag, "\n")

var_model <- vars::VAR(y = train_mat, p = opt_lag, type = "const")

var_fc_obj <- predict(var_model, n.ahead = h, ci = 0.95)

# Извлечение прогнозов для Consumption
var_consumption_fc <- as.numeric(var_fc_obj$fcst$Consumption[, "fcst"])
var_consumption_lo <- as.numeric(var_fc_obj$fcst$Consumption[, "lower"])
var_consumption_hi <- as.numeric(var_fc_obj$fcst$Consumption[, "upper"])

# =============================================================================
# Approach 2: ARIMAX (auto.arima with xreg)
# =============================================================================
cat("\n=== ARIMAX model ===\n")
train_consumption <- as.numeric(window(consumption_ts, end = time(consumption_ts)[train_len]))
train_xreg <- cbind(
  Income = as.numeric(window(income_ts, end = time(income_ts)[train_len])),
  Production = as.numeric(window(production_ts, end = time(production_ts)[train_len]))
)
test_xreg <- cbind(
  Income = as.numeric(window(income_ts, start = time(income_ts)[train_len + 1])),
  Production = as.numeric(window(production_ts, start = time(production_ts)[train_len + 1]))
)

# Fit ARIMAX
arimax_model <- forecast::auto.arima(train_consumption, xreg = train_xreg,
                                     seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
cat("ARIMAX model:\n"); print(summary(arimax_model))

arimax_fc <- forecast::forecast(arimax_model, xreg = test_xreg, h = h)
arimax_consumption_fc <- as.numeric(arimax_fc$mean)
arimax_consumption_lo <- as.numeric(arimax_fc$lower[,2])
arimax_consumption_hi <- as.numeric(arimax_fc$upper[,2])

# =============================================================================
# Evaluation: compute metrics on test set
# =============================================================================
actual_consumption <- as.numeric(window(consumption_ts, start = time(consumption_ts)[train_len + 1]))

var_rmse <- rmse(actual_consumption, var_consumption_fc)
var_mape <- mape(actual_consumption, var_consumption_fc)
arimax_rmse <- rmse(actual_consumption, arimax_consumption_fc)
arimax_mape <- mape(actual_consumption, arimax_consumption_fc)

cat("\n=== Performance on test set ===\n")
cat(sprintf("VAR    RMSE=%.4f  MAPE=%.2f%%\n", var_rmse, var_mape))
cat(sprintf("ARIMAX RMSE=%.4f  MAPE=%.2f%%\n", arimax_rmse, arimax_mape))

# =============================================================================
# Visualization: time series + forecast ribbons
# =============================================================================
time_all <- time(multivariate_ts)
time_test <- time_all[(train_len+1):N]

plot_df <- data.frame(Time = time_all,
                      Consumption = as.numeric(multivariate_ts[,"Consumption"]))

df_var_fc <- data.frame(Time = time_test, Forecast = var_consumption_fc,
                        Lower = var_consumption_lo, Upper = var_consumption_hi, Model = "VAR")
df_arimax_fc <- data.frame(Time = time_test, Forecast = arimax_consumption_fc,
                           Lower = arimax_consumption_lo, Upper = arimax_consumption_hi, Model = "ARIMAX")

p <- ggplot() +
  geom_line(data = plot_df, aes(x = Time, y = Consumption), color = "black") +
  geom_vline(xintercept = time_all[train_len], linetype = "dashed") +
  geom_line(data = df_var_fc, aes(x = Time, y = Forecast), color = "red") +
  geom_ribbon(data = df_var_fc, aes(x = Time, ymin = Lower, ymax = Upper), fill = "red", alpha = 0.15) +
  geom_line(data = df_arimax_fc, aes(x = Time, y = Forecast), color = "blue") +
  geom_ribbon(data = df_arimax_fc, aes(x = Time, ymin = Lower, ymax = Upper), fill = "blue", alpha = 0.15) +
  labs(title = "Consumption forecasts: VAR (red) vs ARIMAX (blue)",
       x = "Time", y = "Consumption change (%)") +
  theme_minimal()

print(p)

# =============================================================================
# ============================ FINAL CONCLUSIONS ==============================
# =============================================================================
#
# 1. Stationarity: All series (Consumption, Income, Production) are stationary.
#    Johansen test → cointegration rank = 3.
#    Granger causality → one-way (Consumption → Income).
#
# 2. Models:
#    - VAR (p=5 by AIC).
#    - ARIMAX (Consumption with Income & Production as xreg).
#
# 3. Forecast accuracy (8 quarters ahead):
#       VAR:    RMSE = 0.355, MAPE = 53.6%
#       ARIMAX: RMSE = 0.339, MAPE = 55.8%
#
# 4. Conclusion:
#    - VAR is theoretically preferable (cointegration, system view).
#    - ARIMAX is simpler and slightly better in RMSE.
#    - Both show high errors (MAPE > 50%), so results should be used cautiously.
#
# =============================================================================




