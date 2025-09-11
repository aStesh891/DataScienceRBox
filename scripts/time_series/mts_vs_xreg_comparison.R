# =============================================================================
# Description: Comparative analysis of Multivariate Time Series (MTS) models 
#              vs ARIMAX with external regressors. Includes Granger causality,
#              Johansen cointegration tests, and forecast performance evaluation
#              for EuStockMarkets and AirPassengers datasets.
# =============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(vars)       # Vector Autoregression models
  library(urca)       # Unit root and cointegration tests
  library(lmtest)     # Granger causality tests
  library(forecast)   # Time series forecasting
  library(ggplot2)    # Data visualization
  library(reshape2)   # Data reshaping
  library(gridExtra)  # Grid arrangement for plots
  library(plotly)     # Interactive plots
  library(Metrics)    # Evaluation metrics
})

# ---------- Helper functions ----------
rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}

mape <- function(actual, forecast) {
  mean(abs((actual - forecast)/actual), na.rm = TRUE) * 100
}

plot_granger <- function(df, title) {
  ggplot(df, aes(x = Direction, y = p_value, fill = Causal)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "darkred") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey")) +
    labs(title = title, y = "p-value", x = "Causality Direction") +
    theme_minimal()
}

plot_johansen <- function(jo_test, title) {
  johansen_df <- data.frame(
    Statistic = jo_test@teststat,
    Critical_5pct = jo_test@cval[, 2],
    Hypothesis = rownames(jo_test@cval)
  )
  ggplot(johansen_df, aes(x = Hypothesis)) +
    geom_bar(aes(y = Statistic), stat = "identity", fill = "darkgreen", alpha = 0.7) +
    geom_point(aes(y = Critical_5pct), color = "red", size = 3) +
    labs(title = title, y = "Statistic / Critical (5%)", x = "Hypothesis") +
    theme_minimal()
}

vline_at <- function(x) geom_vline(xintercept = x, linetype = "dashed")

# =============================================================================
# Example 1: MTS Analysis - EuStockMarkets (DAX, SMI, CAC, FTSE)
# =============================================================================
cat("=== Analyzing EuStockMarkets Dataset ===\n")

data("EuStockMarkets")
stocks <- as.data.frame(EuStockMarkets)
stocks$Index <- seq_len(nrow(stocks))  # Simple time index for plotting
stocks_m <- melt(stocks, id.vars = "Index", variable.name = "Series", value.name = "Value")

# --- Time series visualization
p_stocks_ts <- ggplot(stocks_m, aes(Index, Value, color = Series)) +
  geom_line() +
  labs(title = "EuStockMarkets - Stock Indices Dynamics", x = "Day", y = "Value") +
  theme_minimal()
print(p_stocks_ts)

# --- Lag order selection for VAR/VECM
sel <- VARselect(EuStockMarkets, lag.max = 10, type = "const")
p_opt <- sel$selection[["AIC(n)"]]  # Use AIC criterion
if (is.null(p_opt) || is.na(p_opt)) p_opt <- 2
cat(sprintf("Optimal lag order (AIC): %d\n", p_opt))

# --- Johansen cointegration test (for EuStockMarkets)
jo_stocks <- ca.jo(EuStockMarkets, type = "trace", ecdet = "const", K = p_opt)
rank_pos <- sum(jo_stocks@teststat > jo_stocks@cval[, 2])  # Number of cointegrating vectors at 5%
cat(sprintf("Number of cointegrating vectors: %d\n", rank_pos))

p_johansen <- plot_johansen(jo_stocks, 
                            sprintf("Johansen Test - EuStockMarkets (K=%d, rank=%d)", p_opt, rank_pos))
print(p_johansen)

# --- Granger causality tests: DAX <-> SMI
g_DAX_from_SMI <- grangertest(stocks$DAX ~ stocks$SMI, order = min(4, p_opt))
g_SMI_from_DAX <- grangertest(stocks$SMI ~ stocks$DAX, order = min(4, p_opt))

granger_stocks_df <- data.frame(
  Direction = c("SMI → DAX", "DAX → SMI"),
  p_value   = c(g_DAX_from_SMI$`Pr(>F)`[2], g_SMI_from_DAX$`Pr(>F)`[2]),
  Causal    = c(g_DAX_from_SMI$`Pr(>F)`[2] < 0.05, g_SMI_from_DAX$`Pr(>F)`[2] < 0.05)
)

p_granger_stocks <- plot_granger(granger_stocks_df, 
                                 sprintf("Granger Causality - EuStockMarkets (order=%d)", min(4, p_opt)))
print(p_granger_stocks)

# Arrange all plots together
grid.arrange(p_stocks_ts, p_granger_stocks, p_johansen, ncol = 1)

# --- Backtesting: Compare forecast quality for DAX (MTS vs ARIMAX)
h <- 60  # Test set length
n <- nrow(stocks)
train_idx <- 1:(n - h)
test_idx  <- (n - h + 1):n

train_mts <- EuStockMarkets[train_idx, ]
test_mts  <- EuStockMarkets[test_idx, ]

# Model A (MTS): VECM->VAR if cointegration exists, otherwise VAR
if (rank_pos >= 1) {
  vecm <- ca.jo(train_mts, type = "trace", ecdet = "const", K = p_opt)
  mts_model <- vec2var(vecm, r = 1)
  model_type <- "VECM->VAR"
} else {
  mts_model <- VAR(train_mts, p = p_opt, type = "const")
  model_type <- "VAR"
}
cat(sprintf("MTS model type: %s\n", model_type))

f_mts <- predict(mts_model, n.ahead = h)
dax_hat_mts <- as.numeric(f_mts$fcst$DAX[, "fcst"])

# Model B (Xreg): ARIMAX for DAX with SMI as external regressor
# Note: In real forecasting, xreg values must be known in advance.
# For backtesting, we use actual SMI values (for comparison purposes).
xreg_train <- matrix(train_mts[, "SMI"], ncol = 1)
xreg_test  <- matrix(test_mts[, "SMI"], ncol = 1)  # Actual SMI values

fit_arimax <- auto.arima(train_mts[, "DAX"], xreg = xreg_train, seasonal = FALSE)
f_arimax   <- forecast(fit_arimax, xreg = xreg_test, h = h)
dax_hat_xreg <- as.numeric(f_arimax$mean)

# Calculate performance metrics for DAX
dax_true <- as.numeric(test_mts[, "DAX"])
eu_rmse_mts <- rmse(dax_true, dax_hat_mts)
eu_mape_mts <- mape(dax_true, dax_hat_mts)
eu_rmse_xrg <- rmse(dax_true, dax_hat_xreg)
eu_mape_xrg <- mape(dax_true, dax_hat_xreg)

cat("\n=== DAX Forecast Performance (EuStockMarkets) ===\n")
cat(sprintf("MTS (%s):   RMSE = %.3f, MAPE = %.2f%%\n", model_type, eu_rmse_mts, eu_mape_mts))
cat(sprintf("ARIMAX (SMI xreg): RMSE = %.3f, MAPE = %.2f%% [using actual SMI values]\n", 
            eu_rmse_xrg, eu_mape_xrg))

# --- DAX forecast plot: MTS vs Xreg
df_plot_dax <- data.frame(
  t = c(train_idx, test_idx, test_idx),
  value = c(as.numeric(EuStockMarkets[train_idx, "DAX"]),
            dax_hat_mts,
            dax_hat_xreg),
  group = factor(c(rep("Actual (train)", length(train_idx)),
                   rep("MTS Forecast", h),
                   rep("Xreg Forecast", h)),
                 levels = c("Actual (train)", "MTS Forecast", "Xreg Forecast"))
)

p_dax_fc <- ggplot() +
  geom_line(data = data.frame(t = 1:n, y = as.numeric(EuStockMarkets[, "DAX"])),
            aes(t, y), color = "black") +
  geom_line(data = df_plot_dax[df_plot_dax$group != "Actual (train)", ],
            aes(t, value, color = group), linewidth = 1) +
  vline_at(n - h) +
  labs(title = "DAX: Forecast Comparison (MTS vs Xreg)", 
       x = "Day", y = "DAX Value", color = "Series") +
  theme_minimal() +
  scale_color_manual(values = c("MTS Forecast" = "#1f77b4", "Xreg Forecast" = "#ff7f7f"))
print(p_dax_fc)
ggplotly(p_dax_fc)

# =============================================================================
# Example 2: Xreg Analysis - AirPassengers (No Johansen test)
# Rationale: Regressors (Fourier harmonics) are time-determined, 
# making Johansen matrices singular.
# =============================================================================
cat("\n=== Analyzing AirPassengers Dataset ===\n")

data("AirPassengers")
ap <- AirPassengers  # Monthly time series, frequency=12

# --- Parameters
K <- 2     # Number of Fourier harmonics
h_ap <- 12 # Forecast horizon (12 months)

# --- Train/test split (ensuring exactly 12 test points)
ap_train <- ts(head(ap, length(ap) - h_ap), frequency = 12, start = start(ap))
ap_test  <- ts(tail(ap, h_ap), frequency = 12,
               start = c(end(ap_train)[1] + (1/12)*(end(ap_train)[2] %% 12 == 0),
                         ifelse(end(ap_train)[2] == 12, 1, end(ap_train)[2] + 1)))

# --- Fourier regressors
xreg_train <- fourier(ap_train, K = K)
xreg_test  <- fourier(ap, K = K, h = h_ap)  # For forecasting

# --- Model 1: ARIMA + Fourier Xreg
fit_ap_xreg <- auto.arima(ap_train, xreg = xreg_train, seasonal = TRUE)
fc_ap_xreg  <- forecast(fit_ap_xreg, xreg = xreg_test, h = h_ap)

# --- Model 2: SARIMA without Xreg (baseline)
fit_ap_sarima <- auto.arima(ap_train, seasonal = TRUE)
fc_ap_sarima  <- forecast(fit_ap_sarima, h = h_ap)

# --- Performance metrics
ap_true <- as.numeric(ap_test)
ap_hat_xreg <- as.numeric(fc_ap_xreg$mean)
ap_hat_sar  <- as.numeric(fc_ap_sarima$mean)

ap_rmse_xreg <- rmse(ap_true, ap_hat_xreg)
ap_mape_xreg <- mape(ap_true, ap_hat_xreg)

ap_rmse_sar  <- rmse(ap_true, ap_hat_sar)
ap_mape_sar  <- mape(ap_true, ap_hat_sar)

cat("\n=== AirPassengers Forecast Performance (12 months) ===\n")
cat(sprintf("ARIMA + Fourier Xreg: RMSE = %.1f, MAPE = %.2f%%\n", ap_rmse_xreg, ap_mape_xreg))
cat(sprintf("SARIMA (no Xreg):     RMSE = %.1f, MAPE = %.2f%%\n", ap_rmse_sar, ap_mape_sar))

# --- AirPassengers forecast plot
df_ap <- data.frame(
  t = time(ap),
  y = as.numeric(ap),
  set = ifelse(time(ap) <= max(time(ap_train)), "train", "test")
)

df_ap_fc <- data.frame(
  t = time(ap_test),
  Xreg = ap_hat_xreg,
  SARIMA = ap_hat_sar
)

gp0 <- ggplot(df_ap, aes(x = t, y = y, color = set)) +
  geom_line(lwd = 1) +
  geom_line(data = reshape2::melt(df_ap_fc, id.vars = "t"),
            aes(x = t, y = value, color = variable), lwd = 0.75, linetype = "dashed") +
  scale_color_manual(values = c("train" = "black", "test" = "gold3",
                                "Xreg" = "darkblue", "SARIMA" = "darkred")) +
  labs(title = "AirPassengers Forecast: ARIMA+Xreg vs SARIMA",
       x = "Time", y = "Passengers (thousands)",
       color = "Series/Model") +
  theme_minimal()
print(gp0)
ggplotly(gp0)

# =============================================================================
# Final Conclusions and Recommendations
# =============================================================================
cat("\n================ FINAL RECOMMENDATIONS =================\n")

# EuStockMarkets analysis conclusions
if (all(granger_stocks_df$Causal) && rank_pos >= 1) {
  cat("EuStockMarkets: Bidirectional causality and cointegration → Multivariate model (VECM/VAR) is justified.\n")
} else if (any(granger_stocks_df$Causal)) {
  cat("EuStockMarkets: Unidirectional causality → Modeling with external regressors is acceptable.\n")
} else {
  cat("EuStockMarkets: No causality detected → Joint modeling is not necessary.\n")
}

cat(sprintf("DAX Forecast Quality: MTS RMSE=%.3f / MAPE=%.2f%%  vs  Xreg RMSE=%.3f / MAPE=%.2f%%\n",
            eu_rmse_mts, eu_mape_mts, eu_rmse_xrg, eu_mape_xrg))

cat("AirPassengers: Johansen test not applicable (Fourier harmonics are time-determined) → Valid to compare SARIMA and ARIMA+Fourier Xreg.\n")
cat(sprintf("Forecast Quality (12 months): Xreg RMSE=%.1f / MAPE=%.2f%%  vs  SARIMA RMSE=%.1f / MAPE=%.2f%%\n",
            ap_rmse_xreg, ap_mape_xreg, ap_rmse_sar, ap_mape_sar))

cat("================================================\n")

# Summary of key findings
cat("\n=== KEY INSIGHTS ===\n")
cat("1. For multivariate systems with cointegration, MTS models (VECM/VAR) provide robust forecasting\n")
cat("2. When causality is unidirectional, ARIMAX with external regressors can be more efficient\n")
cat("3. For seasonal patterns, Fourier harmonics in ARIMAX can compete with traditional SARIMA\n")
cat("4. Model selection should consider both statistical tests and forecast performance metrics\n")



