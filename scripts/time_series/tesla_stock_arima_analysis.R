# Tesla Stock Price Prediction using ARIMA Models

########################################################
#               Load Libraries                         #
########################################################
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(plotly)
library(lubridate)

########################################################
#         Data Loading and Preprocessing               #
########################################################
cat("\n[STATUS] Loading and preprocessing data...\n")

# Load processed Tesla stock data
tryCatch({
  tesla <- read.delim("data/processed/Tesla_Stock_Price.csv", sep = ';')
  cat("[SUCCESS] Data loaded successfully. Dimensions:", dim(tesla), "\n")
}, error = function(e) {
  cat("[ERROR] Failed to load data:", e$message, "\n")
  stop("Terminating script due to data loading error")
})

tesla$ds <- as.Date(tesla$ds, format = "%m/%d/%Y")
tesla <- arrange(tesla, ds)

# Check for NA values in price
na_check <- sum(is.na(tesla$y))
if(na_check > 0) {
  cat("[WARNING]", na_check, "NA values found in price data. Handling them...\n")
  tesla$y <- na.approx(tesla$y)  # Linear interpolation for NAs
}

########################################################
#         Stationarity Check and Differencing          #
########################################################
cat("\n[STATUS] Checking stationarity...\n")

tryCatch({
  adf_test <- adf.test(na.omit(tesla$y))
  cat("\nAugmented Dickey-Fuller Test Results:\n")
  cat("ADF Statistic:", round(adf_test$statistic, 4), "\n")
  cat("p-value:", round(adf_test$p.value, 4), "\n")
  
  if(adf_test$p.value > 0.05) {
    cat("[INFO] Series is non-stationary (p > 0.05). Differencing required.\n")
  } else {
    cat("[INFO] Series appears stationary (p ≤ 0.05)\n")
  }
}, error = function(e) {
  cat("[ERROR] Stationarity test failed:", e$message, "\n")
})

########################################################
#         Train-Test Split and TS Creation             #
########################################################
cat("\n[STATUS] Creating train-test split...\n")

# 80-20% split (rounded to whole numbers)
train_size <- round(0.8 * nrow(tesla))
test_size <- nrow(tesla) - train_size

train <- tesla[1:train_size, ]
test <- tesla[(train_size+1):nrow(tesla), ]

cat("[INFO] Training period:", format(min(train$ds), "%Y-%m-%d"), "to", 
    format(max(train$ds), "%Y-%m-%d"), "(", nrow(train), "observations )\n")
cat("[INFO] Test period:", format(min(test$ds), "%Y-%m-%d"), "to", 
    format(max(test$ds), "%Y-%m-%d"), "(", nrow(test), "observations )\n")

# Create time series object with proper date handling
tryCatch({
  start_year <- year(min(train$ds))
  start_day <- yday(min(train$ds))
  
  ts_train <- ts(train$y, 
                 frequency = 252, 
                 start = c(start_year, start_day))
  
  cat("[SUCCESS] Time series object created with", length(ts_train), "observations\n")
  cat("[DEBUG] Time series start:", start(ts_train), "\n")
  cat("[DEBUG] Time series end:", end(ts_train), "\n")
}, error = function(e) {
  cat("[ERROR] Failed to create time series:", e$message, "\n")
  cat("[DEBUG] First 5 dates in training set:\n")
  print(head(train$ds, 5))
  stop("Cannot proceed without valid time series")
})

########################################################
#        Automatic Parameter Selection                 #
########################################################
cat("\n[STATUS] Running auto.arima for parameter selection...\n")

auto_model <- tryCatch({
  auto.arima(ts_train,
             seasonal = TRUE,
             stepwise = TRUE,
             approximation = TRUE,
             trace = TRUE,
             max.p = 5,
             max.q = 5,
             max.d = 10)
}, error = function(e) {
  cat("[ERROR] auto.arima failed:", e$message, "\n")
  return(NULL)
})

if(!is.null(auto_model)) {
  cat("\nBest Auto ARIMA Model:\n")
  print(auto_model)
  cat("AIC:", AIC(auto_model), "\n")
} else {
  cat("[WARNING] Proceeding without auto.arima results\n")
}

########################################################
#        Manual Parameter Tuning                       #
########################################################
cat("\n[STATUS] Beginning manual parameter tuning...\n")

# Determine differencing orders
nd <- tryCatch({
  ndiffs(ts_train)
}, error = function(e) {
  cat("[WARNING] ndiffs failed, using default d=1:", e$message, "\n")
  return(1)
})

nsd <- tryCatch({
  nsdiffs(ts_train)
}, error = function(e) {
  cat("[WARNING] nsdiffs failed, using default D=0:", e$message, "\n")
  return(0)
})

d <- nd
D <- nsd

cat("\nDifferencing Parameters:\n")
cat("Non-seasonal differencing (d):", d, "\n")
cat("Seasonal differencing (D):", D, "\n")

# Step 1: Find optimal q with p=0
aic_res_q <- numeric()
models_q <- list()

for (q in 1:10) {
  fit <- try(arima(ts_train, order = c(0, d, q)), silent = TRUE)
  if(!inherits(fit, "try-error")) {
    models_q[[q]] <- fit
    aic_res_q[q] <- AIC(fit)
  } else {
    aic_res_q[q] <- NA
  }
}

best_q <- which.min(aic_res_q)
cat("\nOptimal q:", best_q, "with AIC =", aic_res_q[best_q], "\n")

# Step 2: Find optimal p with selected q
aic_res_p <- rep(NA, 10)
models_p <- vector("list", 10)

for (p in 1:10) {
  fit <- try(arima(ts_train, order = c(p, d, best_q)), silent = TRUE)
  if(!inherits(fit, "try-error")) {
    models_p[[p]] <- fit
    aic_res_p[p] <- AIC(fit)
  }
}

# Remove NAs and find minimal AIC
aic_df <- data.frame(p = 1:10, AIC = aic_res_p) %>% filter(!is.na(AIC))
best_p <- aic_df$p[which.min(aic_df$AIC)]

# Final model
best_model <- models_p[[best_p]]
best_model_name <- paste0("ARIMA(", best_p, ",", d, ",", best_q, ")")

cat("\nBest Manual Model:", best_model_name, "with AIC =", AIC(best_model), "\n")

# Model comparison
cat("\nModel Comparison:\n")
cat("Manual Model AIC:", AIC(best_model), "\n")
cat("Auto ARIMA AIC:", AIC(auto_model), "\n")

########################################################
#               Results Visualization                  #
########################################################

# Build final model with proper fitted values
final_model <- Arima(ts_train, 
                     order = c(best_p, d, best_q),
                     method = "ML")  # Ensure fitted values are available

# Generate forecasts
forecast_values <- forecast(final_model, h = nrow(test))

# Prepare data frames for visualization with consistent column names
actual_df <- data.frame(
  date = tesla$ds,
  price = tesla$y,
  type = "Actual"
)

fitted_df <- data.frame(
  date = train$ds,  # Используем ds вместо date для согласованности
  price = as.numeric(fitted(final_model)),
  type = "Fitted"
)

forecast_df <- data.frame(
  date = test$ds,   # Используем ds вместо date для согласованности
  price = as.numeric(forecast_values$mean),
  lower = as.numeric(forecast_values$lower[,2]),
  upper = as.numeric(forecast_values$upper[,2]),
  type = "Forecast"
)

# Проверяем соответствие столбцов перед объединением
cat("\n[DEBUG] Checking column names before merging:\n")
cat("Actual_df:", names(actual_df), "\n")
cat("Fitted_df:", names(fitted_df), "\n")
cat("Forecast_df:", names(forecast_df[,1:3]), "\n")

# Объединяем данные для визуализации
plot_df <- bind_rows(
  actual_df,
  fitted_df,
  forecast_df %>% select(date, price, type)  # Явно выбираем нужные столбцы
)

# Проверяем результат
cat("\n[DEBUG] Final plot_df structure:\n")
str(plot_df)

# Main plot with improved formatting
gp <- ggplot() +
  geom_line(data = actual_df,
            aes(x = date, y = price, color = "Actual")) +
  geom_line(data = fitted_df, 
            aes(x = date, y = price, color = "Fitted")) +
  geom_line(data = forecast_df, 
            aes(x = date, y = price, color = "Forecast")) +
  geom_ribbon(data = forecast_df,
              aes(x = date, ymin = lower, ymax = upper, 
                  fill = "Confidence Interval"),alpha = 0.2) +
  geom_vline(xintercept = max(train$ds), 
             linetype = "dashed", color = "gray") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_color_manual(name = "Series",
                     values = c("Actual" = "darkred", 
                                "Fitted" = "darkgreen", 
                                "Forecast" = "blue")) +
  scale_fill_manual(name = "Interval",
                    values = "blue") +
  labs(title = "Tesla Stock Price Prediction using ARIMA",
       subtitle = paste("Model:", best_model_name),
       x = "Date", y = "Price ($)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(gp)

# Model diagnostics
checkresiduals(final_model)

########################################################
#               Model Evaluation                       #
########################################################
cat("\n[STATUS] Evaluating model accuracy...\n")

# Ensure test data is clean and aligned with forecast
if(nrow(test) != length(forecast_values$mean)) {
  common_length <- min(nrow(test), length(forecast_values$mean))
  test <- test[1:common_length, ]
  forecast_values$mean <- forecast_values$mean[1:common_length]
}

# Calculate accuracy metrics
accuracy_metrics <- accuracy(forecast_values, test$y)

# Calculate MAPE manually
test$forecast <- forecast_values$mean
test$ape <- abs((test$y - test$forecast) / test$y) * 100
mape <- mean(test$ape, na.rm = TRUE)

cat("\nTest Set Accuracy Metrics:\n")
print(accuracy_metrics)
cat("\nMAPE (Mean Absolute Percentage Error):", round(mape, 2), "%\n")


