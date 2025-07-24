# ==================================================
# Prophet Model Comparison for AirPassengers Dataset
# Standard vs Tuned Parameters
# ==================================================

# ---------------------------
# 1. Libraries and Data Prep
# ---------------------------
library(prophet)
library(dplyr)
library(ggplot2)
library(lubridate)

# Prepare AirPassengers data
start_date <- as.Date("1949-01-01")
df <- data.frame(ds = seq.Date(from = start_date, 
                 by = "month", 
                 length.out = length(AirPassengers)),
  y = as.numeric(AirPassengers)
) %>%
  mutate(ds = as.Date(ds))  # ! ensure Date format

# ---------------------------
# 2. Model Configuration
# ---------------------------
h <- 24  # Forecast horizon (2 years)
cutoff_date <- max(df$ds)  # Train/test cutoff

# Base model
model1 <- prophet(df)

# Tuned model
model2 <- prophet(df,
  yearly.seasonality = TRUE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = 'multiplicative',  # reasonable for exponential growth
  changepoint.prior.scale = 0.15,       # more flexible trend
  seasonality.prior.scale = 10          # allow seasonality more
)

# ---------------------------
# 3. Forecasting
# ---------------------------
# Generate future dataframes
future1 <- make_future_dataframe(model1, periods = h, freq = "month")
future2 <- make_future_dataframe(model2, periods = h, freq = "month")

# Make predictions
forecast1 <- predict(model1, future1) %>%
  mutate(ds = as.Date(ds), model = "Standard")

forecast2 <- predict(model2, future2) %>%
  mutate(ds = as.Date(ds), model = "Tuned")

# ---------------------------
# 4. Visualizations
# ---------------------------

### Plot 1: Standard Model Forecast
plot_standard <- ggplot() +
  geom_line(data = df, aes(x = ds, y = y, color = "Actual Data"), linewidth = 0.6) +
  geom_line(data = forecast1, aes(x = ds, y = yhat, color = "Forecast"), linewidth = 0.6) +
  geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "black") +
  scale_color_manual(name = "", values = c("Actual Data" = "darkred", "Forecast" = "darkblue")) +
  labs(title = "AirPassengers: Prophet Forecast",
       subtitle = "Default Parameters", y = "Passengers", x = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_standard)

### Plot 2: Model Comparison
plot_comparison <- ggplot() +
  geom_line(data = df, aes(x = ds, y = y), color = "black", linewidth = 0.8) +
  geom_line(data = bind_rows(forecast1, forecast2),
    aes(x = ds, y = yhat, color = model), linewidth = 0.7, linetype = "dashed") +
  geom_vline(xintercept = cutoff_date, linetype = "dashed", color = "red") +
  scale_color_manual(name = "Model", values = c("Standard" = "#6a329f", "Tuned" = "#ff7f0e")) +
  labs(title = "AirPassengers: Model Comparison",
       subtitle = "Standard vs Tuned Prophet", y = "Passengers", x = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_comparison)


