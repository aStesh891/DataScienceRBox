# ==============================================
# Tesla Stock Price Forecasting with GAM and Structural Breaks
# ==============================================

# === 1. Load Required Libraries ===
library(strucchange)
library(mgcv)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)

# === 2. Data Loading and Preparation ===
cat("Loading Tesla stock price data from data/processed/Tesla_Stock_Price.csv...\n")
tesla <- read.delim("data/processed/Tesla_Stock_Price.csv", sep = ';')

cat("Preparing data...\n")
tesla_data <- tesla %>%
  mutate(
    Date = as.Date(ds, format = "%m/%d/%Y"),
    Time = as.numeric(Date)
  ) %>%
  arrange(Date) %>%
  select(Date, Price = y, Time)

cat("Data prepared successfully. Observations:", nrow(tesla_data), 
    "from", as.character(min(tesla_data$Date)), 
    "to", as.character(max(tesla_data$Date)), "\n")

# === 3. Train-Test Split ===
cat("Creating train/test split (1109 training observations)...\n")
train <- tesla_data[1:1109, ]
test <- tesla_data[1110:nrow(tesla_data), ]  # Fixed variable name from original

cat("Training set:", nrow(train), "obs, Test set:", nrow(test), "obs\n")

# === 4. Structural Break Analysis ===
cat("Detecting structural breaks...\n")
bp <- breakpoints(Price ~ Time, data = train)
breakpoint_idx <- bp$breakpoints
breakpoint_dates <- train$Date[breakpoint_idx]

if(length(breakpoint_idx) > 0) {
  cat("Found", length(breakpoint_idx), "structural break(s) at:", 
      as.character(breakpoint_dates), "\n")
} else {
  cat("No structural breaks detected.\n")
}

# === 5. Create Segments ===
cat("Creating time segments...\n")
train$segment <- cut(train$Time,
                     breaks = c(-Inf, train$Time[breakpoint_idx], Inf),
                     labels = paste0("seg", seq_len(length(breakpoint_idx) + 1)))

# === 6. GAM Model Training ===
cat("Training GAM model...\n")
gam_model <- gam(Price ~ s(Time, by = segment), data = train)
cat("Model trained. Summary:\n")
print(summary(gam_model))

# === 7. Generate Predictions ===
cat("Generating predictions...\n")
train$Fitted <- predict(gam_model)

# For test set, use last segment from training
test$segment <- factor(tail(train$segment, 1), levels = levels(train$segment))
test$Fitted <- predict(gam_model, newdata = test)

# === 8. Combine Data for Plotting ===
combined <- bind_rows(
  tesla_data %>% mutate(type = "Actual", Fitted = Price),
  train %>% mutate(type = "Fitted"),
  test %>% mutate(type = "Forecast")
)

# correct order of levels of factor variable
combined$type <- factor(combined$type, levels = c("Actual", "Fitted", "Forecast"))

# === 9. Main Visualization ===
cat("Creating main plot...\n")
gp <- ggplot() +
  geom_line(data = combined %>% filter(type == "Actual"), 
            aes(x = Date, y = Price, color = type), linewidth = 0.6) +
  geom_line(data = combined %>% filter(type == "Fitted"), 
            aes(x = Date, y = Fitted, color = type), linewidth = 0.4) +
  geom_line(data = combined %>% filter(type == "Forecast"), 
            aes(x = Date, y = Fitted, color = type), linewidth = 0.4) +
  {if(length(breakpoint_dates) > 0)
    geom_vline(xintercept = breakpoint_dates, linetype = "dashed", color = "blue")} +
  geom_vline(xintercept = min(test$Date), linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("Actual" = "darkred", "Fitted" = "black", "Forecast" = "darkblue"),
    labels = c("Actual" = "Actual Price", "Fitted" = "Model Fit", "Forecast" = "Forecast")) +
  labs(
    title = "Tesla Stock Price Forecast with Piecewise GAM Model",
    subtitle = ifelse(length(breakpoint_dates) > 0, "Blue dashed lines show structural break points", ""),
    x = "Date", 
    y = "Price ($)", 
    color = "Type") +
  theme_minimal()

ggplotly(gp)

# === 10. Model Diagnostics ===
cat("\n=== Model Diagnostics ===\n")
train$Residuals <- residuals(gam_model)

# Residual plot
p_resid <- ggplot(train, aes(x = Date, y = Residuals)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Model Residuals Over Time", x = "Date", y = "Residuals") +
  theme_minimal()

# Residual distribution
p_dist <- ggplot(train, aes(x = Residuals)) +
  geom_density(fill = "steelblue", alpha = 0.7) +
  labs(title = "Residual Distribution") +
  theme_minimal()

# ACF plot
p_acf <- ggAcf(train$Residuals) +
  labs(title = "Residual Autocorrelation") +
  theme_minimal()

# Display diagnostics
gridExtra::grid.arrange(p_resid, p_dist, p_acf, ncol = 1)

cat("\nAnalysis complete.\n")

