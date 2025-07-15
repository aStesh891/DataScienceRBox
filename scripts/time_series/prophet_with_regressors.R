# =============================================
# Prophet Model with External Regressors (CPI)
# Example: Australian Accommodation Takings
# =============================================

# ---------------------------
# 1. Libraries and Data Prep
# ---------------------------
library(fpp3)
library(fable.prophet)
library(plotly)

# Load dataset
data("aus_accommodation")

# Train/test split (1998 Q1 - 2012 Q2 / 2012 Q3 - ...)
train <- aus_accommodation %>% 
  filter_index("1998 Q1" ~ "2012 Q2")

test <- aus_accommodation %>% 
  filter_index("2012 Q3" ~ .)

# ---------------------------
# 2. Model Training
# ---------------------------
fit <- train %>%
  model(
    # Base Prophet model
    prophet_base = prophet(Takings),
    
    # Prophet with CPI as regressor
    prophet_cpi = prophet(
      Takings ~ xreg(CPI) + 
        season(period = 4, type = "multiplicative")
    )
  )

# ---------------------------
# 3. Forecasting
# ---------------------------
fc <- fit %>% 
  forecast(new_data = test)

# ---------------------------
# 4. Visualization
# ---------------------------
plot <- autoplot(fc, aus_accommodation) +
  labs(
    title = "Accommodation Takings Forecast: Prophet with/without CPI",
    subtitle = "Multiplicative seasonality with external regressor",
    y = "Takings (million A$)",
    x = "Quarter",
    color = "Model"
  ) +
  scale_color_manual(
    values = c(prophet_base = "purple", prophet_cpi = "orange")
  ) +
  theme_minimal(base_size = 12)

# Interactive version
ggplotly(plot)

# ---------------------------
# 5. Model Accuracy
# ---------------------------
accuracy(fc, test) %>% 
  knitr::kable(digits = 3)

