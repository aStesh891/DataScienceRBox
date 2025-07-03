# Tesla Stock Price Prediction using Fable ARIMA Models
# Experiments:
#             - Analysis with a numeric index instead of dates
#             - Models with high orders (ARIMA(5,2,5))

########################################################
#               Load Libraries                         #
########################################################
library(tsibble)
library(fpp3)
library(fable)
library(fabletools)
library(dplyr)
library(zoo)

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

# Convert to tsibble and complement to regularity
tesla <- tesla %>%
  arrange(ds) %>%
  complete(ds = seq(min(ds), max(ds), by = "day")) %>%   # add missing dates
  fill(y, .direction = "downup") %>%                     # fill NA
  as_tsibble(index = ds)                                 # <--- important! convert to tsibble again

autoplot(tesla)

########################################################
#         v1 ARIMA Models Selection                    #
########################################################
caf_fit <- tesla%>%
  model(arima010 = ARIMA(y ~ pdq(0,1,0)),
        arima110 = ARIMA(y ~ pdq(1,1,0)),
        arima011 = ARIMA(y ~ pdq(0,1,1)),
        arima111 = ARIMA(y ~ pdq(1,1,1)),
        arima113 = ARIMA(y ~ pdq(1,1,3)),
        arima311 = ARIMA(y ~ pdq(3,1,1)),
        stepwise = ARIMA(y),
        search = ARIMA(y, stepwise=FALSE))

glance(caf_fit)%>%
  arrange(AICc)%>%
  select(.model:BIC)

caf_fit%>%
  select(arima311)%>%
  gg_tsresiduals()

augment(caf_fit)%>%
  filter(.model=='arima311') %>%
  features(.innov, ljung_box, lag = 20, dof =4)

caf_fit%>%
  forecast(h=20) %>%
  filter(.model=='arima311') %>%
  autoplot(tesla)

########################################################
#        v2 ARIMA Models Selection                     #
########################################################

# Replace dates with indexes from 1 to n
tesla_idx <- tesla %>%
  mutate(t = row_number()) %>%
  select(t, y) %>%
  as_tsibble(index = t)

autoplot(tesla_idx)

tesla_idx%>%
  gg_tsdisplay(difference(y), plot_type='partial')

caf_fit_idx <- tesla_idx%>%
  model(arima010 = ARIMA(y ~ pdq(0,1,0)),
        arima110 = ARIMA(y ~ pdq(1,1,0)),
        arima011 = ARIMA(y ~ pdq(0,1,1)),
        arima111 = ARIMA(y ~ pdq(1,1,1)),
        arima113 = ARIMA(y ~ pdq(1,1,3)),
        arima525 = ARIMA(y ~ 1 + pdq(5,2,5)+PDQ(0,1,0)),
        arima313 = ARIMA(y ~ 1 + pdq(3,1,3)),
        stepwise = ARIMA(y),
        search = ARIMA(y, stepwise=FALSE))

glance(caf_fit_idx)%>%
  arrange(AICc)%>%
  select(.model:BIC)

caf_fit_idx%>%
  select(arima525)%>%
  gg_tsresiduals()

augment(caf_fit_idx)%>%
  filter(.model=='arima525') %>%
  features(.innov, ljung_box, lag = 10, dof = 10)

caf_fit_idx%>%
  forecast(h=20) %>%
  filter(.model=='stepwise') %>%
  autoplot(tesla_idx)

########################################################
#                     Conclusion:                      #
########################################################
# Best models:
#  for dated data: ARIMA(0,1,0) (AICc = 5377) → Simple random walk model
#                  Residuals: There is weak autocorrelation (LB test p-value ≈ 0)
#
#  for numeric index: ARIMA(5,2,5) (AICc = 5348) → Complex model with trend
#                     Problem: High order of differentiation (d=2) may lead to overfitting
#
# The new approach with fable provides more structured workflow, 
#  but does not solve the key problem - low forecast accuracy due to:
#  - Fundamental changes in Tesla's business (2020)
#  - Unaccounted factors (market shocks, news)
