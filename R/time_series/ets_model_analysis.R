#' Compare ETS Models and Visualize Results
#'
#' Fits multiple ETS models to a time series, compares their performance metrics (AIC, MSE, sMAPE),
#' and generates a plot of fitted values and forecasts.
#'
#' @param data A data frame containing the time series data (must include columns `ds` for dates and `y` for values).
#' @param models A character vector of ETS model types to compare (e.g., `c("ANN", "AAN", "AAA")`).
#' @param forecast_horizon Integer specifying the number of periods to forecast (default: 15).
#' @return A list containing:
#'   - `comparison`: Data frame of model performance metrics.
#'   - `plot`: Interactive plot of fitted values and forecasts (using `plotly`).
#' @examples
#' petrol <- read.csv("Diesel_ready.csv")
#' results <- compare_ets_models(petrol, c("ANN", "AAdN", "AAN", "AAA"))

compare_ets_models <- function(data, models, forecast_horizon = 15) {
  library(smooth)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(tidyr)
  
  # Validate input
  if (!all(c("ds", "y") %in% colnames(data))) {
    stop("Input data must include columns 'ds' (date) and 'y' (value).")
  }
  data$ds <- as.Date(data$ds)
  
  # Fit models and collect metrics
  results <- lapply(models, function(m) {
    fit <- es(data$y, model = m, silent = TRUE)
    actual_model <- fit$model
    list(
      model = if (m == "AAA" && actual_model == "ETS(AAN)") "AAA → AAN" else m,
      aic = AIC(fit),
      mse = mean((fit$fitted - data$y)^2, na.rm = TRUE),
      smape = mean(abs(fit$fitted - data$y) / ((abs(fit$fitted) + abs(data$y)) / 2), na.rm = TRUE) * 100,
      fit = fit
    )
  })
  
  # Create comparison table
  comparison <- data.frame(
    Model = sapply(results, function(x) x$model),
    AIC = sapply(results, function(x) x$aic),
    MSE = sapply(results, function(x) x$mse),
    sMAPE = sapply(results, function(x) x$smape)
  ) %>% arrange(AIC)
  
  # Generate fitted values and forecasts
  fitted_df <- data.frame(ds = data$ds, actual = data$y)
  forecast_dates <- seq(max(data$ds) + 1, by = "day", length.out = forecast_horizon)
  
  for (res in results) {
    fitted_df[[paste0(res$model, "_fitted")]] <- res$fit$fitted
  }
  
  forecast_dfs <- lapply(results, function(res) {
    fc <- forecast(res$fit, h = forecast_horizon, interval = "none")
    data.frame(
      ds = forecast_dates,
      value = as.numeric(fc$mean),
      model = res$model
    )
  })
  
  # Prepare data for plotting
  fitted_long <- fitted_df %>%
    pivot_longer(cols = -c(ds, actual), names_to = "Series", values_to = "Value") %>%
    mutate(Series = gsub("_fitted", "", Series))
  
  forecast_all <- bind_rows(forecast_dfs)
  
  # Create plot
  p <- ggplot() +
    geom_line(data = fitted_long, aes(x = ds, y = Value, color = Series, linetype = "Fitted"), size = 0.5) +
    geom_line(data = forecast_all, aes(x = ds, y = value, color = model, linetype = "Forecast"), size = 0.25, alpha = 0.7) +
    geom_line(data = fitted_df, aes(x = ds, y = actual, color = "Actual"), size = 0.25) +
    geom_smooth(data = data, aes(x = ds, y = y), method = "loess", formula = y ~ x, se = FALSE, col="gold", size = 0.5)+
    scale_linetype_manual(values = c("Fitted" = "solid", "Forecast" = "dashed")) +
    labs(
      title = "ETS Model Comparison",
      x = "Date",
      y = "Value",
      color = "Model",
      linetype = "Type"
    ) +
    theme_minimal()
  
  list(
    comparison = comparison,
    plot = ggplotly(p)
  )
}
