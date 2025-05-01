#' @title Time Series Stationarity Analysis with Visualization
#' @description This function analyzes the stationarity of a given numeric column or time series object using ADF and KPSS tests. It also visualizes the original series and its first difference. Results are interpreted to help determine stationarity.
#' @param df A data frame or time series object
#' @param target_col The name of the numeric column to analyze (only for data frames)
#' @param start_date The start date for the time series (optional, used if input is a data frame)
#' @param freq The frequency of the time series (e.g., "month", "week", "day")
#' @param ts_name Optional title for the plots
#' @return A list containing the time series object and ADF/KPSS test results
#' @examples
#' ts_stationarity_analysis(AirPassengers, freq = "month", ts_name = "Air Passengers")
#'
#' data(UKgas)
#' ts_stationarity_analysis(UKgas, freq = "quarter", ts_name = "UK Gas Consumption")
#'
#' data(nottem)
#' ts_stationarity_analysis(nottem, freq = "month", ts_name = "Nottingham Temperature")

ts_stationarity_analysis <- function(df, target_col = NULL, start_date = NULL, freq = "month", ts_name = "Time Series") {
  library(ggplot2)
  library(tseries)
  library(forecast)
  
  freq_val <- switch(freq,
                     "month" = 12,
                     "week" = 52,
                     "day" = 365,
                     "quarter" = 4,
                     1)
  
  if (inherits(df, "ts")) {
    ts_obj <- df
    ts_data <- as.numeric(df)
    if (is.null(start_date)) {
      start_year <- start(df)[1]
      start_period <- start(df)[2]
      start_date <- switch(freq,
                           "month" = as.Date(paste0(start_year, "-", sprintf("%02d", start_period), "-01")),
                           "quarter" = as.Date(paste0(start_year, "-", sprintf("%02d", (start_period - 1) * 3 + 1), "-01")),
                           "week" = as.Date(paste0(start_year, "-01-01")) + (start_period - 1) * 7,
                           "day" = as.Date(paste0(start_year, "-01-01")) + (start_period - 1),
                           as.Date(paste0(start_year, "-01-01")))
    }
  } else {
    if (is.null(target_col)) {
      target_col <- names(df)[sapply(df, is.numeric)][1]
    }
    ts_data <- df[[target_col]]
    ts_obj <- ts(ts_data, frequency = freq_val)
    if (is.null(start_date)) {
      start_date <- Sys.Date()
    }
  }
  
  date_seq <- seq(as.Date(start_date), by = freq, length.out = length(ts_data))
  df_plot <- data.frame(date = date_seq, value = ts_data)
  
  safe_test <- function(test_expr) {
    tryCatch(test_expr, error = function(e) list(p.value = NA))
  }
  
  adf_result <- safe_test(adf.test(ts_obj))
  kpss_result <- safe_test(kpss.test(ts_obj))
  
  cat("\n--- Stationarity Test Results (Original Series) ---\n")
  cat(sprintf("ADF test p-value: %.4f | H0: Non-stationary => %s\n",
              adf_result$p.value,
              ifelse(is.na(adf_result$p.value), "NA", ifelse(adf_result$p.value < 0.05, "Reject H0 (Stationary)", "Fail to Reject H0"))))
  cat(sprintf("KPSS test p-value: %.4f | H0: Stationary => %s\n",
              kpss_result$p.value,
              ifelse(is.na(kpss_result$p.value), "NA", ifelse(kpss_result$p.value < 0.05, "Reject H0 (Non-stationary)", "Fail to Reject H0"))))
  
  if (is.na(adf_result$p.value) || is.na(kpss_result$p.value)) {
    conclusion <- "Conclusion: Could not determine due to NA values."
  } else if (adf_result$p.value < 0.05 && kpss_result$p.value > 0.05) {
    conclusion <- "Conclusion: Likely stationary."
  } else if (adf_result$p.value > 0.05 && kpss_result$p.value < 0.05) {
    conclusion <- "Conclusion: Likely non-stationary."
  } else {
    conclusion <- "Conclusion: Inconclusive or borderline. Use caution."
  }
  cat(conclusion, "\n")
  
  p <- ggplot(df_plot, aes(x = date, y = value)) +
    geom_line(color = "gold", lwd = 0.5) +
    geom_point(color = "steelblue", size = 1) +
    geom_smooth(method = "loess", color = "darkred", se = FALSE) +
    labs(title = paste("Time Series Plot:", ts_name),
         x = "Date", y = "Value") +
    theme_minimal()
  print(p)
  
  diff_obj <- diff(ts_obj)
  cat("\n--- Stationarity Test Results (First Differences) ---\n")
  adf_diff <- safe_test(adf.test(diff_obj))
  kpss_diff <- safe_test(kpss.test(diff_obj))
  cat(sprintf("ADF test p-value: %.4f | H0: Non-stationary => %s\n",
              adf_diff$p.value,
              ifelse(is.na(adf_diff$p.value), "NA", ifelse(adf_diff$p.value < 0.05, "Reject H0 (Stationary)", "Fail to Reject H0"))))
  cat(sprintf("KPSS test p-value: %.4f | H0: Stationary => %s\n",
              kpss_diff$p.value,
              ifelse(is.na(kpss_diff$p.value), "NA", ifelse(kpss_diff$p.value < 0.05, "Reject H0 (Non-stationary)", "Fail to Reject H0"))))
  
  if (is.na(adf_diff$p.value) || is.na(kpss_diff$p.value)) {
    diff_conclusion <- "Conclusion: Could not determine due to NA values."
  } else if (adf_diff$p.value < 0.05 && kpss_diff$p.value > 0.05) {
    diff_conclusion <- "Conclusion: Likely stationary."
  } else if (adf_diff$p.value > 0.05 && kpss_diff$p.value < 0.05) {
    diff_conclusion <- "Conclusion: Likely non-stationary."
  } else {
    diff_conclusion <- "Conclusion: Inconclusive or borderline. Use caution."
  }
  cat(diff_conclusion, "\n")
  
  forecast::ggtsdisplay(ts_obj, main = paste(ts_name, ": Original Series"))
  forecast::ggtsdisplay(diff_obj, main = paste(ts_name, ": First Differences"))
  
  invisible(list(original = ts_obj, diff = diff_obj, adf = adf_result, kpss = kpss_result, adf_diff = adf_diff, kpss_diff = kpss_diff))
}





