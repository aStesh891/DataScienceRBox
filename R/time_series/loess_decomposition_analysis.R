#' @title LOESS Time Series Decomposition Analysis
#' @description 
#' Performs complete decomposition of time series data using LOESS smoothing to extract
#' trend and seasonal components. The function provides:
#' \itemize{
#'   \item Decomposition into trend and seasonal components using LOESS regression
#'   \item Quantitative assessment of trend and seasonality strength (0-1 scale)
#'   \item Statistical stationarity testing (ADF and KPSS tests)
#'   \item Visualization of original series with extracted components
#'   \item Detailed diagnostic outputs
#' }
#' 
#' @param df Data frame containing the time series with columns:
#'   \itemize{
#'     \item \code{Date}: Date or POSIXct vector with time points
#'     \item \code{Value}: Numeric vector with observed values
#'   }
#' @param ts_name Optional character string specifying the time series name for plot titles 
#'   (default: "Time Series")
#' @param span Numeric value between 0 and 1 controlling the degree of smoothing:
#'   \itemize{
#'     \item Smaller values = more localized smoothing
#'     \item Larger values = smoother trend (default: 0.5)
#'   }
#' @param show_plots Logical indicating whether to display plots (default: TRUE)
#' 
#' @return Returns an invisible list object containing:
#' \itemize{
#'   \item \code{data}: Data frame with original data and decomposition results
#'   \item \code{trend_strength}: Numeric measure of trend dominance (0-1)
#'   \item \code{seasonality_strength}: Numeric measure of seasonality (0-1)
#'   \item \code{stationarity_tests}: List containing:
#'     \itemize{
#'       \item \code{ADF}: Augmented Dickey-Fuller test results
#'       \item \code{KPSS}: KPSS test results
#'     }
#'   \item \code{plots}: List of ggplot objects (trend and seasonal components)
#'   \item \code{model}: The fitted LOESS model object
#' }
#' 
#' @examples
#' # Example 1: Basic monthly data with trend
#' set.seed(123)
#' monthly_data <- data.frame(
#'   Date = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 36),
#'   Value = 100 + 2*(1:36) + rnorm(36, sd = 5)
#' )
#' decomp1 <- loess_decomposition_analysis(monthly_data, "Monthly Trend")
#' 
#' # Example 2: Seasonal daily data
#' daily_data <- data.frame(
#'   Date = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 365),
#'   Value = 200 + 0.2*(1:365) + 20*sin(2*pi*(1:365)/365) + rnorm(365, sd = 8)
#' )
#' decomp2 <- loess_decomposition_analysis(daily_data, "Daily Seasonal", span = 0.2)
#' 
#' # Example 3: AirPassengers dataset
#' \dontrun{
#' library(zoo)
#' ap_data <- data.frame(
#'   Date = as.Date(as.yearmon(time(AirPassengers))),
#'   Value = as.numeric(AirPassengers)
#' )
#' decomp3 <- loess_decomposition_analysis(ap_data, "AirPassengers", span = 0.3)
#' }
#' 
#' @export
#' @importFrom ggplot2 ggplot geom_line labs theme_minimal
#' @importFrom stats loess predict var ts
#' @importFrom tseries adf.test kpss.test

loess_decomposition_analysis <- function(df, ts_name = "Time Series", 
                                         span = 0.5, show_plots = TRUE) {
  
  # Validate input structure
  if (!all(c("Date", "Value") %in% names(df))) {
    stop("Input dataframe must contain 'Date' and 'Value' columns")
  }
  
  # Validate data types
  if (!inherits(df$Date, c("Date", "POSIXct"))) {
    stop("Date column must be either Date or POSIXct class")
  }
  if (!is.numeric(df$Value)) {
    stop("Value column must be numeric")
  }
  
  # Order by date
  df <- df[order(df$Date), ]
  
  # Create numeric date for modeling
  df$Date_numeric <- as.numeric(df$Date)
  
  # Detect frequency
  time_diff <- as.numeric(difftime(df$Date[2], df$Date[1], units = "days"))
  freq <- if (time_diff >= 28) 12 else if (time_diff >= 7) 52 else 365
  
  # Fit LOESS model with error handling
  trend_model <- tryCatch({
    loess(Value ~ Date_numeric, data = df, span = span,
          na.action = na.exclude, 
          control = loess.control(surface = "direct"))
  }, error = function(e) {
    stop("LOESS fitting failed: ", e$message)
  })
  
  # Extract components
  df$Trend <- predict(trend_model)
  df$Seasonal <- df$Value - df$Trend
  
  # Calculate component strengths
  trend_str <- max(0, 1 - var(df$Seasonal, na.rm = TRUE)/var(df$Value, na.rm = TRUE))
  seasonal_str <- max(0, 1 - var(df$Trend, na.rm = TRUE)/var(df$Value, na.rm = TRUE))
  
  # Perform stationarity tests
  ts_obj <- ts(df$Value, frequency = freq)
  
  adf_test <- tryCatch({
    res <- adf.test(ts_obj)
    list(p.value = res$p.value, method = res$method)
  }, error = function(e) {
    list(p.value = NA, method = "ADF (failed)", error = e$message)
  })
  
  kpss_test <- tryCatch({
    res <- kpss.test(ts_obj)
    list(p.value = res$p.value, method = res$method)
  }, error = function(e) {
    list(p.value = NA, method = "KPSS (failed)", error = e$message)
  })
  
  # Create plots
  plots <- list()
  
  # Main plot with components
  plots$trend <- ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Value), color = "gray50", alpha = 0.7) +
    geom_line(aes(y = Trend), color = "blue", linewidth = 1) +
    labs(title = paste("Trend Component -", ts_name),
         subtitle = sprintf("Trend strength: %.2f | Seasonality strength: %.2f", 
                            trend_str, seasonal_str),
         y = "Value") +
    theme_minimal()
  
  # Seasonal component plot
  plots$seasonal <- ggplot(df, aes(x = Date, y = Seasonal)) +
    geom_line(color = "darkgreen") +
    labs(title = "Seasonal Component", 
         subtitle = sprintf("ADF: p=%.3f | KPSS: p=%.3f",
                            adf_test$p.value, kpss_test$p.value),
         y = "Deviation") +
    theme_minimal()
  
  # Display plots if requested
  if (show_plots) {
    print(plots$trend)
    print(plots$seasonal)
  }
  
  # Prepare return object
  result <- list(
    data = df,
    trend_strength = trend_str,
    seasonality_strength = seasonal_str,
    stationarity_tests = list(
      ADF = adf_test,
      KPSS = kpss_test
    ),
    plots = plots,
    model = trend_model
  )
  
  # Console output
  cat("=== LOESS Decomposition Results ===\n")
  cat(sprintf("Time series: %s\n", ts_name))
  cat(sprintf("Observations: %d | Frequency: %d/year\n", nrow(df), freq))
  cat(sprintf("Date range: %s to %s\n", min(df$Date), max(df$Date)))
  cat(sprintf("LOESS span parameter: %.2f\n", span))
  
  cat("\n--- Component Strength ---\n")
  cat(sprintf("Trend strength: %.3f (1 = pure trend)\n", trend_str))
  cat(sprintf("Seasonality strength: %.3f (1 = pure seasonality)\n", seasonal_str))
  
  cat("\n--- Stationarity Tests ---\n")
  cat(sprintf("ADF test (%s): p-value = %.4f\n", adf_test$method, adf_test$p.value))
  cat(sprintf("KPSS test (%s): p-value = %.4f\n", kpss_test$method, kpss_test$p.value))
  
  invisible(result)
}


