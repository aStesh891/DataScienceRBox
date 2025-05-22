#' @title LOESS Time Series Decomposition Analysis
#' @description 
#' Performs complete decomposition of time series data using LOESS smoothing.
#' @param df Data frame with 'Date' and 'Value' columns
#' @param ts_name Name for plot titles (default: "Time Series")
#' @param span LOESS smoothing parameter (0-1, default: 0.5)
#' @param show_plots Logical to display plots (default: TRUE)
#' @return List with decomposition results
#' @export
#' @importFrom ggplot2 ggplot geom_line labs theme_minimal
#' @importFrom stats loess predict var ts decompose
#' @importFrom tseries adf.test kpss.test
#' @importFrom plotly ggplotly

loess_decomposition_analysis <- function(df, ts_name = "Time Series",
                                         span = 0.5, show_plots = TRUE) {

  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed")
  }

  # Validate input structure
  if (!all(c("Date", "Value") %in% names(df))) {
    stop("Input dataframe must contain 'Date' and 'Value' columns")
  }

  # Convert and validate dates
  df$Date <- tryCatch(
    as.Date(df$Date),
    error = function(e) stop("Invalid date format in 'Date' column")
  )

  if (!is.numeric(df$Value)) {
    stop("'Value' column must contain numeric data")
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
  df$Detrended <- df$Value - df$Trend

  # Calculate seasonal component
  if (freq > 1) {
    seasonal_comp <- decompose(ts(df$Detrended, frequency = freq))$seasonal
    df$Seasonal <- as.numeric(seasonal_comp)[1:nrow(df)]
  } else {
    df$Seasonal <- 0
    warning("No meaningful seasonality detected - frequency too low")
  }

  df$Residual <- df$Value - (df$Trend + df$Seasonal)

  # Calculate component strengths using correct formula
  var_total <- var(df$Value, na.rm = TRUE)
  var_resid <- var(df$Residual, na.rm = TRUE)

  trend_str <- if (var_total > 0) {
    max(0, min(1, 1 - (var_resid/var(df$Trend + df$Residual, na.rm = TRUE)))
  } else 0

  seasonal_str <- if (var_total > 0) {
    max(0, min(1, 1 - (var_resid/var(df$Seasonal + df$Residual, na.rm = TRUE))))
  } else 0

  # Stationarity tests
  adf_test <- try(adf.test(na.omit(df$Value)), silent = TRUE)
  kpss_test <- try(kpss.test(na.omit(df$Value)), silent = TRUE)

  # Create plots
  plot_trend <- ggplot2::ggplot(df, ggplot2::aes(x = Date)) +
    ggplot2::geom_line(ggplot2::aes(y = Value), color = "gray50") +
    ggplot2::geom_line(ggplot2::aes(y = Trend), color = "blue", linewidth = 1) +
    ggplot2::labs(
      title = paste("Trend Component -", ts_name),
      subtitle = sprintf("Trend strength: %.2f | Seasonality strength: %.2f",
                        trend_str, seasonal_str),
      y = "Value"
    ) +
    ggplot2::theme_minimal()

  plot_seasonal <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Seasonal)) +
    ggplot2::geom_line(color = "darkgreen") +
    ggplot2::labs(
      title = "Seasonal Component",
      subtitle = sprintf("ADF p-value: %.3f | KPSS p-value: %.3f",
                        if (inherits(adf_test, "htest")) adf_test$p.value else NA,
                        if (inherits(kpss_test, "htest")) kpss_test$p.value else NA),
      y = "Deviation"
    ) +
    ggplot2::theme_minimal()

  # Display plots if requested
  if (show_plots) {
    if (requireNamespace("plotly", quietly = TRUE)) {
      print(plotly::ggplotly(plot_trend))
      print(plotly::ggplotly(plot_seasonal))
    } else {
      print(plot_trend)
      print(plot_seasonal)
    }
  }

  # Prepare detailed console output
  cat("\n=== TIME SERIES DECOMPOSITION RESULTS ===\n")
  cat(sprintf("Series: %s\n", ts_name))
  cat(sprintf("Period: %s to %s\n", min(df$Date), max(df$Date)))
  cat(sprintf("Observations: %d | Frequency: %d/year\n", nrow(df), freq))
  cat(sprintf("LOESS span: %.2f\n\n", span))

  cat("--- Component Strength ---\n")
  cat(sprintf("Trend strength:        %.3f (1 = pure trend)\n", trend_str))
  cat(sprintf("Seasonality strength:  %.3f (1 = pure seasonality)\n\n", seasonal_str))

  cat("--- Stationarity Tests ---\n")
  if (inherits(adf_test, "htest")) {
    cat(sprintf("ADF test (%s): p-value = %.4f\n",
               adf_test$method, adf_test$p.value))
  } else {
    cat("ADF test: Failed to compute\n")
  }

  if (inherits(kpss_test, "htest")) {
    cat(sprintf("KPSS test (%s): p-value = %.4f\n",
               kpss_test$method, kpss_test$p.value))
  } else {
    cat("KPSS test: Failed to compute\n")
  }

  # Return structured results
  invisible(
    structure(
      list(
        data = df,
        trend_strength = trend_str,
        seasonality_strength = seasonal_str,
        stationarity_tests = list(
          ADF = if (inherits(adf_test, "htest")) adf_test else NULL,
          KPSS = if (inherits(kpss_test, "htest")) kpss_test else NULL
        ),
        plots = list(
          trend = plot_trend,
          seasonal = plot_seasonal
        ),
        model = trend_model
      ),
      class = "loess_decomposition"
    )
  )
}
