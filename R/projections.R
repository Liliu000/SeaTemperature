
#' Calculate Annual Mean Anomalies by Year and Depth
#'
#' Aggregates monthly anomalies into annual mean values for each
#' year and depth combination.
#'
#' @param sea_anom A data frame returned by calculate_anomalies().
#'   If NULL, calculates from built-in data.
#'
#' @return A tibble with columns: Any (year), Profunditat (depth),
#'   mean_anom (annual mean anomaly).
#'
#' @examples
#' # Use built-in data
#' annual_means <- annual_anomaly_means()
#' head(annual_means)
#'
#' @importFrom dplyr group_by summarise %>%
#' @export
annual_anomaly_means <- function(sea_anom = NULL) {

  # Calculate anomalies if not provided
  if (is.null(sea_anom)) {
    sea_anom <- calculate_anomalies()
  }

  sea_anom %>%
    dplyr::group_by(Any, Profunditat) %>%
    dplyr::summarise(mean_anom = mean(anomaly, na.rm = TRUE), .groups = "drop")
}


#' Fit Linear Trend to Annual Mean Anomalies by Depth
#'
#' Fits a linear model to annual mean anomalies for each depth and
#' returns trend diagnostics (slope, R^2, p-value).
#'
#' @param sea_anom A data frame returned by calculate_anomalies().
#'   If NULL, calculates from built-in data.
#' @param depths Numeric vector. Depths to analyze. Default is c(0, -20, -50, -80).
#'
#' @return A tibble with columns: Profunditat, slope_C_per_year, r2, p_value_slope.
#'
#' @examples
#' # Analyze trends for all depths
#' trends <- fit_anom_linear_by_depth()
#' print(trends)
#'
#' @importFrom dplyr filter %>% bind_rows
#' @export
fit_anom_linear_by_depth <- function(sea_anom = NULL, depths = c(0, -20, -50, -80)) {

  # Calculate annual means if not provided
  if (is.null(sea_anom)) {
    sea_anom <- calculate_anomalies()
  }

  ann <- annual_anomaly_means(sea_anom) %>% dplyr::filter(Profunditat %in% depths)

  dplyr::bind_rows(lapply(depths, function(d) {
    dat_d <- ann %>% dplyr::filter(Profunditat == d)
    fit <- lm(mean_anom ~ Any, data = dat_d)
    sm <- summary(fit)

    tibble::tibble(
      Profunditat = d,
      slope_C_per_year = coef(fit)[2],
      r2 = sm$r.squared,
      p_value_slope = sm$coefficients["Any", "Pr(>|t|)"]
    )
  }))
}


#' Project Future Annual Mean Anomalies Using Linear Extrapolation
#'
#' Extrapolates annual mean anomalies to future years using linear regression
#' and provides prediction intervals.
#'
#' @param sea_anom A data frame returned by calculate_anomalies().
#'   If NULL, calculates from built-in data.
#' @param years_future Numeric vector. Future years to project. Default 2018:2030.
#' @param depths Numeric vector. Depths to project. Default c(0, -20, -50, -80).
#' @param interval Character. Type of interval ("prediction" or "confidence").
#' @param level Numeric. Confidence level for intervals (default 0.95).
#'
#' @return A tibble with columns: Any (future year), Profunditat, pred_anom,
#'   lo (lower bound), hi (upper bound).
#'
#' @examples
#' # Project to 2030
#' projections <- project_anom_linear(years_future = 2018:2030)
#' head(projections)
#'
#' @importFrom dplyr filter mutate %>% bind_rows
#' @export
project_anom_linear <- function(sea_anom = NULL,
                                years_future = 2018:2030,
                                depths = c(0, -20, -50, -80),
                                interval = "prediction",
                                level = 0.95) {

  # Calculate annual means if not provided
  if (is.null(sea_anom)) {
    sea_anom <- calculate_anomalies()
  }

  ann <- annual_anomaly_means(sea_anom) %>% dplyr::filter(Profunditat %in% depths)

  dplyr::bind_rows(lapply(depths, function(d) {
    dat_d <- ann %>% dplyr::filter(Profunditat == d)
    fit <- lm(mean_anom ~ Any, data = dat_d)

    future <- data.frame(Any = years_future)
    pr <- predict(fit, newdata = future, interval = interval, level = level)

    future %>%
      dplyr::mutate(
        Profunditat = d,
        pred_anom = pr[, "fit"],
        lo = pr[, "lwr"],
        hi = pr[, "upr"]
      )
  }))
}


#' Plot Observed and Projected Annual Mean Anomalies
#'
#' Creates a visualization showing historical annual mean anomalies
#' and linear projections into the future with uncertainty bands.
#'
#' @param sea_anom A data frame returned by calculate_anomalies().
#'   If NULL, calculates from built-in data.
#' @param proj_df A data frame returned by project_anom_linear().
#'   If NULL, projects to 2030.
#' @param depths Numeric vector. Depths to plot. Default c(0, -80).
#' @param split_year Numeric. Year marking the transition from observed
#'   to projected data. Default 2017.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Plot with default projections
#' plot_anom_projection()
#'
#' # Custom projection
#' projections <- project_anom_linear(years_future = 2018:2025)
#' plot_anom_projection(proj_df = projections, depths = c(0, -50))
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point geom_ribbon
#'   geom_vline labs theme_minimal
#' @importFrom dplyr filter %>%
#' @export
plot_anom_projection <- function(sea_anom = NULL, proj_df = NULL,
                                 depths = c(0, -80), split_year = 2017) {

  # Calculate anomalies if not provided
  if (is.null(sea_anom)) {
    sea_anom <- calculate_anomalies()
  }

  # Calculate projections if not provided
  if (is.null(proj_df)) {
    proj_df <- project_anom_linear(sea_anom)
  }

  ann <- annual_anomaly_means(sea_anom) %>% dplyr::filter(Profunditat %in% depths)
  proj_df <- proj_df %>% dplyr::filter(Profunditat %in% depths)

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(data = ann, ggplot2::aes(Any, mean_anom, color = factor(Profunditat))) +
    ggplot2::geom_point(data = ann, ggplot2::aes(Any, mean_anom, color = factor(Profunditat)), size = 1.6) +
    ggplot2::geom_ribbon(
      data = proj_df,
      ggplot2::aes(Any, ymin = lo, ymax = hi, fill = factor(Profunditat)),
      alpha = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = proj_df,
      ggplot2::aes(Any, pred_anom, color = factor(Profunditat)),
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(xintercept = split_year, linewidth = 0.3) +
    ggplot2::labs(
      title = "Exploratory linear projection of annual mean anomalies",
      x = "Year",
      y = "Annual mean anomaly (C)",
      color = "Depth (m)",
      fill = "Depth (m)"
    ) +
    ggplot2::theme_minimal()
}
