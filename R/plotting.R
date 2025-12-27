
#' Plot Month-to-Month Temperature Changes by Year
#'
#' Creates a faceted line plot showing temperature changes between
#' consecutive months for each year and depth.
#'
#' @param data_delta A data frame returned by calculate_month_deltas().
#'   If NULL, calculates from built-in data.
#' @param years Numeric vector. Years to include. Default NULL includes all.
#' @param depths Numeric vector. Depths to include. Default NULL includes all.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Plot all years and depths
#' plot_delta_by_year()
#'
#' # Plot specific years and depths
#' plot_delta_by_year(years = c(2000, 2010, 2017), depths = c(0, -80))
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point facet_wrap
#'   scale_x_continuous labs theme_minimal
#' @importFrom dplyr filter %>%
#' @export
plot_delta_by_year <- function(data_delta = NULL, years = NULL, depths = NULL) {

  # Calculate deltas if not provided
  if (is.null(data_delta)) {
    data_delta <- calculate_month_deltas()
  }

  pdat <- data_delta
  if (!is.null(years)) pdat <- pdat %>% dplyr::filter(Any %in% years)
  if (!is.null(depths)) pdat <- pdat %>% dplyr::filter(Profunditat %in% depths)

  ggplot2::ggplot(pdat, ggplot2::aes(x = Mes_num, y = delta_next,
                                     color = factor(Profunditat),
                                     group = Profunditat)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~ Any, ncol = 3) +
    ggplot2::scale_x_continuous(breaks = 1:11, labels = month.abb[1:11]) +
    ggplot2::labs(
      title = "Temperature change between one month and the next (per year)",
      x = "Month (start of change)",
      y = "Delta T = T(next month) - T(current month) (C)",
      color = "Depth (m)"
    ) +
    ggplot2::theme_minimal()
}


#' Plot Mean Month-to-Month Changes Across All Years
#'
#' Creates a line plot showing the average temperature change between
#' consecutive months, averaged across all years.
#'
#' @param data_delta_mean A data frame returned by calculate_delta_mean().
#'   If NULL, calculates from built-in data.
#' @param depths Numeric vector. Depths to include. Default NULL includes all.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Plot all depths
#' plot_delta_mean()
#'
#' # Plot specific depths
#' plot_delta_mean(depths = c(0, -80))
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point
#'   scale_x_continuous labs theme_minimal
#' @importFrom dplyr filter %>%
#' @export
plot_delta_mean <- function(data_delta_mean = NULL, depths = NULL) {

  # Calculate mean deltas if not provided
  if (is.null(data_delta_mean)) {
    data_delta_mean <- calculate_delta_mean()
  }

  pdat <- data_delta_mean
  if (!is.null(depths)) pdat <- pdat %>% dplyr::filter(Profunditat %in% depths)

  ggplot2::ggplot(pdat, ggplot2::aes(x = Mes_num, y = delta_mean,
                                     color = factor(Profunditat),
                                     group = Profunditat)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(breaks = 1:11, labels = month.abb[1:11]) +
    ggplot2::labs(
      title = "Average (all years) of month-to-month change",
      x = "Month (start of change)",
      y = "Average Delta T (C)",
      color = "Depth (m)"
    ) +
    ggplot2::theme_minimal()
}


#' Plot Temperature Anomalies by Year
#'
#' Creates a faceted line plot showing monthly temperature anomalies
#' relative to the 1974-1999 baseline for each year.
#'
#' @param data A data frame returned by calculate_anomalies().
#'   If NULL, calculates from built-in data.
#' @param years Numeric vector. Years to include. Default NULL includes all.
#' @param depths Numeric vector. Depths to include. Default NULL includes all.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Plot all years and depths
#' plot_anom_by_year()
#'
#' # Plot specific years and depths
#' plot_anom_by_year(years = c(2000, 2010, 2017), depths = c(0, -80))
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point facet_wrap
#'   scale_x_continuous labs theme_minimal
#' @importFrom dplyr filter %>%
#' @export
plot_anom_by_year <- function(data = NULL, years = NULL, depths = NULL) {

  # Calculate anomalies if not provided
  if (is.null(data)) {
    data <- calculate_anomalies()
  }

  labels_en <- month.abb

  p <- data
  if (!is.null(years))  p <- p %>% dplyr::filter(Any %in% years)
  if (!is.null(depths)) p <- p %>% dplyr::filter(Profunditat %in% depths)

  ggplot2::ggplot(p, ggplot2::aes(x = Mes_num, y = anomaly,
                                  color = factor(Profunditat),
                                  group = Profunditat)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~ Any, ncol = 3) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = labels_en) +
    ggplot2::labs(
      title = "Monthly anomaly vs 1974-1999 (by year)",
      x = "Month",
      y = "Anomaly = T(year, month) - mean(1974-1999) (C)",
      color = "Depth (m)"
    ) +
    ggplot2::theme_minimal()
}


#' Plot Mean Temperature Anomalies Across All Years
#'
#' Creates a line plot showing the average monthly temperature anomaly
#' across all years in the study period.
#'
#' @param data_mean A data frame returned by calculate_anomaly_mean().
#'   If NULL, calculates from built-in data.
#' @param depths Numeric vector. Depths to include. Default NULL includes all.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Plot all depths
#' plot_anom_mean()
#'
#' # Plot specific depths
#' plot_anom_mean(depths = c(0, -80))
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point
#'   scale_x_continuous labs theme_minimal
#' @importFrom dplyr filter %>%
#' @export
plot_anom_mean <- function(data_mean = NULL, depths = NULL) {

  # Calculate mean anomalies if not provided
  if (is.null(data_mean)) {
    data_mean <- calculate_anomaly_mean()
  }

  labels_en <- month.abb

  p <- data_mean
  if (!is.null(depths)) p <- p %>% dplyr::filter(Profunditat %in% depths)

  ggplot2::ggplot(p, ggplot2::aes(x = Mes_num, y = anomaly_mean,
                                  color = factor(Profunditat),
                                  group = Profunditat)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = labels_en) +
    ggplot2::labs(
      title = "Mean anomaly (2000-2017) vs 1974-1999",
      x = "Month",
      y = "Mean anomaly (C)",
      color = "Depth (m)"
    ) +
    ggplot2::theme_minimal()
}
