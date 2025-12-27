#' Calculate Temperature Anomalies Relative to 1974-1999 Baseline
#'
#' Computes monthly temperature anomalies by subtracting the 1974-1999
#' climatological mean from each observation.
#'
#' @param sea_data A data frame with sea temperature observations.
#'   If NULL, uses the package's built-in sea.deep dataset.
#' @param baseline_data A data frame with baseline temperatures.
#'   If NULL, uses the package's built-in baseline_1974_1999 dataset.
#'
#' @return A tibble with all columns from sea_data plus: Mes_num,
#'   baseline_temp, anomaly (observed - baseline).
#'
#' @examples
#' # Use built-in data
#' anomalies <- calculate_anomalies()
#' head(anomalies)
#'
#' # Or provide custom data
#' data(sea.deep)
#' data(baseline_1974_1999)
#' anomalies <- calculate_anomalies(sea.deep, baseline_1974_1999)
#'
#' @importFrom dplyr mutate left_join %>%
#' @export
calculate_anomalies <- function(sea_data = NULL, baseline_data = NULL) {

  # Use built-in data if not provided
  if (is.null(sea_data)) {
    sea_data <- SeaTemperature::sea.deep
  }

  sea_data %>%
    dplyr::mutate(
      Mes_num = as.integer(Mes),  # Factor to number
      Profunditat = as.numeric(Profunditat)
  )
  if (is.null(baseline_data)) {
    baseline_data <- SeaTemperature::baseline_1974_1999
  }

  sea.anom <- sea_data %>%
    dplyr::mutate(Mes_num = as.integer(Mes),
                  Profunditat = as.numeric(Profunditat)) %>%
    dplyr::left_join(baseline_data, by = c("Mes_num", "Profunditat")) %>%
    dplyr::mutate(anomaly = Temperatura - baseline_temp)

  return(sea.anom)
}


#' Calculate Mean Anomalies Across All Years
#'
#' Computes the average temperature anomaly for each month and depth
#' across all years in the study period.
#'
#' @param sea_anom A data frame returned by calculate_anomalies().
#'   If NULL, calculates from built-in data.
#'
#' @return A tibble with columns: Mes_num, Profunditat, anomaly_mean.
#'
#' @examples
#' # Calculate from built-in data
#' mean_anom <- calculate_anomaly_mean()
#' head(mean_anom)
#'
#' # Or provide your own anomalies
#' anomalies <- calculate_anomalies()
#' mean_anom <- calculate_anomaly_mean(anomalies)
#'
#' @importFrom dplyr group_by summarise %>%
#' @export
calculate_anomaly_mean <- function(sea_anom = NULL) {

  # Calculate anomalies if not provided
  if (is.null(sea_anom)) {
    sea_anom <- calculate_anomalies()
  }

  sea.anom.mean <- sea_anom %>%
    dplyr::group_by(Mes_num, Profunditat) %>%
    dplyr::summarise(anomaly_mean = mean(anomaly, na.rm = TRUE), .groups = "drop")

  return(sea.anom.mean)
}
