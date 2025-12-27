#' Calculate Month-to-Month Temperature Changes
#'
#' Computes the temperature difference between consecutive months
#' (Delta T = T(month+1) - T(month)) for each year and depth.
#'
#' @param sea_data A data frame with columns: Any (year), Mes (month as factor),
#'   Profunditat (depth), Temperatura (temperature). If NULL, uses the package's
#'   built-in sea.deep dataset.
#'
#' @return A tibble with columns: Any, Profunditat, Mes_num (month number 1-11),
#'   Mes, Temperatura, delta_next (temperature change to next month).
#'   December is excluded as there is no following month within the year.
#'
#' @examples
#' # Use built-in data
#' deltas <- calculate_month_deltas()
#' head(deltas)
#'
#' # Use custom data
#' data(sea.deep)
#' deltas <- calculate_month_deltas(sea.deep)
#'
#' @importFrom dplyr filter mutate group_by arrange ungroup lead %>%
#' @export
calculate_month_deltas <- function(sea_data = NULL) {

  # Use built-in data if not provided
  if (is.null(sea_data)) {
    sea_data <- SeaTemperature::sea.deep
  }

  # Clean and prepare data
  sea.deep.clean <- sea_data %>%
    dplyr::filter(Mes != "Mitjana anual") %>%
    dplyr::mutate(
      Any = as.integer(Any),
      Mes_num = as.integer(Mes)
    ) %>%
    dplyr::arrange(Any, Profunditat, Mes_num)

  # Calculate delta
  sea.delta <- sea.deep.clean %>%
    dplyr::group_by(Any, Profunditat) %>%
    dplyr::arrange(Mes_num, .by_group = TRUE) %>%
    dplyr::mutate(
      delta_next = dplyr::lead(Temperatura) - Temperatura
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Mes_num <= 11)

  return(sea.delta)
}


#' Calculate Mean Month-to-Month Changes Across All Years
#'
#' Computes the average temperature change between consecutive months
#' across all years in the dataset, for each month and depth.
#'
#' @param sea_delta A data frame returned by calculate_month_deltas().
#'   If NULL, calculates from built-in data.
#'
#' @return A tibble with columns: Mes_num (1-11), Profunditat, delta_mean
#'   (average delta across all years).
#'
#' @examples
#' # Calculate from built-in data
#' mean_deltas <- calculate_delta_mean()
#' head(mean_deltas)
#'
#' # Or provide your own deltas
#' deltas <- calculate_month_deltas()
#' mean_deltas <- calculate_delta_mean(deltas)
#'
#' @importFrom dplyr group_by summarise %>%
#' @export
calculate_delta_mean <- function(sea_delta = NULL) {

  # Calculate deltas if not provided
  if (is.null(sea_delta)) {
    sea_delta <- calculate_month_deltas()
  }

  sea.delta.mean <- sea_delta %>%
    dplyr::group_by(Mes_num, Profunditat) %>%
    dplyr::summarise(
      delta_mean = mean(delta_next, na.rm = TRUE),
      .groups = "drop"
    )

  return(sea.delta.mean)
}
