# Data documentation

#' Sea Temperature Data (2000-2017)
#'
#' Monthly sea temperature measurements at four depths on the Costa Brava, Spain. Data spans from 2000 to 2017.
#'
#' @format A data frame with 936 rows and 4 variables:
#' \describe{
#'   \item{Any}{Year (2000-2017)}
#'   \item{Mes}{Month in Catalan, as an ordered factor with 13 levels:
#'     "Gener", "Febrer", "Març", "Abril", "Maig", "Juny", "Juliol",
#'     "Agost", "Setembre", "Octubre", "Novembre", "Desembre", "Mitjana anual"}
#'   \item{Profunditat}{Depth in meters (0, -20, -50, -80)}
#'   \item{Temperatura}{Mean temperature in degrees Celsius}
#' }
#'
#' @details
#' Measurements taken at 1 nautical mile east of Medes Islands (Girona).
#' Coordinates: 42º03'N, 3º15'E.
#'
#' The dataset includes monthly temperatures and annual means for each depth.
#' Variables have descriptive labels attached using haven::labelled().
#'
#' @source Institut d'Estadística de Catalunya (IDESCAT)
#' \url{https://www.idescat.cat/pub/?id=aec&n=218&t=2000}
#'
#' @examples
#' data(sea.deep)
#' head(sea.deep)
#' summary(sea.deep)
#'
#' # View structure with labels
#' str(sea.deep)
#'
#' # Filter surface temperatures only
#' surface_temps <- subset(sea.deep, Profunditat == 0)
"sea.deep"


#' Baseline Sea Temperatures (1974-1999)
#'
#' Climatological mean sea temperatures by month and depth for the reference
#' period 1974-1999. Used as baseline for calculating temperature anomalies.
#'
#' @format A data frame with 48 rows and 3 variables:
#' \describe{
#'   \item{Mes_num}{Month number (1-12, where 1=January, 12=December)}
#'   \item{Profunditat}{Depth in meters (0, -20, -50, -80)}
#'   \item{baseline_temp}{Mean temperature in degrees Celsius for the 1974-1999 period}
#' }
#'
#' @details
#' This dataset represents a 30-year climatological average used as a reference
#' for computing temperature anomalies. Each combination of month and depth has
#' one baseline value representing the long-term mean for that period.
#'
#' @source Institut d'Estadística de Catalunya (IDESCAT)
#' \url{https://www.idescat.cat/pub/?id=aec&n=218&t=2000}
#'
#' @examples
#' data(baseline_1974_1999)
#' head(baseline_1974_1999)
#'
#' # View baseline for surface waters
#' subset(baseline_1974_1999, Profunditat == 0)
#'
#' # Compare baseline across depths for January
#' subset(baseline_1974_1999, Mes_num == 1)
"baseline_1974_1999"
