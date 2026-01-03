#' Demo Dataset for Plasticity Analysis
#'
#' Simulated dataset containing insect and fungus data
#' to demonstrate phenop package functionalities.
#'
#' @format A data.frame with 200 rows and 8 columns:
#' \describe{
#'   \item{insect_population}{Insect population (P1, P2, P3)}
#'   \item{fungus_strain}{Fungus strain (C1, C2, C3)}
#'   \item{temperature}{Temperature in Celsius (15-30)}
#'   \item{humidity}{Relative humidity in percentage (40-80)}
#'   \item{insect_size}{Insect size in mm (mean 10, sd 2)}
#'   \item{resistance}{Insect resistance to infection (mean 5, sd 1)}
#'   \item{fungus_growth}{Fungus growth rate (mean 8, sd 1.5)}
#'   \item{virulence}{Fungus virulence (mean 6, sd 1)}
#' }
#'
#' @source Simulated data for demonstration purposes
#' @examples
#' data(demo_data)
#' head(demo_data)
"demo_data"
