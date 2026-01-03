#' Fungi-Insect Interaction Dataset
#'
#' Simulated dataset of phenotypic plasticity in a fungus-insect host-pathogen system.
#' Contains data for 20 fungus strains across 10 insect populations under different
#' temperature and humidity conditions.
#'
#' @format A data frame with 600 rows and 8 variables:
#' \describe{
#'   \item{fungus_strain}{Fungus strain identifier (H1-H20)}
#'   \item{insect_population}{Insect population identifier (I1-I10)}
#'   \item{temperature}{Environmental temperature in °C (15-35)}
#'   \item{humidity}{Relative humidity in percent (30, 60, 90)}
#'   \item{fungus_growth}{Fungus growth rate}
#'   \item{virulence}{Fungus virulence (0-1 scale)}
#'   \item{insect_size}{Insect body size}
#'   \item{resistance}{Insect resistance to fungus (0-1 scale)}
#' }
#'
#' @source Simulated data for package examples and testing
"fungi_insect_data"
