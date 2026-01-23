#' phenop: Multidimensional Analysis of Phenotypic Plasticity
#'
#' @description
#' The `phenop` package provides novel methods for analyzing phenotypic
#' plasticity across multiple traits and environmental variables, with
#' special focus on insect-fungus host-pathogen systems. It implements
#' original multidimensional plasticity indices and interaction analyses
#' not available in other packages.
#'
#' @details
#' Key features include:
#' - Multidimensional Plasticity Indices (MPI)
#' - Host-pathogen interaction analysis for insect-fungus systems
#' - Environmental optimization tools
#' - Advanced visualization of reaction norms
#' - Meta-analysis of plasticity studies
#' - Trade-off analysis between plasticities
#'
#' The package bridges the gap between ecological, evolutionary, and
#' phenotypic studies by offering comprehensive tools for plasticity
#' analysis in eco-evolutionary contexts.
#'
#' @section Main functions:
#' - \code{\link{multidim_plasticity}}: Calculate Multidimensional Plasticity Index
#' - \code{\link{host_pathogen_interaction}}: Analyze host-pathogen systems
#' - \code{\link{anova_plasticity}}: G×E ANOVA for plasticity
#' - \code{\link{plot_reaction_norm}}: Visualize reaction norms
#' - \code{\link{simulate_plasticity_data}}: Generate simulated data
#'
#' @section Datasets:
#' - \code{\link{pheno_parameters}}: Phenological parameters dataset
#' - \code{\link{pheno_spatial}}: Spatial phenology data
#' - \code{\link{pheno_time_series}}: Time series data
#' - \code{\link{fungi_insect_data}}: Host-pathogen simulation data
#'
#' @section Vignettes:
#' Run \code{vignette("phenop_vignette", package = "phenop")} for a
#' comprehensive tutorial.
#'
#' @author
#' **Maintainer**: Leonel Stazione <leonelstazione@hotmail.com>
#'
#' @references
#' For theoretical background, see:
#' - Phenotypic plasticity literature in evolutionary ecology
#' - Host-pathogen coevolution studies
#' - Multivariate statistical methods in ecology
#'
#' @seealso
#' Useful links:
#' - \url{https://github.com/leonelstazione/phenop}
#' - Report bugs at \url{https://github.com/leonelstazione/phenop/issues}
#'
#' @examples
#' \dontrun{
#' # Load the package
#' library(phenop)
#'
#' # Generate example data
#' data <- simulate_plasticity_data(n_genotypes = 5, n_environments = 3, n_traits = 2)
#'
#' # Calculate plasticity index
#' results <- multidim_plasticity(data, traits = c("trait1", "trait2"),
#'                                environment = "environment", genotype = "genotype")
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom stats as.formula
#' @importFrom utils data
## usethis namespace: end
NULL
