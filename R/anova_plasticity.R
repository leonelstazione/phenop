#' G×E ANOVA for Phenotypic Plasticity Analysis
#'
#' Performs ANOVA to test genotype-environment interactions
#' and phenotypic plasticity significance.
#'
#' @param formula A formula specifying the model (e.g., trait ~ genotype * environment)
#' @param data A data frame containing the variables
#' @param return_type Type of output: "anova" (default), "model", or "summary"
#'
#' @return ANOVA table, model object, or summary depending on return_type
#' @export
#'
#' @examples
#' # Using the example data from calcular_ip
#' example_data <- data.frame(
#'   genotype = rep(LETTERS[1:5], each = 4),
#'   environment = rep(c("Low", "High"), each = 2, times = 5),
#'   trait_value = rnorm(20, 10, 2)
#' )
#'
#' anova_plasticity(trait_value ~ genotype * environment, example_data)
anova_plasticity <- function(formula, data, return_type = "anova") {

  # Ajustar el modelo lineal
  model <- stats::lm(formula, data = data)

  # Realizar ANOVA
  anova_result <- stats::anova(model)

  # Retornar según el tipo solicitado
  if (return_type == "anova") {
    return(anova_result)
  } else if (return_type == "model") {
    return(model)
  } else if (return_type == "summary") {
    return(summary(model))
  } else {
    stop("return_type must be 'anova', 'model', or 'summary'")
  }
}
