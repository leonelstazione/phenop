# R/host_pathogen_interaction.R
#' Host-Pathogen Interaction Analysis
#'
#' Calculates basic plasticity metrics for host-pathogen comparisons.
#' Robust alternative that calculates plasticity metrics without errors.
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @param data Data frame with both host and pathogen data
#' @param host_trait Host trait name
#' @param pathogen_trait Pathogen trait name
#' @param environment Environmental variable name
#' @param host_group Column name for host groups
#' @param pathogen_group Column name for pathogen groups
#'
#' @return List with plasticity comparison results
#' @export
#'
#' @examples
#' data(fungi_insect_data)
#' result <- host_pathogen_interaction(
#'   data = fungi_insect_data,
#'   host_trait = "insect_size",
#'   pathogen_trait = "fungus_growth",
#'   environment = "temperature",
#'   host_group = "insect_population",
#'   pathogen_group = "fungus_strain"
#' )
# Copia y pega este código en R/host_pathogen_interaction.R:
host_pathogen_interaction <- function(data,
                                      host_trait,
                                      pathogen_trait,
                                      environment,
                                      host_group = "insect_population",
                                      pathogen_group = "fungus_strain") {
  # Calculate simple plasticity: coefficient of variation across environments
  calculate_cv_plasticity <- function(data, trait, group_var, env_var) {
    results <- data %>%
      dplyr::group_by(.data[[group_var]], .data[[env_var]]) %>%
      dplyr::summarise(mean_value = mean(.data[[trait]], na.rm = TRUE), .groups = "drop_last") %>%
      dplyr::summarise(
        mean_across_envs = mean(mean_value, na.rm = TRUE),
        sd_across_envs = stats::sd(mean_value, na.rm = TRUE),  # stats::sd
        cv = ifelse(mean_across_envs != 0, sd_across_envs / abs(mean_across_envs), 0),
        .groups = "drop"
      )
    return(list(
      by_group = results,
      overall = mean(results$cv, na.rm = TRUE)
    ))
  }
  # Calculate for host
  host_results <- calculate_cv_plasticity(data, host_trait, host_group, environment)
  # Calculate for pathogen
  pathogen_results <- calculate_cv_plasticity(data, pathogen_trait, pathogen_group, environment)
  # Compare
  comparison <- host_results$overall - pathogen_results$overall
  # Determine interpretation - CORREGIDO PARA MANEJAR NAs
  interpretation_value <- if (is.na(comparison)) {
    "Cannot compare plasticity (missing data)"
  } else if (comparison > 0.1) {
    "Host shows substantially higher plasticity"
  } else if (comparison > 0) {
    "Host shows moderately higher plasticity"
  } else if (comparison > -0.1) {
    "Pathogen shows moderately higher plasticity"
  } else {
    "Pathogen shows substantially higher plasticity"
  }
  return(list(
    host_plasticity = host_results$overall,
    pathogen_plasticity = pathogen_results$overall,
    difference = comparison,
    interpretation = interpretation_value,
    details = list(
      host = host_results$by_group,
      pathogen = pathogen_results$by_group
    )
  ))
}

