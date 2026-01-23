#' Extended Host-Pathogen Interaction Analysis
#'
#' Comprehensive analysis of phenotypic plasticity in host-pathogen systems.
#'
#' @param host_data Data frame with host phenotypic data
#' @param pathogen_data Data frame with pathogen phenotypic data
#' @param host_traits Character vector of host trait names
#' @param pathogen_traits Character vector of pathogen trait names
#' @param interaction_metrics Metrics to calculate
#' @param environments Environmental variables
#' @param host_group Column name for host groups
#' @param pathogen_group Column name for pathogen groups
#' @param infection_data Optional infection data
#' @param ... Additional arguments
#' @export
host_pathogen_interaction_extended <- function(
    host_data, pathogen_data,
    host_traits, pathogen_traits,
    interaction_metrics = c("correlation"),
    environments,
    host_group = NULL, pathogen_group = NULL,
    infection_data = NULL, ...
) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  # ---------------------------
  # 1. Input validation
  # ---------------------------
  if (!is.data.frame(host_data)) stop("host_data must be a data frame")
  if (!is.data.frame(pathogen_data)) stop("pathogen_data must be a data frame")
  host_group <- host_group %||% "group"
  pathogen_group <- pathogen_group %||% "group"
  if (!host_group %in% names(host_data)) host_data[[host_group]] <- "Host"
  if (!pathogen_group %in% names(pathogen_data)) pathogen_data[[pathogen_group]] <- "Pathogen"
  host_required <- unique(c(host_traits, environments, host_group))
  pathogen_required <- unique(c(pathogen_traits, environments, pathogen_group))
  missing_host <- setdiff(host_required, names(host_data))
  missing_pathogen <- setdiff(pathogen_required, names(pathogen_data))
  if (length(missing_host) > 0)
    stop("Missing host columns: ", paste(missing_host, collapse = ", "))
  if (length(missing_pathogen) > 0)
    stop("Missing pathogen columns: ", paste(missing_pathogen, collapse = ", "))
  # ---------------------------
  # 2. Initialize results
  # ---------------------------
  results <- list(
    metadata = list(
      host_traits = host_traits,
      pathogen_traits = pathogen_traits,
      environments = environments,
      interaction_metrics = interaction_metrics,
      timestamp = Sys.time()
    ),
    interaction_correlations = list(),
    plasticity_matching = list(),
    arms_race_metrics = list(),
    infection_models = list(),
    visualization_data = list()
  )
  # ---------------------------
  # 3. Plasticity estimation
  # ---------------------------
  host_pl <- tryCatch(
    safe_multidim_plasticity(
      host_data,
      traits = host_traits,
      environments = environments,
      groups = host_group,
      na.action = "omit"
    ),
    error = function(e) NULL
  )
  path_pl <- tryCatch(
    safe_multidim_plasticity(
      pathogen_data,
      traits = pathogen_traits,
      environments = environments,
      groups = pathogen_group,
      na.action = "omit"
    ),
    error = function(e) NULL
  )
  # ---------------------------
  # 4. Correlations
  # ---------------------------
  if ("correlation" %in% interaction_metrics &&
      !is.null(host_pl) && !is.null(path_pl)) {
    host_df <- host_pl$individual_plasticity
    path_df <- path_pl$individual_plasticity
    correlations <- list()
    for (h in host_traits) {
      for (p in pathogen_traits) {
        if (h %in% names(host_df) && p %in% names(path_df)) {
          n <- min(nrow(host_df), nrow(path_df))
          if (n >= 3) {
            r <- suppressWarnings(
              cor(host_df[[h]][1:n], path_df[[p]][1:n],
                  use = "pairwise.complete.obs")
            )
            if (is.finite(r)) {
              correlations[[paste(h, p, sep = "_vs_")]] <- list(
                correlation = r,
                n_pairs = n
              )
            }
          }
        }
      }
    }
    if (length(correlations) == 0) {
      correlations[["no_valid_correlations"]] <- list(
        correlation = NA_real_,
        n_pairs = 0
      )
    }
    results$interaction_correlations <- correlations
  }
  # ---------------------------
  # 5. Plasticity matching
  # ---------------------------
  if ("plasticity_matching" %in% interaction_metrics &&
      !is.null(host_pl) && !is.null(path_pl)) {
    host_mpi <- as.numeric(host_pl$multidimensional_index$index)
    path_mpi <- as.numeric(path_pl$multidimensional_index$index)
    if (length(host_mpi) == 1 && length(path_mpi) == 1 &&
        is.finite(host_mpi) && is.finite(path_mpi)) {
      max_mpi <- max(host_mpi, path_mpi)
      results$plasticity_matching <- list(
        host_mpi = host_mpi,
        pathogen_mpi = path_mpi,
        absolute_difference = abs(host_mpi - path_mpi),
        relative_difference = if (max_mpi > 0)
          abs(host_mpi - path_mpi) / max_mpi else 0
      )
    } else {
      results$plasticity_matching <- list(
        host_mpi = NA_real_,
        pathogen_mpi = NA_real_
      )
    }
  }
  # ---------------------------
  # 6. Arms race metrics
  # ---------------------------
  if ("arms_race_index" %in% interaction_metrics) {
    virulence_traits <- grep(
      "virulence|toxicity|aggressiveness",
      pathogen_traits, ignore.case = TRUE, value = TRUE
    )
    resistance_traits <- grep(
      "resistance|defense|immunity",
      host_traits, ignore.case = TRUE, value = TRUE
    )
    results$arms_race_metrics <- list(
      virulence_traits_found = virulence_traits,
      resistance_traits_found = resistance_traits,
      arms_race_potential =
        length(virulence_traits) > 0 && length(resistance_traits) > 0
    )
  }
  # ---------------------------
  # 7. Infection model
  # ---------------------------
  if (!is.null(infection_data) &&
      "infection_success" %in% interaction_metrics &&
      all(c("infection_rate", "temperature") %in% names(infection_data))) {
    model <- try(lm(infection_rate ~ temperature, data = infection_data),
                 silent = TRUE)
    if (!inherits(model, "try-error")) {
      results$infection_models$temperature_model <- summary(model)
    }
  }
  # ---------------------------
  # 8. Visualization data
  # ---------------------------
  results$visualization_data$correlation_matrix <-
    if (length(results$interaction_correlations) > 0) TRUE else NULL
  class(results) <- c("host_pathogen_interaction_extended", "list")
  results
}

