#' Safe Multidimensional Plasticity Analysis
#'
#' A robust version of multidimensional plasticity analysis with error handling
#' and data validation. This function calculates actual plasticity indices
#' based on environmental variation in phenotypic traits.
#'
#' @param data A data frame containing phenotypic and environmental data
#' @param traits Character vector of trait names to analyze
#' @param environments Character vector of environmental variables
#' @param groups Character string specifying the grouping variable (e.g., genotype, population)
#' @param weights Optional numeric vector of weights for each trait (ecological importance)
#' @param na.action How to handle missing values: "omit" (default), "fail", "impute.mean"
#' @param ... Additional arguments passed to the plasticity calculation
#'
#' @return A list containing:
#' \item{individual_plasticity}{Data frame with plasticity indices for each group}
#' \item{group_plasticity}{Summary statistics by group}
#' \item{multidimensional_index}{Overall multidimensional plasticity index}
#' \item{safe_analysis}{List with analysis metadata and success status}
#' \item{message}{Informative message about the analysis}
#'
#' @export
#'
#' @examples
#' # Create example data with real variability
#' set.seed(123)
#' example_data <- data.frame(
#'   genotype = rep(paste0("G", 1:10), each = 4),
#'   environment = rep(c("Cold", "Cool", "Warm", "Hot"), 10),
#'   trait1 = rnorm(40, mean = 10, sd = 2),
#'   trait2 = rnorm(40, mean = 5, sd = 1),
#'   trait3 = rnorm(40, mean = 20, sd = 3)
#' )
#'
#' # Run analysis
#' result <- safe_multidim_plasticity(
#'   data = example_data,
#'   traits = c("trait1", "trait2", "trait3"),
#'   environments = "environment",
#'   groups = "genotype"
#' )
#'
#' print(result)
#'
#' @importFrom stats aggregate sd complete.cases
#' @importFrom dplyr group_by summarise across all_of
safe_multidim_plasticity <- function(data, traits, environments, groups,
                                     weights = NULL, na.action = "omit", ...) {
  # Start timing
  start_time <- Sys.time()

  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }

  if (!is.character(traits) || length(traits) == 0) {
    stop("traits must be a non-empty character vector")
  }

  if (!is.character(environments) || length(environments) == 0) {
    stop("environments must be a non-empty character vector")
  }

  if (!is.character(groups) || length(groups) != 1) {
    stop("groups must be a single character string")
  }

  # Check for required columns
  required_cols <- unique(c(traits, environments, groups))
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    error_msg <- paste("The following required columns are missing:",
                       paste(missing_cols, collapse = ", "))
    return(list(
      safe_analysis = list(
        success = FALSE,
        error_message = error_msg,
        traits_analyzed = traits,
        environments = environments,
        groups = groups,
        timestamp = Sys.time()
      ),
      message = paste("Analysis failed:", error_msg)
    ))
  }

  # Handle missing values
  if (na.action == "omit") {
    complete_cases <- complete.cases(data[, required_cols])
    data_clean <- data[complete_cases, ]
    n_omitted <- sum(!complete_cases)

    if (n_omitted > 0) {
      message(paste("Omitted", n_omitted, "rows with missing values"))
    }

    if (nrow(data_clean) < 2) {
      error_msg <- "Insufficient data after removing missing values"
      return(list(
        safe_analysis = list(
          success = FALSE,
          error_message = error_msg,
          traits_analyzed = traits,
          environments = environments,
          groups = groups,
          timestamp = Sys.time()
        ),
        message = paste("Analysis failed:", error_msg)
      ))
    }
  } else if (na.action == "fail") {
    if (any(!complete.cases(data[, required_cols]))) {
      error_msg <- "Missing values found. Set na.action = \"omit\" to remove them."
      return(list(
        safe_analysis = list(
          success = FALSE,
          error_message = error_msg,
          traits_analyzed = traits,
          environments = environments,
          groups = groups,
          timestamp = Sys.time()
        ),
        message = paste("Analysis failed:", error_msg)
      ))
    }
    data_clean <- data
  } else if (na.action == "impute.mean") {
    # Simple mean imputation for numeric columns
    data_clean <- data
    for (col in required_cols) {
      if (is.numeric(data_clean[[col]])) {
        mean_val <- mean(data_clean[[col]], na.rm = TRUE)
        data_clean[[col]][is.na(data_clean[[col]])] <- mean_val
      }
    }
  } else {
    stop("na.action must be one of: 'omit', 'fail', 'impute.mean'")
  }

  # Get unique groups
  unique_groups <- unique(data_clean[[groups]])
  n_groups <- length(unique_groups)

  if (n_groups < 2) {
    error_msg <- "At least 2 groups are required for plasticity analysis"
    return(list(
      safe_analysis = list(
        success = FALSE,
        error_message = error_msg,
        traits_analyzed = traits,
        environments = environments,
        groups = groups,
        timestamp = Sys.time()
      ),
      message = paste("Analysis failed:", error_msg)
    ))
  }


  # Matriz para almacenar plasticidad por grupo y rasgo
  plasticity_matrix <- matrix(
    NA,
    nrow = n_groups,
    ncol = length(traits),
    dimnames = list(unique_groups, traits)
  )

  # Calcular plasticidad para cada grupo
  for (i in seq_along(unique_groups)) {
    group <- unique_groups[i]
    group_data <- data_clean[data_clean[[groups]] == group, ]

    # Para cada rasgo
    for (j in seq_along(traits)) {
      trait <- traits[j]

      # Agrupar por ambiente y calcular media del rasgo
      env_means <- aggregate(
        as.formula(paste(trait, "~", environments)),
        data = group_data,
        FUN = mean,
        na.rm = TRUE
      )

      names(env_means) <- c("environment", "mean_value")

      # Calcular plasticidad como Coeficiente de Variación (CV) entre ambientes
      if (nrow(env_means) > 1 && sd(env_means$mean_value, na.rm = TRUE) > 0) {
        cv_value <- sd(env_means$mean_value, na.rm = TRUE) /
          mean(env_means$mean_value, na.rm = TRUE)
        plasticity_matrix[i, j] <- cv_value
      } else {
        plasticity_matrix[i, j] <- 0
      }
    }
  }

  # Calcular pesos (si no se proporcionan, usar pesos iguales)
  if (!is.null(weights) && length(weights) == length(traits)) {
    weights <- weights / sum(weights)  # Normalizar
  } else {
    weights <- rep(1 / length(traits), length(traits))
  }

  # Calcular MPI por grupo (promedio ponderado)
  mpi_by_group <- numeric(n_groups)
  for (i in 1:n_groups) {
    mpi_by_group[i] <- sum(plasticity_matrix[i, ] * weights, na.rm = TRUE)
  }

  # MPI general (promedio de MPI por grupo)
  overall_mpi <- mean(mpi_by_group, na.rm = TRUE)

  # Individual plasticity data frame
  individual_plasticity <- data.frame(
    group = rep(unique_groups, each = length(traits)),
    trait = rep(traits, times = n_groups),
    plasticity = as.vector(t(plasticity_matrix)),
    stringsAsFactors = FALSE
  )

  # Group plasticity summary
  group_plasticity <- data.frame(
    group = unique_groups,
    mean_plasticity = mpi_by_group,
    sd_plasticity = apply(plasticity_matrix, 1, sd, na.rm = TRUE),
    n_traits = length(traits),
    stringsAsFactors = FALSE
  )


  # Calculate computation time
  end_time <- Sys.time()
  computation_time <- difftime(end_time, start_time, units = "secs")

  # Prepare final result
  result <- list(
    individual_plasticity = individual_plasticity,
    group_plasticity = group_plasticity,
    multidimensional_index = list(
      index = overall_mpi,
      method = ifelse(is.null(weights), "unweighted mean", "weighted mean"),
      components = colMeans(plasticity_matrix, na.rm = TRUE),
      weights = weights,
      n_traits = length(traits),
      n_groups = n_groups,
      range = range(mpi_by_group, na.rm = TRUE)
    ),
    plasticity_matrix = plasticity_matrix,
    safe_analysis = list(
      success = TRUE,
      error_message = NA,
      traits_analyzed = traits,
      environments = environments,
      groups = groups,
      n_observations = nrow(data_clean),
      n_groups = n_groups,
      na.action = na.action,
      computation_time = as.numeric(computation_time),
      timestamp = end_time
    ),
    message = paste(
      "Safe multidimensional plasticity analysis completed successfully.",
      "Analyzed", length(traits), "traits across", n_groups, "groups.",
      "MPI range:", round(min(mpi_by_group, na.rm = TRUE), 3), "-",
      round(max(mpi_by_group, na.rm = TRUE), 3)
    )
  )

  class(result) <- c("safe_multidim_result", "list")
  return(result)
}

#' Print method for safe_multidim_result
#'
#' @param x An object of class safe_multidim_result
#' @param ... Additional arguments passed to print
#'
#' @export
print.safe_multidim_result <- function(x, ...) {
  cat("Safe Multidimensional Plasticity Analysis Results\n")
  cat("==================================================\n\n")

  # Analysis summary
  cat("ANALYSIS SUMMARY:\n")
  cat("  Status:", ifelse(x$safe_analysis$success, "SUCCESS", "FAILED"), "\n")

  if (!x$safe_analysis$success) {
    cat("  Error:", x$safe_analysis$error_message, "\n\n")
    return(invisible(x))
  }

  cat("  Traits analyzed:", paste(x$safe_analysis$traits_analyzed, collapse = ", "), "\n")
  cat("  Environments:", paste(x$safe_analysis$environments, collapse = ", "), "\n")
  cat("  Groups variable:", x$safe_analysis$groups, "\n")
  cat("  Number of groups:", x$safe_analysis$n_groups, "\n")
  cat("  Number of observations:", x$safe_analysis$n_observations, "\n")
  cat("  Computation time:", round(x$safe_analysis$computation_time, 2), "seconds\n\n")

  # Multidimensional Plasticity Index (MPI)
  cat("MULTIDIMENSIONAL PLASTICITY INDEX (MPI):\n")
  cat("  MPI value:", round(x$multidimensional_index$index, 4), "\n")
  cat("  Method:", x$multidimensional_index$method, "\n")
  cat("  Range across groups:",
      paste(round(x$multidimensional_index$range, 4), collapse = " to "), "\n")

  # Component contributions
  if (!is.null(x$multidimensional_index$components)) {
    cat("  Trait contributions:\n")
    for (trait in names(x$multidimensional_index$components)) {
      cat("    -", trait, ":", round(x$multidimensional_index$components[trait], 4), "\n")
    }
  }
  cat("\n")

  # Group plasticity summary
  cat("GROUP PLASTICITY (first 5 groups):\n")
  if (nrow(x$group_plasticity) > 0) {
    print(utils::head(x$group_plasticity, 5))
  }

  cat("\n", x$message, "\n", sep = "")
  invisible(x)
}
