#' Multidimensional Plasticity Index (MPI)
#'
#' Calculates an original index that integrates plasticity across multiple traits
#' considering their correlations and ecological importance.
#'
#' @param data Data frame with the data
#' @param traits Vector of traits to analyze
#' @param environments Environmental variables
#' @param groups Grouping variable(genotypes/populations)
#' @param weights Optional: ecological importance weights for each trait
#'
#' @return List with multidimensional plasticity metrics
#' @export
multidim_plasticity <- function(data, traits, environments, groups, weights = NULL) {

  # Validation
  if (!all(c(traits, environments, groups) %in% names(data))) {
    stop("Some variables not found in the dataset")
  }

  # 1. Calculate individual plasticity for each trait (ORIGINAL)
  individual_plasticity <- .calculate_individual_plasticity(
    data, traits, environments, groups
  )

  # 2. Correlation matrix between plasticities (NEW)
  plasticity_cor_matrix <- .plasticity_correlation_matrix(individual_plasticity)

  # 3. Multidimensional Plasticity Index (ORIGINAL CONTRIBUTION)
  mpi <- .calculate_mpi(individual_plasticity, plasticity_cor_matrix, weights)

  # 4. Phenotypic integration analysis (ORIGINAL)
  integration <- .analyze_integration(individual_plasticity, plasticity_cor_matrix)

  return(list(
    individual_plasticity = individual_plasticity,
    plasticity_correlations = plasticity_cor_matrix,
    multidimensional_index = mpi,
    phenotypic_integration = integration,
    method = "MPI (Multidimensional Plasticity Index)"
  ))
}

# Internal ORIGINAL function for individual plasticity
.calculate_individual_plasticity <- function(data, traits, environments, groups) {
  results <- list()

  for (trait in traits) {
    # ORIGINAL method: plasticity considering multiple environments simultaneously

    formula <- as.formula(paste(trait, "~", paste(environments, collapse = "+")))

    # Fit model for each group
    models <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
      dplyr::group_map(~ {
        model <- stats::lm(formula, data = .x)
        list(
          group = unique(.x[[groups]]),
          r_squared = summary(model)$r.squared,
          coefficients = stats::coef(model),
          plasticity_norm = stats::sd(stats::predict(model)) / mean(stats::predict(model))
        )
      })

    results[[trait]] <- models
  }

  return(results)
}

# Internal ORIGINAL function: Correlation matrix
.plasticity_correlation_matrix <- function(individual_plasticity) {
  traits <- names(individual_plasticity)
  n_traits <- length(traits)
  matrix <- matrix(NA, nrow = n_traits, ncol = n_traits)
  rownames(matrix) <- colnames(matrix) <- traits

  for (i in 1:n_traits) {
    for (j in 1:n_traits) {
      if (i == j) {
        matrix[i, j] <- 1
      } else {
        # Extract plasticity values for correlation
        plast_i <- sapply(individual_plasticity[[i]], function(x) x$plasticity_norm)
        plast_j <- sapply(individual_plasticity[[j]], function(x) x$plasticity_norm)
        matrix[i, j] <- stats::cor(plast_i, plast_j, use = "complete.obs")
      }
    }
  }

  return(matrix)
}

# Internal ORIGINAL function: MPI
.calculate_mpi <- function(individual_plasticity, cor_matrix, weights = NULL) {
  traits <- names(individual_plasticity)

  # If no weights, use equal weights
  if (is.null(weights)) {
    weights <- rep(1, length(traits))
  }

  # Average plasticity per trait
  plasticities <- sapply(individual_plasticity, function(x) {
    mean(sapply(x, function(y) y$plasticity_norm), na.rm = TRUE)
  })

  # Component 1: Weighted average plasticity
  avg_plasticity <- weighted.mean(plasticities, weights, na.rm = TRUE)

  # Component 2: Integration (considering correlations)
  # Only correlations between different traits
  cor_triangular <- cor_matrix[lower.tri(cor_matrix)]
  integration <- mean(abs(cor_triangular), na.rm = TRUE)

  # ORIGINAL MPI formula
  mpi <- avg_plasticity * (1 + integration)

  return(list(
    index = mpi,
    average_plasticity = avg_plasticity,
    integration = integration,
    interpretation = "MPI > 1 indicates high multidimensional plasticity"
  ))
}

# Internal ORIGINAL function: Integration analysis
.analyze_integration <- function(individual_plasticity, cor_matrix) {
  # Analysis of how plasticities integrate
  eigen_values <- eigen(cor_matrix)$values
  global_integration <- sum(eigen_values) / max(eigen_values)

  return(list(
    global_integration = global_integration,
    eigen_values = eigen_values,
    interpretation = "High values indicate greater phenotypic integration"
  ))
}
