#' Calculate Multidimensional Plasticity Index (MPI)
#'
#' @param data A data frame containing the phenotypic data
#' @param traits Character vector of trait column names
#' @param environments Character string of environment column name
#' @param groups Character string of group/genotype column name
#' @param weights Optional numeric vector of weights for each trait
#' @param na.action How to handle NAs ("omit", "fail", "warning")
#' @param min_variation Minimum variation threshold (default 0.01)
#' @param ... Additional arguments passed to internal functions
#'
#' @return A list with multidimensional_index and individual_plasticity
#' @export
#'
#' @examples
#' data(demo_data)
#' result <- multidim_plasticity(
#'   data = demo_data,
#'   traits = c("insect_size", "resistance"),
#'   environments = "temperature",
#'   groups = "insect_population"
#' )
multidim_plasticity <- function(data, traits, environments, groups,
                                weights = NULL, na.action = "omit",
                                min_variation = 0.01, ...) {
  # Validate inputs
  .validate_multidim_inputs(data, traits, environments, groups, weights, na.action)
  # Handle NAs
  data <- .handle_na_action(data, c(traits, environments, groups), na.action)
  # Filter low variation groups
  data_filtered <- .filter_low_variation(data, traits, groups, min_variation)
  # Calculate plasticity metrics
  plasticity_metrics <- .calculate_plasticity_metrics(
    data_filtered, traits, environments, groups
  )
  # Apply weights if provided
  if (!is.null(weights)) {
    plasticity_metrics <- .apply_weights(plasticity_metrics, weights)
  }
  # Calculate multidimensional index
  multidimensional_index <- .calculate_multidimensional_index(plasticity_metrics)
  # Prepare output
  result <- list(
    multidimensional_index = multidimensional_index,
    individual_plasticity = plasticity_metrics,
    input_parameters = list(
      traits = traits,
      environments = environments,
      groups = groups,
      n_groups = length(unique(data_filtered[[groups]])),
      n_environments = length(unique(data_filtered[[environments]]))
    )
  )
  class(result) <- c("multidim_plasticity_result", class(result))
  return(result)
}
# Internal function to filter low variation groups
.filter_low_variation <- function(data, traits, groups, min_variation = 0.01) {
  # Filter rows with complete trait data
  data <- data %>%
    dplyr::filter(rowSums(dplyr::across(dplyr::all_of(traits))) > 0)
  # Calculate variation per group
  group_variation <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(traits),
        ~ stats::var(.x, na.rm = TRUE) >= min_variation
      ),
      .groups = "drop"
    )
  # Identify groups with sufficient variation in at least one trait
  valid_groups <- group_variation %>%
    dplyr::filter(rowSums(dplyr::across(dplyr::all_of(traits))) > 0) %>%
    dplyr::pull(!!dplyr::sym(groups))
  # Filter data to only valid groups
  data <- data %>%
    dplyr::filter(!!dplyr::sym(groups) %in% valid_groups)
  return(data)
}
# Internal function to calculate plasticity metrics
.calculate_plasticity_metrics <- function(data, traits, environments, groups) {
  group_env_means <- data %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(groups)),
      dplyr::across(dplyr::all_of(environments))
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(traits),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
  # Calculate plasticity as coefficient of variation across environments
  plasticity_df <- group_env_means %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(traits),
        ~ ifelse(
          mean(.x, na.rm = TRUE) != 0,
          stats::sd(.x, na.rm = TRUE) / abs(mean(.x, na.rm = TRUE)),
          0
        )
      ),
      .groups = "drop"
    )
  return(plasticity_df)
}
# Internal function to apply weights
.apply_weights <- function(plasticity_df, weights) {
  # Identify numeric columns (traits)
  numeric_cols <- sapply(plasticity_df, is.numeric)
  trait_cols <- names(plasticity_df)[numeric_cols]
  if (length(weights) != length(trait_cols)) {
    stop("Weights length must match number of traits")
  }
  for (i in seq_along(trait_cols)) {
    plasticity_df[[trait_cols[i]]] <- plasticity_df[[trait_cols[i]]] * weights[i]
  }
  return(plasticity_df)
}
# Internal function to calculate multidimensional index
.calculate_multidimensional_index <- function(plasticity_df) {
  # Identify numeric columns (traits)
  numeric_cols <- sapply(plasticity_df, is.numeric)
  trait_cols <- names(plasticity_df)[numeric_cols]
  if (length(trait_cols) == 0) {
    return(NA_real_)
  }
  # Calculate mean plasticity across traits for each group
  # Using rowMeans directly on numeric columns
  trait_matrix <- as.matrix(plasticity_df[, trait_cols, drop = FALSE])
  group_indices <- rowMeans(trait_matrix, na.rm = TRUE)
  # Overall multidimensional index is mean of group indices
  multidimensional_index <- mean(group_indices, na.rm = TRUE)
  return(multidimensional_index)
}
# Internal function to validate inputs
.validate_multidim_inputs <- function(data, traits, environments, groups,
                                      weights, na.action) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  missing_cols <- setdiff(c(traits, environments, groups), names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  if (!na.action %in% c("omit", "fail", "warning")) {
    stop("na.action must be one of: 'omit', 'fail', 'warning'")
  }
  if (!is.null(weights) && !is.numeric(weights)) {
    stop("weights must be numeric vector or NULL")
  }
}
# Internal function to handle NAs
.handle_na_action <- function(data, required_cols, na.action) {
  na_rows <- which(rowSums(is.na(data[, required_cols, drop = FALSE])) > 0)
  if (length(na_rows) > 0) {
    if (na.action == "fail") {
      stop("NAs found in required columns. Use na.action = 'omit' to remove them.")
    } else if (na.action == "warning") {
      warning("Removing ", length(na_rows), " rows with NAs")
      data <- data[-na_rows, , drop = FALSE]
    } else if (na.action == "omit") {
      data <- data[-na_rows, , drop = FALSE]
    }
  }
  return(data)
}

