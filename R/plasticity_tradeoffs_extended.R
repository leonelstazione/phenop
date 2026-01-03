#' Extended Plasticity Trade-off Analysis
#'
#' Comprehensive analysis of trade-offs between different phenotypic plasticity
#' indices. This function identifies and quantifies constraints and trade-offs
#' in multivariate plasticity space.
#'
#' @param plasticity_result Result object from multidim_plasticity() or safe_multidim_plasticity()
#' @param method Analysis method: "correlation" (default), "pca", "constraint", "regression"
#' @param threshold Correlation threshold for identifying trade-offs (default: -0.5)
#' @param p_value_threshold Significance threshold for statistical tests (default: 0.05)
#' @param visualization Logical, whether to prepare data for visualization (default: TRUE)
#' @param ... Additional arguments passed to analysis functions
#'
#' @return A list containing:
#' \item{tradeoff_matrix}{Matrix of correlations between plasticity indices}
#' \item{significant_tradeoffs}{Data frame of significant trade-off pairs}
#' \item{pca_results}{PCA results if method includes PCA}
#' \item{constraint_indices}{Constraint strength indices}
#' \item{visualization_data}{Data prepared for plotting}
#' \item{analysis_parameters}{Parameters used in the analysis}
#'
#' @export
#'
#' @examples
#' data(demo_data)
#' 
#' # First run plasticity analysis
#' insect_data <- demo_data %>%
#'   dplyr::select(insect_population, temperature, humidity,
#'                 insect_size, resistance) %>%
#'   dplyr::distinct()
#'   
#' result <- safe_multidim_plasticity(
#'   insect_data,
#'   traits = c("insect_size", "resistance"),
#'   environments = c("temperature", "humidity"),
#'   groups = "insect_population"
#' )
#' 
#' # Then analyze trade-offs
#' tradeoffs <- plasticity_tradeoffs_extended(result, method = "correlation")
#' print(tradeoffs)
#'
#' @importFrom stats cor cor.test prcomp
plasticity_tradeoffs_extended <- function(plasticity_result,
                                          method = "correlation",
                                          threshold = -0.5,
                                          p_value_threshold = 0.05,
                                          visualization = TRUE,
                                          ...) {
  
  # Validate input
  if (is.null(plasticity_result$individual_plasticity)) {
    stop("plasticity_result must contain individual_plasticity data")
  }
  
  data <- plasticity_result$individual_plasticity
  
  # Identify trait columns (numeric columns excluding group identifiers)
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  group_cols <- c("group", "individual", "groups", "genotype", 
                  "population", "strain", "id")
  trait_cols <- setdiff(numeric_cols, group_cols)
  
  if (length(trait_cols) < 2) {
    warning("At least 2 traits are required for trade-off analysis")
    return(list(
      tradeoff_matrix = NULL,
      significant_tradeoffs = data.frame(),
      pca_results = NULL,
      constraint_indices = NULL,
      visualization_data = NULL,
      analysis_parameters = list(
        method = method,
        threshold = threshold,
        p_value_threshold = p_value_threshold,
        n_traits = length(trait_cols),
        success = FALSE
      ),
      message = "Insufficient traits for trade-off analysis"
    ))
  }
  
  # Extract plasticity matrix
  plasticity_matrix <- as.matrix(data[, trait_cols, drop = FALSE])
  n_traits <- length(trait_cols)
  n_groups <- nrow(plasticity_matrix)
  
  # Initialize results
  tradeoff_matrix <- matrix(NA, nrow = n_traits, ncol = n_traits,
                           dimnames = list(trait_cols, trait_cols))
  p_value_matrix <- matrix(NA, nrow = n_traits, ncol = n_traits,
                          dimnames = list(trait_cols, trait_cols))
  significant_tradeoffs <- data.frame()
  
  # Correlation-based trade-off analysis
  if (method %in% c("correlation", "regression")) {
    
    for (i in 1:(n_traits - 1)) {
      for (j in (i + 1):n_traits) {
        trait_i <- trait_cols[i]
        trait_j <- trait_cols[j]
        
        # Calculate correlation
        cor_result <- try(cor.test(plasticity_matrix[, i], plasticity_matrix[, j],
                                  use = "pairwise.complete.obs"),
                         silent = TRUE)
        
        if (!inherits(cor_result, "try-error")) {
          correlation <- cor_result$estimate
          p_value <- cor_result$p.value
          
          tradeoff_matrix[i, j] <- correlation
          tradeoff_matrix[j, i] <- correlation
          p_value_matrix[i, j] <- p_value
          p_value_matrix[j, i] <- p_value
          
          # Check if this is a significant trade-off
          is_tradeoff <- correlation < threshold && p_value < p_value_threshold
          
          if (is_tradeoff) {
            significant_tradeoffs <- rbind(significant_tradeoffs,
                                          data.frame(
                                            trait1 = trait_i,
                                            trait2 = trait_j,
                                            correlation = correlation,
                                            p_value = p_value,
                                            strength = abs(correlation),
                                            type = ifelse(correlation < 0, "trade-off", "positive association"),
                                            stringsAsFactors = FALSE
                                          ))
          }
        }
      }
    }
    
    # Add diagonal
    diag(tradeoff_matrix) <- 1
    diag(p_value_matrix) <- 0
  }
  
  # PCA-based analysis
  pca_results <- NULL
  if (method %in% c("pca", "constraint")) {
    
    # Perform PCA on plasticity matrix
    pca_data <- scale(plasticity_matrix, center = TRUE, scale = TRUE)
    pca_results <- prcomp(pca_data, center = FALSE, scale. = FALSE)
    
    # Calculate variance explained
    variance_explained <- pca_results$sdev^2 / sum(pca_results$sdev^2)
    cum_variance <- cumsum(variance_explained)
    
    pca_results$variance_explained <- variance_explained
    pca_results$cumulative_variance <- cum_variance
  }
  
  # Constraint analysis (simplified)
  constraint_indices <- NULL
  if (method %in% c("constraint", "pca")) {
    
    if (!is.null(pca_results)) {
      # Constraint index based on variance explained by first PC
      constraint_strength <- variance_explained[1]
      
      # Effective dimensionality
      effective_dims <- sum(variance_explained > 1/n_traits)  # Kaiser criterion
      
      constraint_indices <- list(
        constraint_strength = constraint_strength,
        effective_dimensionality = effective_dims,
        integration_index = 1 - (effective_dims / n_traits),
        variance_first_pc = variance_explained[1],
        variance_second_pc = ifelse(length(variance_explained) > 1, 
                                   variance_explained[2], NA)
      )
    }
  }
  
  # Prepare visualization data
  visualization_data <- NULL
  if (visualization) {
    visualization_data <- list(
      plasticity_matrix = plasticity_matrix,
      trait_names = trait_cols,
      group_names = if (!is.null(data$group)) data$group else paste0("Group", 1:n_groups),
      correlation_matrix = tradeoff_matrix,
      p_value_matrix = p_value_matrix,
      pca_results = if (!is.null(pca_results)) {
        list(scores = pca_results$x[, 1:2],
             loadings = pca_results$rotation[, 1:2],
             variance = variance_explained[1:2])
      } else NULL
    )
  }
  
  # Sort significant trade-offs by strength
  if (nrow(significant_tradeoffs) > 0) {
    significant_tradeoffs <- significant_tradeoffs[order(
      significant_tradeoffs$strength, decreasing = TRUE), ]
  }
  
  # Prepare final result
  result <- list(
    tradeoff_matrix = tradeoff_matrix,
    p_value_matrix = p_value_matrix,
    significant_tradeoffs = significant_tradeoffs,
    pca_results = pca_results,
    constraint_indices = constraint_indices,
    visualization_data = visualization_data,
    analysis_parameters = list(
      method = method,
      threshold = threshold,
      p_value_threshold = p_value_threshold,
      n_traits = n_traits,
      n_groups = n_groups,
      success = TRUE
    ),
    message = paste("Trade-off analysis completed.",
                   nrow(significant_tradeoffs), 
                   "significant trade-offs found.")
  )
  
  class(result) <- c("plasticity_tradeoffs_extended", "list")
  return(result)
}

#' Print method for plasticity_tradeoffs_extended
#'
#' @param x An object of class plasticity_tradeoffs_extended
#' @param ... Additional arguments passed to print
#'
#' @export
print.plasticity_tradeoffs_extended <- function(x, ...) {
  cat("Extended Phenotypic Plasticity Trade-off Analysis\n")
  cat("==================================================\n\n")
  
  # Analysis parameters
  cat("ANALYSIS PARAMETERS:\n")
  cat("  Method:", x$analysis_parameters$method, "\n")
  cat("  Correlation threshold:", x$analysis_parameters$threshold, "\n")
  cat("  P-value threshold:", x$analysis_parameters$p_value_threshold, "\n")
  cat("  Number of traits:", x$analysis_parameters$n_traits, "\n")
  cat("  Number of groups:", x$analysis_parameters$n_groups, "\n\n")
  
  # Significant trade-offs
  cat("SIGNIFICANT TRADE-OFFS:", nrow(x$significant_tradeoffs), "found\n")
  if (nrow(x$significant_tradeoffs) > 0) {
    print(x$significant_tradeoffs)
  } else {
    cat("  No significant trade-offs found\n")
  }
  cat("\n")
  
  # Constraint indices (if available)
  if (!is.null(x$constraint_indices)) {
    cat("CONSTRAINT INDICES:\n")
    cat("  Constraint strength:", round(x$constraint_indices$constraint_strength, 3), "\n")
    cat("  Effective dimensionality:", x$constraint_indices$effective_dimensionality, "\n")
    cat("  Integration index:", round(x$constraint_indices$integration_index, 3), "\n\n")
  }
  
  cat(x$message, "\n")
  invisible(x)
}

