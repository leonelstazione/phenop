#' Plasticity Trade-off Analysis
#'
#' Identifies and quantifies trade-offs between plasticities of different traits
#' using original correlation-based and constraint-based methods.
#'
#' @param multidim_result Result from multidim_plasticity() function
#' @param method Trade-off detection method: "correlation", "pca", "constraint"
#' @param threshold Threshold for significant trade-offs
#'
#' @return List with trade-off metrics and visualizations
#' @export
plasticity_tradeoffs <- function(multidim_result, method = "correlation",
                                 threshold = -0.5) {

  # ORIGINAL: Multiple methods for trade-off detection
  tradeoff_results <- switch(method,
                             "correlation" = .correlation_tradeoffs(multidim_result, threshold),
                             "pca" = .pca_tradeoffs(multidim_result),
                             "constraint" = .constraint_tradeoffs(multidim_result),
                             stop("Unknown method. Use: correlation, pca, constraint"))

  return(tradeoff_results)
}

# Internal function: ORIGINAL correlation-based trade-offs
.correlation_tradeoffs <- function(result, threshold) {
  cor_matrix <- result$plasticity_correlations

  # ORIGINAL: Identify significant negative correlations as trade-offs
  tradeoff_pairs <- which(cor_matrix < threshold & lower.tri(cor_matrix),
                          arr.ind = TRUE)

  tradeoff_strengths <- cor_matrix[tradeoff_pairs]

  tradeoff_df <- data.frame(
    trait1 = rownames(cor_matrix)[tradeoff_pairs[, 1]],
    trait2 = colnames(cor_matrix)[tradeoff_pairs[, 2]],
    correlation = tradeoff_strengths,
    strength = abs(tradeoff_strengths)
  )

  # ORIGINAL: Trade-off network metrics
  network_density <- nrow(tradeoff_df) / choose(nrow(cor_matrix), 2)

  return(list(
    tradeoff_pairs = tradeoff_df,
    network_density = network_density,
    threshold_used = threshold,
    interpretation = "Negative correlations indicate trade-offs between trait plasticities"
  ))
}

# Internal function: ORIGINAL PCA-based trade-offs
.pca_tradeoffs <- function(result) {
  # ORIGINAL: Use PCA to identify trade-off dimensions
  plasticity_matrix <- t(sapply(result$individual_plasticity, function(trait) {
    sapply(trait, function(x) x$plasticity_norm)
  }))

  pca_result <- stats::prcomp(plasticity_matrix, scale. = TRUE)

  # ORIGINAL: Identify trade-off axes (PCs with high negative loadings)
  tradeoff_axes <- which(apply(pca_result$rotation < -0.3, 2, any))

  return(list(
    pca_result = pca_result,
    tradeoff_axes = tradeoff_axes,
    variance_explained = summary(pca_result)$importance[2, ],
    interpretation = "PC axes with opposing loadings indicate trade-off dimensions"
  ))
}

# Internal function: ORIGINAL constraint-based trade-offs
.constraint_tradeoffs <- function(result) {
  # ORIGINAL: Quantify constraints on total plasticity
  total_plasticity <- sapply(result$individual_plasticity, function(trait) {
    mean(sapply(trait, function(x) x$plasticity_norm))
  })

  mean_plasticity <- mean(total_plasticity)
  plasticity_variance <- var(total_plasticity)

  # ORIGINAL: Constraint index (lower = stronger constraints)
  constraint_index <- plasticity_variance / (mean_plasticity + 1e-6)

  return(list(
    total_plasticity = total_plasticity,
    constraint_index = constraint_index,
    interpretation = "Lower constraint index indicates stronger trade-offs"
  ))
}
