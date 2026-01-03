#' Extended Plasticity Meta-Analysis
#'
#' Comprehensive meta-analysis of phenotypic plasticity studies.
#' This function synthesizes effect sizes from multiple studies, assesses
#' heterogeneity, publication bias, and moderating effects.
#'
#' @param study_data Data frame containing study results. Must include columns
#'   for effect sizes and their variances.
#' @param effect_size Character string specifying the column name for effect sizes
#' @param variance Character string specifying the column name for variances
#' @param study_labels Character string specifying the column name for study identifiers
#' @param method Meta-analysis method: "fixed" (default), "random", "bayesian", "multilevel"
#' @param p_value_threshold Threshold for statistical significance (default: 0.05)
#' @param moderator Optional character vector of moderator variable names
#' @param ... Additional arguments passed to meta-analysis functions
#'
#' @return A list containing:
#' \item{meta_results}{Overall meta-analysis results}
#' \item{individual_studies}{Study-level statistics}
#' \item{heterogeneity}{Heterogeneity analysis}
#' \item{publication_bias}{Publication bias assessment}
#' \item{moderator_analysis}{Results of moderator analysis (if requested)}
#' \item{forest_plot_data}{Data for creating forest plots}
#'
#' @export
#'
#' @examples
#' # Create simulated meta-analysis data
#' n_studies <- 15
#' meta_data <- data.frame(
#'   study = paste("Study", 1:n_studies),
#'   effect_size = rnorm(n_studies, mean = 0.6, sd = 0.3),
#'   variance = runif(n_studies, 0.05, 0.2),
#'   year = 2000 + 1:n_studies,
#'   taxon = sample(c("Insect", "Fungus", "Plant"), n_studies, replace = TRUE),
#'   trait_type = sample(c("Morphological", "Physiological", "Life History"), 
#'                      n_studies, replace = TRUE)
#' )
#' 
#' # Run meta-analysis
#' meta_result <- plasticity_meta_analysis_extended(
#'   study_data = meta_data,
#'   effect_size = "effect_size",
#'   variance = "variance",
#'   study_labels = "study",
#'   method = "random",
#'   moderator = c("year", "taxon", "trait_type")
#' )
#' 
#' print(meta_result)
#'
#' @importFrom stats weighted.mean pchisq pnorm qnorm cor
plasticity_meta_analysis_extended <- function(study_data,
                                              effect_size = "effect_size",
                                              variance = "variance",
                                              study_labels = "study",
                                              method = "random",
                                              p_value_threshold = 0.05,
                                              moderator = NULL,
                                              ...) {
  
  # Validate inputs
  if (!is.data.frame(study_data)) {
    stop("study_data must be a data frame")
  }
  
  if (!effect_size %in% names(study_data)) {
    stop("Column '", effect_size, "' not found in study_data")
  }
  
  if (!variance %in% names(study_data)) {
    stop("Column '", variance, "' not found in study_data")
  }
  
  if (!study_labels %in% names(study_data)) {
    study_data[[study_labels]] <- paste0("Study_", 1:nrow(study_data))
  }
  
  # Extract vectors
  yi <- study_data[[effect_size]]
  vi <- study_data[[variance]]
  study_names <- study_data[[study_labels]]
  
  # Check for valid variances
  if (any(vi <= 0, na.rm = TRUE)) {
    warning("Some variances are <= 0. Setting to minimum positive value/10")
    min_positive <- min(vi[vi > 0], na.rm = TRUE)
    vi[vi <= 0] <- min_positive / 10
  }
  
  # Remove studies with missing data
  complete_cases <- !is.na(yi) & !is.na(vi) & vi > 0
  yi_clean <- yi[complete_cases]
  vi_clean <- vi[complete_cases]
  study_names_clean <- study_names[complete_cases]
  study_data_clean <- study_data[complete_cases, ]
  
  n_studies <- length(yi_clean)
  
  if (n_studies < 2) {
    warning("Meta-analysis requires at least 2 studies. Returning basic statistics.")
    
    return(list(
      meta_results = list(
        n_studies = n_studies,
        mean_effect = if (n_studies == 1) yi_clean else NA,
        method = "insufficient_data",
        message = "Insufficient studies for meta-analysis"
      ),
      individual_studies = study_data_clean,
      heterogeneity = NULL,
      publication_bias = NULL,
      moderator_analysis = NULL,
      forest_plot_data = NULL
    ))
  }
  
  # CALCULATE BASIC STATISTICS
  se_clean <- sqrt(vi_clean)
  weights_fixed <- 1 / vi_clean
  weighted_mean_fixed <- sum(yi_clean * weights_fixed, na.rm = TRUE) / 
    sum(weights_fixed, na.rm = TRUE)
  var_fixed <- 1 / sum(weights_fixed, na.rm = TRUE)
  
  # HETEROGENEITY ANALYSIS (Q statistic)
  Q <- sum(weights_fixed * (yi_clean - weighted_mean_fixed)^2, na.rm = TRUE)
  df <- n_studies - 1
  p_heterogeneity <- if (df > 0) pchisq(Q, df, lower.tail = FALSE) else NA
  
  # I² statistic (percentage of heterogeneity)
  i2 <- if (Q > df) {
    ((Q - df) / Q) * 100
  } else {
    0
  }
  
  # BETWEEN-STUDY VARIANCE (tau²)
  C <- sum(weights_fixed, na.rm = TRUE) - 
    sum(weights_fixed^2, na.rm = TRUE) / sum(weights_fixed, na.rm = TRUE)
  
  if (Q <= df) {
    tau2 <- 0
  } else {
    tau2 <- (Q - df) / C
  }
  
  # META-ANALYSIS BASED ON SPECIFIED METHOD
  if (method == "fixed") {
    # Fixed effects model
    combined_mean <- weighted_mean_fixed
    combined_var <- var_fixed
    weights <- weights_fixed
    method_used <- "Fixed effects"
    
  } else if (method == "random") {
    # Random effects model (DerSimonian-Laird)
    weights_random <- 1 / (vi_clean + tau2)
    combined_mean <- sum(yi_clean * weights_random, na.rm = TRUE) / 
      sum(weights_random, na.rm = TRUE)
    combined_var <- 1 / sum(weights_random, na.rm = TRUE)
    weights <- weights_random
    method_used <- "Random effects (DerSimonian-Laird)"
    
  } else if (method == "bayesian") {
    # Simplified Bayesian approximation
    warning("Bayesian method simplified. Using random effects with empirical Bayes adjustment.")
    
    # Empirical Bayes adjustment
    tau2_eb <- max(0, tau2)  # Ensure non-negative
    weights_bayes <- 1 / (vi_clean + tau2_eb)
    combined_mean <- sum(yi_clean * weights_bayes, na.rm = TRUE) / 
      sum(weights_bayes, na.rm = TRUE)
    combined_var <- 1 / sum(weights_bayes, na.rm = TRUE)
    weights <- weights_bayes
    method_used <- "Bayesian (approximated)"
    
  } else if (method == "multilevel") {
    # Simplified multilevel model
    warning("Multilevel method simplified. Using random effects with adjusted weights.")
    
    # Add small study adjustment
    adj_factor <- 1 + (1 / sqrt(n_studies))
    weights_ml <- 1 / (vi_clean + tau2 * adj_factor)
    combined_mean <- sum(yi_clean * weights_ml, na.rm = TRUE) / 
      sum(weights_ml, na.rm = TRUE)
    combined_var <- 1 / sum(weights_ml, na.rm = TRUE)
    weights <- weights_ml
    method_used <- "Multilevel (approximated)"
    
  } else {
    stop("Method must be one of: 'fixed', 'random', 'bayesian', 'multilevel'")
  }
  
  # CONFIDENCE INTERVALS
  ci_lower <- combined_mean - 1.96 * sqrt(combined_var)
  ci_upper <- combined_mean + 1.96 * sqrt(combined_var)
  
  # Z-TEST FOR OVERALL EFFECT
  z_score <- combined_mean / sqrt(combined_var)
  p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # PUBLICATION BIAS ASSESSMENT
  # Funnel plot asymmetry (correlation between effect size and precision)
  precision <- 1 / se_clean
  funnel_cor <- cor(yi_clean, precision, use = "pairwise.complete.obs")
  
  # Fail-safe N (Rosenthal's approach)
  z_scores_individual <- yi_clean / se_clean
  p_values_individual <- 2 * (1 - pnorm(abs(z_scores_individual)))
  significant_studies <- sum(p_values_individual < p_value_threshold, na.rm = TRUE)
  
  fail_safe_n <- if (significant_studies > 0) {
    # Rosenthal's method
    sum(z_scores_individual) / qnorm(1 - p_value_threshold/2)
  } else {
    Inf
  }
  
  # MODERATOR ANALYSIS (if requested)
  moderator_results <- NULL
  if (!is.null(moderator) && length(moderator) > 0) {
    
    moderator_results <- list()
    
    for (mod_var in moderator) {
      if (mod_var %in% names(study_data_clean)) {
        
        moderator_values <- study_data_clean[[mod_var]]
        
        # For categorical moderators
        if (is.factor(moderator_values) || is.character(moderator_values)) {
          unique_cats <- unique(moderator_values)
          cat_results <- list()
          
          for (cat in unique_cats) {
            cat_indices <- moderator_values == cat
            if (sum(cat_indices) >= 2) {
              cat_mean <- weighted.mean(yi_clean[cat_indices], 
                                       1/vi_clean[cat_indices], 
                                       na.rm = TRUE)
              cat_results[[as.character(cat)]] <- list(
                n_studies = sum(cat_indices),
                mean_effect = cat_mean
              )
            }
          }
          
          moderator_results[[mod_var]] <- list(
            type = "categorical",
            categories = cat_results
          )
          
        } else if (is.numeric(moderator_values)) {
          # For continuous moderators (meta-regression)
          # Simplified correlation
          mod_cor <- cor(yi_clean, moderator_values, use = "pairwise.complete.obs")
          
          moderator_results[[mod_var]] <- list(
            type = "continuous",
            correlation_with_effect = mod_cor,
            n_observations = sum(!is.na(yi_clean) & !is.na(moderator_values))
          )
        }
      }
    }
  }
  
  # PREPARE FOREST PLOT DATA
  forest_data <- data.frame(
    study = study_names_clean,
    effect_size = yi_clean,
    variance = vi_clean,
    weight = weights / sum(weights, na.rm = TRUE) * 100,  # as percentage
    ci_lower = yi_clean - 1.96 * sqrt(vi_clean),
    ci_upper = yi_clean + 1.96 * sqrt(vi_clean),
    stringsAsFactors = FALSE
  )
  
  # PREPARE FINAL RESULTS
  results <- list(
    meta_results = list(
      combined_mean = combined_mean,
      combined_variance = combined_var,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      z_score = z_score,
      p_value = p_value,
      n_studies = n_studies,
      method = method_used,
      tau2 = tau2
    ),
    heterogeneity = list(
      Q = Q,
      df = df,
      p_heterogeneity = p_heterogeneity,
      i2 = i2,
      interpretation = if (i2 < 25) "Low heterogeneity" else
        if (i2 < 50) "Moderate heterogeneity" else
          if (i2 < 75) "Substantial heterogeneity" else
            "Considerable heterogeneity"
    ),
    publication_bias = list(
      funnel_asymmetry = funnel_cor,
      fail_safe_n = fail_safe_n,
      significant_studies = significant_studies,
      interpretation = if (abs(funnel_cor) > 0.3 && fail_safe_n < 5 * n_studies) {
        "Potential publication bias detected"
      } else {
        "No strong evidence of publication bias"
      }
    ),
    moderator_analysis = moderator_results,
    individual_studies = forest_data,
    forest_plot_data = list(
      study_names = study_names_clean,
      effect_sizes = yi_clean,
      standard_errors = se_clean,
      weights = weights,
      overall_effect = combined_mean,
      overall_ci = c(ci_lower, ci_upper)
    ),
    metadata = list(
      effect_size_column = effect_size,
      variance_column = variance,
      study_label_column = study_labels,
      method = method,
      p_value_threshold = p_value_threshold,
      timestamp = Sys.time()
    ),
    message = paste("Meta-analysis completed using", method_used, "model.",
                   "Combined effect size:", round(combined_mean, 3),
                   "[CI:", round(ci_lower, 3), "to", round(ci_upper, 3), "].",
                   "I² =", round(i2, 1), "%.")
  )
  
  class(results) <- c("plasticity_meta_analysis_extended", "list")
  return(results)
}

#' Print method for plasticity_meta_analysis_extended
#'
#' @param x An object of class plasticity_meta_analysis_extended
#' @param ... Additional arguments passed to print
#'
#' @export
print.plasticity_meta_analysis_extended <- function(x, ...) {
  cat("Extended Plasticity Meta-Analysis Results\n")
  cat("=========================================\n\n")
  
  # Overall results
  cat("OVERALL META-ANALYSIS:\n")
  cat("  Method:", x$meta_results$method, "\n")
  cat("  Number of studies:", x$meta_results$n_studies, "\n")
  cat("  Combined effect size:", round(x$meta_results$combined_mean, 3), "\n")
  cat("  95% Confidence Interval: [", 
      round(x$meta_results$ci_lower, 3), ", ",
      round(x$meta_results$ci_upper, 3), "]\n", sep = "")
  cat("  Z-score:", round(x$meta_results$z_score, 2), "\n")
  cat("  P-value:", format.pval(x$meta_results$p_value, digits = 3), "\n\n")
  
  # Heterogeneity
  cat("HETEROGENEITY ANALYSIS:\n")
  cat("  Q statistic:", round(x$heterogeneity$Q, 2), "\n")
  cat("  Degrees of freedom:", x$heterogeneity$df, "\n")
  cat("  P-value for heterogeneity:", 
      format.pval(x$heterogeneity$p_heterogeneity, digits = 3), "\n")
  cat("  I² (heterogeneity):", round(x$heterogeneity$i2, 1), "%\n")
  cat("  Interpretation:", x$heterogeneity$interpretation, "\n\n")
  
  # Publication bias
  cat("PUBLICATION BIAS ASSESSMENT:\n")
  cat("  Funnel plot asymmetry (correlation):", 
      round(x$publication_bias$funnel_asymmetry, 3), "\n")
  cat("  Fail-safe N:", if (is.infinite(x$publication_bias$fail_safe_n)) {
    "Infinity (no significant studies)"
  } else {
    round(x$publication_bias$fail_safe_n, 1)
  }, "\n")
  cat("  Interpretation:", x$publication_bias$interpretation, "\n\n")
  
  # Moderator analysis (if performed)
  if (!is.null(x$moderator_analysis) && length(x$moderator_analysis) > 0) {
    cat("MODERATOR ANALYSIS:\n")
    for (mod_name in names(x$moderator_analysis)) {
      mod_result <- x$moderator_analysis[[mod_name]]
      cat("  ", mod_name, " (", mod_result$type, "): ", sep = "")
      
      if (mod_result$type == "categorical") {
        cat(length(mod_result$categories), "categories\n")
      } else if (mod_result$type == "continuous") {
        cat("r = ", round(mod_result$correlation_with_effect, 3), "\n", sep = "")
      }
    }
    cat("\n")
  }
  
  cat("Studies included (first 5):\n")
  if (nrow(x$individual_studies) > 0) {
    print(head(x$individual_studies, 5))
  }
  
  cat("\n", x$message, "\n", sep = "")
  invisible(x)
}

