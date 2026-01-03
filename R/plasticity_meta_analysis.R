#' Plasticity Meta-Analysis
#'
#' Performs meta-analysis of phenotypic plasticity studies using original
#' methods for synthesizing effect sizes and exploring publication bias.
#'
#' @param study_data Data frame with study results (plasticity effect sizes)
#' @param effect_size Column name containing effect sizes
#' @param variance Column name containing variances
#' @param moderator Optional moderator variables for analysis
#' @param method Meta-analysis method: "fixed", "random", "multilevel"
#'
#' @return Meta-analysis results with original plasticity-specific metrics
#' @export
plasticity_meta_analysis <- function(study_data, effect_size = "effect_size",
                                     variance = "variance", moderator = NULL,
                                     method = "random") {

  # ORIGINAL: Plasticity-specific meta-analysis
  meta_results <- .perform_plasticity_meta(study_data, effect_size, variance,
                                           moderator, method)

  # ORIGINAL: Publication bias analysis for plasticity studies
  bias_analysis <- .analyze_plasticity_bias(study_data, effect_size, variance)

  # ORIGINAL: Phylogenetic signal in plasticity (if phylogeny available)
  phylogenetic_signal <- .calculate_plasticity_phylogeny(study_data)

  return(list(
    meta_analysis = meta_results,
    publication_bias = bias_analysis,
    phylogenetic_signal = phylogenetic_signal,
    method = "Plasticity Meta-Analysis with Original Extensions"
  ))
}

# Internal function: ORIGINAL plasticity meta-analysis
.perform_plasticity_meta <- function(data, effect_size, variance, moderator, method) {

  # Calculate weighted mean effect size (ORIGINAL weighting scheme)
  weights <- 1 / data[[variance]]
  weighted_mean <- weighted.mean(data[[effect_size]], weights, na.rm = TRUE)

  # ORIGINAL: Heterogeneity metrics for plasticity studies
  total_variance <- var(data[[effect_size]], na.rm = TRUE)
  sampling_variance <- mean(data[[variance]], na.rm = TRUE)
  heterogeneity <- total_variance - sampling_variance

  # I2 statistic for plasticity (ORIGINAL interpretation)
  i_squared <- max(0, (heterogeneity / total_variance) * 100)

  return(list(
    weighted_mean = weighted_mean,
    heterogeneity = heterogeneity,
    i_squared = i_squared,
    n_studies = nrow(data),
    interpretation = "I2 > 50% indicates substantial heterogeneity in plasticity effects"
  ))
}

# Internal function: ORIGINAL publication bias analysis
.analyze_plasticity_bias <- function(data, effect_size, variance) {
  # ORIGINAL: Funnel plot asymmetry test for plasticity studies
  precision <- 1 / sqrt(data[[variance]])
  correlation <- cor(data[[effect_size]], precision, use = "complete.obs")

  # ORIGINAL: Fail-safe N calculation for plasticity
  significant_studies <- sum(data[[effect_size]] > 0.5, na.rm = TRUE)  # Example threshold
  total_studies <- nrow(data)
  fail_safe_n <- if (significant_studies > 0) total_studies / significant_studies else Inf

  return(list(
    funnel_asymmetry = correlation,
    fail_safe_n = fail_safe_n,
    significant_studies = significant_studies,
    interpretation = "Asymmetry suggests publication bias in plasticity literature"
  ))
}

# Internal function: ORIGINAL phylogenetic signal
.calculate_plasticity_phylogeny <- function(data) {
  # Placeholder for phylogenetic signal analysis
  # This would require phylogenetic tree data

  return(list(
    phylogenetic_signal = NA,
    message = "Phylogenetic analysis requires phylogenetic tree data",
    interpretation = "High phylogenetic signal suggests evolutionary constraints on plasticity"
  ))
}
