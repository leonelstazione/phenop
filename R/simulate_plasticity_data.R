#' Simulate Phenotypic Plasticity Data
#'
#' Generates realistic simulated data for plasticity studies with
#' customizable parameters for traits, environments, and plasticity patterns.
#' Useful for teaching and method validation.
#'
#' @param n_genotypes Number of genotypes/populations
#' @param n_environments Number of environmental conditions
#' @param n_traits Number of phenotypic traits
#' @param plasticity_patterns List of plasticity patterns: "gradient", "threshold", "optimal"
#' @param trait_correlations Correlation structure between traits
#'
#' @return Simulated plasticity dataset
#' @export
simulate_plasticity_data <- function(n_genotypes = 20, n_environments = 5, n_traits = 3,
                                     plasticity_patterns = list("gradient", "optimal", "neutral"),
                                     trait_correlations = 0.3) {

  # ORIGINAL simulation method with realistic plasticity patterns
  genotypes <- paste0("G", 1:n_genotypes)
  environments <- seq(10, 30, length.out = n_environments)
  traits <- paste0("trait_", 1:n_traits)

  # Generate base data structure
  sim_data <- expand.grid(
    genotype = genotypes,
    environment = environments,
    replicate = 1:3  # Technical replicates
  )

  # Simulate traits with different plasticity patterns (ORIGINAL approach)
  for (i in 1:n_traits) {
    pattern <- plasticity_patterns[[min(i, length(plasticity_patterns))]]
    sim_data[[traits[i]]] <- .simulate_trait_plasticity(
      sim_data$genotype, sim_data$environment, pattern, i
    )
  }

  # Add correlated errors between traits (ORIGINAL feature)
  sim_data <- .add_trait_correlations(sim_data, traits, trait_correlations)

  return(sim_data)
}

# Internal function: Original trait simulation with different plasticity patterns
.simulate_trait_plasticity <- function(genotypes, environments, pattern, trait_id) {
  n <- length(genotypes)
  base_value <- 10 + trait_id * 2  # Trait-specific baseline

  # ORIGINAL: Different plasticity response functions
  plasticity_response <- switch(pattern,
                                "gradient" = environments * 0.5,
                                "optimal" = -0.1 * (environments - 20)^2 + 10,
                                "threshold" = ifelse(environments > 20, 5, 0),
                                "neutral" = rep(0, length(environments)))

  # Genotype-specific plasticity (ORIGINAL component)
  genotype_effects <- as.numeric(factor(genotypes)) * 0.3

  trait_values <- base_value + plasticity_response + genotype_effects + rnorm(n, 0, 1)
  return(trait_values)
}

# Internal function: Add correlated errors between traits (ORIGINAL)
.add_trait_correlations <- function(data, traits, correlation) {
  n <- nrow(data)
  k <- length(traits)

  # Create correlation matrix
  cor_matrix <- matrix(correlation, nrow = k, ncol = k)
  diag(cor_matrix) <- 1

  # Generate correlated errors
  errors <- MASS::mvrnorm(n, mu = rep(0, k), Sigma = cor_matrix)

  # Add correlated errors to traits
  for (i in 1:k) {
    data[[traits[i]]] <- data[[traits[i]]] + errors[, i]
  }

  return(data)
}
