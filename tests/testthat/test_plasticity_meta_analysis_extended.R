test_that("plasticity_meta_analysis_extended works with simulated data", {

  # Simulate meta-analysis data
  set.seed(123)
  n_studies <- 10
  meta_data <- data.frame(
    study = paste("Study", 1:n_studies),
    effect_size = rnorm(n_studies, mean = 0.5, sd = 0.2),
    variance = runif(n_studies, 0.05, 0.2),
    year = 2000 + 1:n_studies,
    taxon = sample(c("Insect", "Plant", "Fungus"), n_studies, replace = TRUE),
    trait_type = sample(c("Morphological", "Physiological"), n_studies, replace = TRUE)
  )

  # Run the extended meta-analysis
  result <- plasticity_meta_analysis_extended(
    study_data = meta_data,
    effect_size = "effect_size",
    variance = "variance",
    study_labels = "study",
    method = "random",
    moderator = c("year", "taxon", "trait_type")
  )

  # Check class
  expect_s3_class(result, "plasticity_meta_analysis_extended")

  # Check main elements exist
  expected_elements <- c(
    "meta_results",
    "heterogeneity",
    "publication_bias",
    "moderator_analysis",
    "individual_studies",
    "forest_plot_data",
    "metadata",
    "message"
  )
  expect_true(all(expected_elements %in% names(result)))

  # Check meta-results values
  expect_true(!is.null(result$meta_results$combined_mean))
  expect_true(!is.null(result$meta_results$ci_lower))
  expect_true(!is.null(result$meta_results$ci_upper))
  expect_true(result$meta_results$n_studies == n_studies)

  # Check heterogeneity values
  expect_true(!is.null(result$heterogeneity$Q))
  expect_true(!is.null(result$heterogeneity$i2))

  # Check publication bias
  expect_true(!is.null(result$publication_bias$funnel_asymmetry))
  expect_true(!is.null(result$publication_bias$fail_safe_n))

  # Check moderator analysis
  expect_true(length(result$moderator_analysis) == 3)
  expect_true(all(c("year", "taxon", "trait_type") %in% names(result$moderator_analysis)))

  # Check forest plot data
  expect_true(all(c("study", "effect_size", "variance", "weight", "ci_lower", "ci_upper") %in%
                    names(result$individual_studies)))

  # Test print method
  expect_output(print(result), "Extended Plasticity Meta-Analysis Results")
})
