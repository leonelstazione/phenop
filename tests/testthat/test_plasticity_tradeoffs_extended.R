test_that("plasticity_tradeoffs_extended works with simulated data", {

  # Simulate a multidimensional plasticity result
  set.seed(123)
  n_groups <- 8
  n_traits <- 3
  plasticity_matrix <- data.frame(
    trait1 = rnorm(n_groups, 0.5, 0.2),
    trait2 = rnorm(n_groups, 0.3, 0.1),
    trait3 = rnorm(n_groups, -0.2, 0.15),
    group = paste0("Group", 1:n_groups)
  )

  plasticity_result <- list(
    individual_plasticity = plasticity_matrix
  )

  # Run extended trade-off analysis
  tradeoff_result <- plasticity_tradeoffs_extended(
    plasticity_result,
    method = "correlation",
    threshold = -0.1
  )

  # Check class
  expect_s3_class(tradeoff_result, "plasticity_tradeoffs_extended")

  # Check main elements exist
  expected_elements <- c(
    "tradeoff_matrix",
    "p_value_matrix",
    "significant_tradeoffs",
    "pca_results",
    "constraint_indices",
    "visualization_data",
    "analysis_parameters",
    "message"
  )
  expect_true(all(expected_elements %in% names(tradeoff_result)))

  # Check tradeoff matrix is square and correct size
  expect_equal(nrow(tradeoff_result$tradeoff_matrix), n_traits)
  expect_equal(ncol(tradeoff_result$tradeoff_matrix), n_traits)

  # Check p_value_matrix has same dimensions
  expect_equal(dim(tradeoff_result$p_value_matrix), dim(tradeoff_result$tradeoff_matrix))

  # Check significant trade-offs are consistent
  if (nrow(tradeoff_result$significant_tradeoffs) > 0) {
    expect_true(all(tradeoff_result$significant_tradeoffs$correlation <
                      tradeoff_result$analysis_parameters$threshold))
    expect_true(all(tradeoff_result$significant_tradeoffs$p_value < 0.05))
  }

  # Check visualization data
  expect_true(!is.null(tradeoff_result$visualization_data))
  expect_true("plasticity_matrix" %in% names(tradeoff_result$visualization_data))

  # Check analysis parameters
  expect_equal(tradeoff_result$analysis_parameters$n_traits, n_traits)
  expect_equal(tradeoff_result$analysis_parameters$n_groups, n_groups)

  # Test print method
  expect_output(print(tradeoff_result), "Extended Phenotypic Plasticity Trade-off Analysis")
})
