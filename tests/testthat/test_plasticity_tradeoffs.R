# tests/testthat/test-plasticity_tradeoffs.R
test_that("plasticity_tradeoffs pca method returns PCA structure", {
  # CORREGIDO: estructura correcta
  mock_result <- list(
    individual_plasticity = list(
      trait1 = list(list(plasticity_norm = 0.1), list(plasticity_norm = 0.2), list(plasticity_norm = 0.3)),
      trait2 = list(list(plasticity_norm = 0.4), list(plasticity_norm = 0.5), list(plasticity_norm = 0.6)),
      trait3 = list(list(plasticity_norm = 0.7), list(plasticity_norm = 0.8), list(plasticity_norm = 0.9))
    )
  )

  result <- plasticity_tradeoffs(mock_result, method = "pca")

  expect_true("pca_result" %in% names(result))
  expect_s3_class(result$pca_result, "prcomp")
})

test_that("plasticity_tradeoffs constraint method calculates constraint index", {
  # CORREGIDO: estructura correcta
  mock_result <- list(
    individual_plasticity = list(
      trait1 = list(list(plasticity_norm = 0.1), list(plasticity_norm = 0.2), list(plasticity_norm = 0.3)),
      trait2 = list(list(plasticity_norm = 0.4), list(plasticity_norm = 0.5), list(plasticity_norm = 0.6))
    )
  )

  result <- plasticity_tradeoffs(mock_result, method = "constraint")

  expect_true("constraint_index" %in% names(result))
  expect_type(result$constraint_index, "double")
})
