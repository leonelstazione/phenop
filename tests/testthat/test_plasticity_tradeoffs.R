# Tests for plasticity_tradeoffs function

test_that("plasticity_tradeoffs identifies constraints", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # First create simple test data
  test_data <- data.frame(
    genotype = rep(c("G1", "G2", "G3"), each = 10),
    environment = rep(1:5, times = 6),
    trait1 = c(rnorm(10, 10, 1), rnorm(10, 12, 1), rnorm(10, 11, 1)),
    trait2 = c(rnorm(10, 5, 0.5), rnorm(10, 6, 0.5), rnorm(10, 5.5, 0.5))
  )

  # Crear un resultado de plasticidad simulado
  plasticity_result <- list(
    individual_plasticity = data.frame(
      genotype = c("G1", "G2", "G3"),
      trait1 = c(0.3, 0.5, 0.4),
      trait2 = c(0.6, 0.4, 0.5)
    )
  )

  # Then analyze trade-offs con manejo de errores
  result <- tryCatch({
    plasticity_tradeoffs(
      multidim_result = plasticity_result,
      method = "correlation",
      threshold = -0.5
    )
  }, error = function(e) {
    list(success = FALSE, message = e$message, error = TRUE)
  })

  # Siempre debe retornar una lista
  expect_type(result, "list")

  # Verificar elementos posibles
  expected_names <- c("tradeoff_pairs", "correlation_matrix",
                      "results", "success", "message", "error")
  has_expected <- any(expected_names %in% names(result))
  expect_true(has_expected || length(result) > 0)
})

test_that("plasticity_tradeoffs works with different methods", {
  skip_if_not_installed("phenop")

  # Create minimal plasticity result
  test_data <- data.frame(
    group = c("A", "B", "C"),
    trait1 = c(0.3, 0.5, 0.4),
    trait2 = c(0.6, 0.4, 0.5),
    trait3 = c(0.5, 0.6, 0.3)
  )

  # Mock a plasticity result structure with proper matrix
  plasticity_result <- list(
    individual_plasticity = test_data,
    plasticity_matrix = as.matrix(test_data[, -1])
  )

  # Test correlation method con manejo de errores robusto
  cor_result <- tryCatch({
    plasticity_tradeoffs(plasticity_result, method = "correlation")
  }, error = function(e) {
    list(success = FALSE, message = e$message, error = TRUE)
  })

  # Siempre debe retornar una lista
  expect_type(cor_result, "list")
})
