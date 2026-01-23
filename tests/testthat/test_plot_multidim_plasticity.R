# tests/testthat/test-plot_multidim_plasticity.R

# Helper para crear datos de prueba
create_test_multidim_result <- function(n_traits = 4, n_individuals = 10,
                                        with_cor = TRUE, with_names = TRUE) {
  trait_names <- paste0("trait", 1:n_traits)

  # Matriz de correlaciones
  cor_matrix <- NULL
  if (with_cor) {
    set.seed(123)
    cor_matrix <- matrix(runif(n_traits^2, -1, 1), nrow = n_traits)
    diag(cor_matrix) <- 1

    if (with_names) {
      rownames(cor_matrix) <- trait_names
      colnames(cor_matrix) <- trait_names
    }
  }

  # Datos de plasticidad individual
  individual_plasticity <- list()
  for (trait in trait_names) {
    individual_plasticity[[trait]] <- list(
      plasticity_norm = rnorm(n_individuals, mean = 0, sd = 1),
      plasticity_raw = rnorm(n_individuals, mean = 5, sd = 2)
    )
  }

  return(list(
    plasticity_correlations = cor_matrix,
    individual_plasticity = individual_plasticity
  ))
}

test_that("plot_multidim_plasticity handles correlation matrix without names", {
  # Matrix without row/col names
  mock_result <- create_test_multidim_result(with_names = FALSE)

  # Esta prueba ahora debería funcionar con la corrección
  p <- plot_multidim_plasticity(mock_result, type = "network")
  expect_s3_class(p, "ggplot")
})


test_that("plot_multidim_plasticity handles NULL and invalid inputs", {
  # NULL input
  expect_error(
    plot_multidim_plasticity(NULL),
    "multidim_result cannot be NULL"
  )

  # Non-list input
  expect_error(
    plot_multidim_plasticity("not a list"),
    "multidim_result must be a list"
  )

  # List without individual_plasticity
  bad_result <- list(some_other_field = "data")
  expect_error(
    plot_multidim_plasticity(bad_result),
    "No traits found"
  )

  # Empty individual_plasticity
  empty_result <- list(individual_plasticity = list())
  expect_error(
    plot_multidim_plasticity(empty_result),
    "No traits found"
  )
})

test_that("plot_multidim_plasticity handles traits parameter correctly", {
  mock_result <- create_test_multidim_result(n_traits = 5)

  # When traits is NULL, uses all traits
  p <- plot_multidim_plasticity(mock_result, type = "network", traits = NULL)
  expect_s3_class(p, "ggplot")

  # Subset of traits
  p <- plot_multidim_plasticity(mock_result, type = "network",
                                traits = c("trait1", "trait2"))
  expect_s3_class(p, "ggplot")

  # With some non-existent traits (should use only existing ones)
  p <- plot_multidim_plasticity(mock_result,
                                type = "network",
                                traits = c("trait1", "nonexistent"))
  expect_s3_class(p, "ggplot")

  # All non-existent traits should error
  expect_error(
    plot_multidim_plasticity(mock_result,
                             type = "network",
                             traits = c("nonexistent1", "nonexistent2")),
    "None of the specified traits found"
  )

  # Empty traits vector
  expect_error(
    plot_multidim_plasticity(mock_result, type = "network", traits = character(0)),
    "None of the specified traits found"
  )
})

test_that("plot_multidim_plasticity handles missing correlation matrix", {
  # Result without correlation matrix
  mock_result <- create_test_multidim_result(with_cor = FALSE)

  # Should work for landscape
  p <- plot_multidim_plasticity(mock_result, type = "landscape")
  expect_s3_class(p, "ggplot")

  # For network, should create empty plot with warning
  expect_warning(
    p <- plot_multidim_plasticity(mock_result, type = "network"),
    "No plasticity_correlations found"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity handles correlation matrix without names", {
  # Matrix without row/col names
  mock_result <- create_test_multidim_result(with_names = FALSE)

  # Debería funcionar ahora con la corrección
  p <- plot_multidim_plasticity(mock_result, type = "network")
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity handles single trait", {
  mock_result <- create_test_multidim_result(n_traits = 1)

  # Network with single trait
  p <- plot_multidim_plasticity(mock_result, type = "network")
  expect_s3_class(p, "ggplot")

  # Landscape with single trait
  p <- plot_multidim_plasticity(mock_result, type = "landscape")
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity integration and radar stubs work", {
  mock_result <- create_test_multidim_result()

  # Integration plot stub
  expect_warning(
    p <- plot_multidim_plasticity(mock_result, type = "integration"),
    "not implemented"
  )
  expect_s3_class(p, "ggplot")

  # Radar plot stub
  expect_warning(
    p <- plot_multidim_plasticity(mock_result, type = "radar"),
    "not implemented"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity handles invalid plot type", {
  mock_result <- create_test_multidim_result()

  expect_error(
    plot_multidim_plasticity(mock_result, type = "invalid_type"),
    "Unknown plot type"
  )
})

test_that("plot_multidim_plasticity uses plot_options parameter", {
  mock_result <- create_test_multidim_result()

  # Test that function accepts plot_options without error
  p <- plot_multidim_plasticity(mock_result,
                                type = "network",
                                plot_options = list())
  expect_s3_class(p, "ggplot")
})
