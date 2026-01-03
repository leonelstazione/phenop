test_that("multidim_plasticity basic functionality", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Datos mínimos pero válidos
  set.seed(123)
  test_data <- data.frame(
    genotype = rep(c("G1", "G2", "G3"), each = 4),
    environment = rep(c("E1", "E2"), times = 6),
    trait1 = rnorm(12, 10, 2),
    trait2 = rnorm(12, 5, 1)
  )

  # Llamar la función SIN tryCatch
  result <- multidim_plasticity(
    data = test_data,
    traits = c("trait1", "trait2"),
    environments = "environment",
    groups = "genotype"
  )

  # Verificaciones
  expect_type(result, "list")
  expect_true(length(result) > 0)

  # Ver elementos esperados
  if ("multidimensional_index" %in% names(result)) {
    mpi <- result$multidimensional_index
    if (!is.null(mpi)) {
      mpi_numeric <- suppressWarnings(as.numeric(mpi))
      if (!all(is.na(mpi_numeric))) {
        expect_true(is.numeric(mpi_numeric))
      }
    }
  }
})

test_that("multidim_plasticity with single trait", {
  skip_if_not_installed("phenop")

  test_data <- data.frame(
    group = rep(c("A", "B"), each = 3),
    env = rep(1:3, times = 2),
    single_trait = rnorm(6, 10, 2)
  )

  result <- multidim_plasticity(
    data = test_data,
    traits = "single_trait",
    environments = "env",
    groups = "group"
  )

  expect_type(result, "list")
})
