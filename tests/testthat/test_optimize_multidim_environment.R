test_that("optimize_multidim_environment works", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Crear datos válidos
  test_data <- data.frame(
    genotype = rep(c("A", "B", "C"), each = 4),
    temperature = rep(c(20, 25, 30, 35), times = 3),
    humidity = rep(c(40, 60, 80, 50), times = 3),
    yield = c(10, 12, 14, 13, 8, 10, 12, 11, 9, 11, 15, 13),
    quality = c(5, 6, 7, 6, 4, 5, 6, 5, 5, 6, 8, 7)
  )

  # Llamar con TODOS los argumentos requeridos
  result <- optimize_multidim_environment(
    data = test_data,
    traits = c("yield", "quality"),
    environments = c("temperature", "humidity"),
    groups = "genotype",
    optimization_goal = "maximize"
  )

  expect_type(result, "list")
})

# AGREGAR al archivo test_optimize_multidim_environment.R

test_that("optimize_multidim_environment handles different optimization goals", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Crear datos
  set.seed(123)
  data <- data.frame(
    genotype = rep(c("A", "B", "C"), each = 6),
    temperature = rep(c(20, 25, 30), times = 6),
    humidity = rep(c(40, 60, 80), each = 6),
    yield = 10 + rnorm(18, 0, 2),
    quality = 5 + rnorm(18, 0, 1)
  )

  # Probar TODAS las opciones de optimization_goal
  goals <- c("maximize", "minimize", "compromise", "balance")

  for (goal in goals) {
    result <- optimize_multidim_environment(
      data = data,
      traits = c("yield", "quality"),
      environments = c("temperature", "humidity"),
      groups = "genotype",
      optimization_goal = goal
    )

    expect_type(result, "list")

    # Si la función tiene elementos específicos, verificarlos
    if ("optimal_conditions" %in% names(result)) {
      expect_true(is.data.frame(result$optimal_conditions) ||
                    is.list(result$optimal_conditions))
    }

    if ("message" %in% names(result)) {
      expect_type(result$message, "character")
    }
  }
})

test_that("optimize_multidim_environment works with weights", {
  skip_if_not_installed("phenop")

  data <- data.frame(
    group = rep(c("A", "B"), each = 4),
    env = rep(1:4, times = 2),
    trait1 = rnorm(8, 10, 2),
    trait2 = rnorm(8, 5, 1)
  )

  # Probar con pesos diferentes
  result <- optimize_multidim_environment(
    data = data,
    traits = c("trait1", "trait2"),
    environments = "env",
    groups = "group",
    optimization_goal = "maximize",
    weights = c(0.7, 0.3)  # Dar más peso al primer rasgo
  )

  expect_type(result, "list")
})
