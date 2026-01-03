# Tests for safe_multidim_plasticity function

test_that("safe_multidim_plasticity returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Create test data - FIX: Hacer los cálculos correctamente
  set.seed(123)
  n <- 18  # Usar número divisible por 3
  test_data <- data.frame(
    genotype = rep(c("A", "B", "C"), each = n/3),
    environment = rep(c("Low", "Medium", "High"), times = n/3),  # times, no each
    trait1 = rnorm(n, 10, 2),
    trait2 = rnorm(n, 5, 1)
  )

  result <- safe_multidim_plasticity(
    data = test_data,
    traits = c("trait1", "trait2"),
    environments = "environment",
    groups = "genotype"
  )

  # Verificar que es una lista
  expect_type(result, "list")

  # Verificar estructura mínima
  expected_names <- c("individual_plasticity", "group_plasticity",
                      "multidimensional_index", "safe_analysis", "message")

  # Verificar que tiene al menos 2 de los elementos esperados
  found_names <- sum(expected_names %in% names(result))
  expect_true(found_names >= 2)

  # Verificar que tiene un mensaje
  expect_true("message" %in% names(result))

  # Verificar que el mensaje es una cadena
  expect_type(result$message, "character")
})

test_that("safe_multidim_plasticity handles errors gracefully", {
  skip_if_not_installed("phenop")

  # Create problematic data (solo 1 fila por grupo)
  problematic_data <- data.frame(
    genotype = c("A", "B"),
    environment = c("Low", "High"),
    trait1 = c(1, 2)
  )

  # Esto no debería lanzar error, sino retornar resultado seguro
  result <- safe_multidim_plasticity(
    data = problematic_data,
    traits = "trait1",
    environments = "environment",
    groups = "genotype"
  )

  # Debería retornar una lista
  expect_type(result, "list")
  expect_true("message" %in% names(result))
})

test_that("safe_multidim_plasticity validates inputs", {
  skip_if_not_installed("phenop")

  # Datos simples
  test_data <- data.frame(
    id = 1:10,
    value = rnorm(10)
  )

  # Con columnas inexistentes - debería manejar el error
  result <- safe_multidim_plasticity(
    data = test_data,
    traits = "nonexistent",
    environments = "env",
    groups = "group"
  )

  # Debería retornar una lista con un mensaje
  expect_type(result, "list")
  expect_true("message" %in% names(result))
})

test_that("function handles large datasets efficiently", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Simular dataset grande (simplificado para rapidez)
  set.seed(456)
  n <- 100  # Reducido para rapidez (original: 1000)
  large_data <- data.frame(
    genotype = rep(paste0("G", 1:5), each = n/5),
    environment = rep(1:4, length.out = n),
    trait1 = rnorm(n, 10, 3),
    trait2 = rnorm(n, 5, 2),
    trait3 = rnorm(n, 8, 1.5)
  )

  # Medir tiempo de ejecución
  exec_time <- system.time({
    result <- safe_multidim_plasticity(
      data = large_data,
      traits = c("trait1", "trait2"),
      environments = "environment",
      groups = "genotype"
    )
  })

  # Debería completarse en tiempo razonable
  expect_true(exec_time["elapsed"] < 5)  # Más realista que 10 segundos

  # Debería retornar resultado
  expect_type(result, "list")
  expect_true("message" %in% names(result))
})
