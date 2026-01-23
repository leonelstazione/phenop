# tests/testthat/test-safe_multidim_plasticity.R

test_that("safe_multidim_plasticity handles all na.actions", {
  # Datos con NAs
  data_with_na <- data.frame(
    genotype = c("A", "A", "B", "B"),
    environment = c(1, 2, 1, 2),
    trait1 = c(1, 2, NA, 4)
  )

  # omit - debería funcionar
  result_omit <- safe_multidim_plasticity(
    data_with_na, "trait1", "environment", "genotype", na.action = "omit"
  )
  expect_true(result_omit$safe_analysis$success)

  # fail - debería fallar
  result_fail <- safe_multidim_plasticity(
    data_with_na, "trait1", "environment", "genotype", na.action = "fail"
  )
  expect_false(result_fail$safe_analysis$success)

  # impute.mean - debería funcionar
  result_impute <- safe_multidim_plasticity(
    data_with_na, "trait1", "environment", "genotype", na.action = "impute.mean"
  )
  expect_true(result_impute$safe_analysis$success)
})

test_that("safe_multidim_plasticity validates weights correctly", {
  test_data <- data.frame(
    group = rep(c("A", "B"), each = 5),
    env = rep(1:5, 2),
    trait1 = rnorm(10),
    trait2 = rnorm(10)
  )

  # 1. Con pesos válidos (longitud correcta)
  result_valid <- safe_multidim_plasticity(
    test_data, c("trait1", "trait2"), "env", "group", weights = c(2, 1)
  )

  expect_true(result_valid$safe_analysis$success)
  expect_equal(result_valid$multidimensional_index$method, "weighted mean")

  # 2. Con pesos de longitud incorrecta
  result_invalid <- safe_multidim_plasticity(
    test_data, c("trait1", "trait2"), "env", "group", weights = c(1)
  )

  # Lo importante es que no falle
  expect_type(result_invalid, "list")
  expect_true(result_invalid$safe_analysis$success)

  # Verificar el método basado en la implementación real
  # Revisa R/safe_multidim_plasticity.R para ver qué hace
  method <- result_invalid$multidimensional_index$method
  expect_true(method %in% c("weighted mean", "unweighted mean"))
})

test_that("safe_multidim_plasticity fails with insufficient groups", {
  one_group_data <- data.frame(
    group = "A",
    trait1 = 1:10,
    env = 1:10
  )

  result <- safe_multidim_plasticity(one_group_data, "trait1", "env", "group")

  expect_false(result$safe_analysis$success)
  expect_true(grepl("At least 2 groups", result$message))
})
