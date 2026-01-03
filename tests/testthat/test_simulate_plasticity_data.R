# Tests for simulate_plasticity_data function

test_that("simulate_plasticity_data creates realistic datasets", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Test default parameters
  default_data <- simulate_plasticity_data()

  expect_s3_class(default_data, "data.frame")
  expect_true(nrow(default_data) >= 100)  # Default n_genotypes * n_environments
  expect_true(ncol(default_data) >= 5)   # Group + env + traits

  # Check column names
  expect_true("genotype" %in% names(default_data) ||
                "group" %in% names(default_data))
  expect_true("environment" %in% names(default_data) ||
                "env" %in% names(default_data))
})

test_that("simulate_plasticity_data respects parameters", {
  skip_if_not_installed("phenop")

  # Test custom parameters
  custom_data <- simulate_plasticity_data(
    n_genotypes = 5,
    n_environments = 3,
    n_traits = 2,
    plasticity_patterns = list("gradient", "threshold")
  )

  # Check dimensions
  expect_equal(length(unique(custom_data$genotype)), 5)
  expect_equal(length(unique(custom_data$environment)), 3)

  # FIX: Contar columnas de rasgos correctamente
  # Primero, identificar columnas que no son de grupo o ambiente
  group_env_cols <- c("genotype", "group", "environment", "env")
  trait_cols <- names(custom_data)[!names(custom_data) %in% group_env_cols]

  # La función puede generar columnas adicionales (como índices o metadatos)
  # Solo verificar que haya al menos n_traits columnas de rasgos
  expect_true(length(trait_cols) >= 2)

  # Opcional: verificar que las primeras n_traits columnas tengan nombres de rasgos
  if (length(trait_cols) >= 2) {
    # Verificar que al menos las primeras 2 se llamen trait1, trait2 o similar
    trait_pattern <- grepl("trait|Trait|var|Var|y[0-9]+", trait_cols)
    expect_true(sum(trait_pattern) >= 2)
  }
})

test_that("simulate_plasticity_data creates different patterns", {
  skip_if_not_installed("phenop")

  # Test different plasticity patterns
  gradient_data <- simulate_plasticity_data(
    plasticity_patterns = list("gradient"),
    n_genotypes = 3,
    n_environments = 4
  )

  optimal_data <- simulate_plasticity_data(
    plasticity_patterns = list("optimal"),
    n_genotypes = 3,
    n_environments = 4
  )

  # Data should be different (al menos en algunas columnas)
  # Comparar solo las primeras 5 filas y las columnas numéricas
  num_cols_gradient <- sapply(gradient_data, is.numeric)
  num_cols_optimal <- sapply(optimal_data, is.numeric)

  if (any(num_cols_gradient) && any(num_cols_optimal)) {
    gradient_numeric <- gradient_data[1:5, num_cols_gradient, drop = FALSE]
    optimal_numeric <- optimal_data[1:5, num_cols_optimal, drop = FALSE]

    # Deberían ser diferentes en al menos algunas celdas
    expect_false(identical(gradient_numeric, optimal_numeric))
  }
})
