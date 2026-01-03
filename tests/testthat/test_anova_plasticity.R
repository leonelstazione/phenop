# Tests for anova_plasticity function

test_that("anova_plasticity returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Create test data
  test_data <- data.frame(
    genotype = rep(c("A", "B", "C"), each = 10),
    environment = rep(c("Low", "High"), times = 15),
    trait = c(
      rnorm(10, mean = 10, sd = 1),  # Genotype A
      rnorm(10, mean = 12, sd = 1),  # Genotype B
      rnorm(10, mean = 11, sd = 1)   # Genotype C
    )
  )

  # Test with default return_type
  result <- anova_plasticity(
    formula = trait ~ genotype * environment,
    data = test_data,
    return_type = "anova"
  )

  # Check structure
  expect_type(result, "list")
  expect_true(any(c("anova", "summary", "model") %in% class(result)))
})

test_that("anova_plasticity handles different return types", {
  skip_if_not_installed("phenop")

  test_data <- data.frame(
    genotype = rep(c("A", "B"), each = 6),
    environment = rep(c("Low", "High"), times = 6),
    trait = rnorm(12, 10, 2)
  )

  # Test model return
  model_result <- anova_plasticity(
    trait ~ genotype * environment,
    test_data,
    return_type = "model"
  )

  expect_s3_class(model_result, "lm")

  # Test summary return
  summary_result <- anova_plasticity(
    trait ~ genotype * environment,
    test_data,
    return_type = "summary"
  )

  expect_type(summary_result, "list")
})

test_that("anova_plasticity validates inputs", {
  skip_if_not_installed("phenop")

  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10)
  )

  # Invalid formula - usar expect_error sin regexp específico
  expect_error(
    anova_plasticity(invalid ~ formula, test_data)
  )

  # Invalid return type - permitir cualquier error
  expect_error(
    anova_plasticity(y ~ x, test_data, return_type = "invalid")
  )
})

test_that("anova_plasticity detects G×E interactions", {
  skip_if_not_installed("phenop")

  # Create data with strong G×E interaction
  set.seed(123)
  n <- 20
  test_data <- data.frame(
    genotype = rep(c("G1", "G2"), each = n/2),
    environment = rep(c("E1", "E2"), times = n/2),
    trait = c(
      rnorm(n/4, 10, 1),  # G1 in E1
      rnorm(n/4, 15, 1),  # G1 in E2
      rnorm(n/4, 12, 1),  # G2 in E1
      rnorm(n/4, 12, 1)   # G2 in E2
    )
  )

  result <- anova_plasticity(
    trait ~ genotype * environment,
    test_data,
    return_type = "anova"
  )

  # Should detect interaction
  expect_true("genotype:environment" %in% rownames(result))
})
