test_that("plasticity_meta_analysis basic functionality", {

  # Datos de ejemplo
  study_data <- data.frame(
    study = paste0("S", 1:5),
    effect = rnorm(5, 0.5, 0.2),
    variance = runif(5, 0.05, 0.15)
  )

  # Ejecutar la función
  result <- plasticity_meta_analysis(
    study_data = study_data,
    effect_size = "effect",
    variance = "variance"
  )

  # Expectativas mínimas para que testthat registre ejecución
  expect_type(result, "list")
  expect_true("meta_analysis" %in% names(result))
  expect_true("publication_bias" %in% names(result))
  expect_true("phylogenetic_signal" %in% names(result))
})

test_that("plasticity_meta_analysis full coverage", {

  # --- 1. Datos de ejemplo normales ---
  study_data <- data.frame(
    study = paste0("S", 1:5),
    effect = c(0.6, 0.7, 0.2, 0.8, 0.5),  # algunos > 0.5
    variance = c(0.05, 0.1, 0.08, 0.12, 0.09)
  )

  result <- plasticity_meta_analysis(
    study_data = study_data,
    effect_size = "effect",
    variance = "variance"
  )

  # Comprobar que se devuelven todas las secciones
  expect_type(result, "list")
  expect_true(all(c("meta_analysis", "publication_bias", "phylogenetic_signal", "method") %in% names(result)))

  # --- 2. Comprobar cálculos internos ---

  # Weighted mean
  weighted_mean_expected <- weighted.mean(study_data$effect, 1/study_data$variance)
  expect_equal(result$meta_analysis$weighted_mean, weighted_mean_expected)

  # Heterogeneidad e i_squared
  total_var <- var(study_data$effect)
  sampling_var <- mean(study_data$variance)
  heterogeneity_expected <- total_var - sampling_var
  i_squared_expected <- max(0, heterogeneity_expected / total_var * 100)

  expect_equal(result$meta_analysis$heterogeneity, heterogeneity_expected)
  expect_equal(result$meta_analysis$i_squared, i_squared_expected)

  # --- 3. Casos de publication bias ---

  # Funnel correlation
  precision <- 1 / sqrt(study_data$variance)
  correlation_expected <- cor(study_data$effect, precision)
  expect_equal(result$publication_bias$funnel_asymmetry, correlation_expected)

  # Fail-safe N
  significant <- sum(study_data$effect > 0.5)
  fail_safe_expected <- nrow(study_data) / significant
  expect_equal(result$publication_bias$fail_safe_n, fail_safe_expected)

  # --- 4. Casos especiales: todos efectos < 0.5 -> fail_safe_n = Inf ---
  study_data2 <- data.frame(
    study = paste0("S", 1:3),
    effect = c(0.1, 0.2, 0.3),
    variance = c(0.05, 0.08, 0.1)
  )

  result2 <- plasticity_meta_analysis(
    study_data = study_data2,
    effect_size = "effect",
    variance = "variance"
  )

  expect_equal(result2$publication_bias$fail_safe_n, Inf)
  expect_equal(result2$publication_bias$significant_studies, 0)

  # --- 5. Casos con NA ---
  study_data3 <- data.frame(
    study = paste0("S", 1:4),
    effect = c(0.5, NA, 0.7, 0.2),
    variance = c(0.05, 0.08, NA, 0.1)
  )

  result3 <- plasticity_meta_analysis(
    study_data = study_data3,
    effect_size = "effect",
    variance = "variance"
  )

  expect_type(result3$meta_analysis$weighted_mean, "double")
  expect_type(result3$publication_bias$funnel_asymmetry, "double")
  expect_equal(result3$phylogenetic_signal$phylogenetic_signal, NA)

  # --- 6. Método por defecto ---
  expect_equal(result$method, "Plasticity Meta-Analysis with Original Extensions")
})
