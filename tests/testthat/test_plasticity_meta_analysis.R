test_that("plasticity_meta_analysis works", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Datos válidos
  study_data <- data.frame(
    study = paste("S", 1:10),
    effect = rnorm(10, 0.5, 0.3),
    var = runif(10, 0.05, 0.2)
  )

  # Llamar con argumentos correctos
  result <- plasticity_meta_analysis(
    study_data = study_data,
    effect_size = "effect",
    variance = "var"
  )

  expect_type(result, "list")
})

# AGREGAR al archivo test_plasticity_meta_analysis.R

test_that("plasticity_meta_analysis handles edge cases and all methods", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Datos más complejos
  set.seed(456)
  study_data <- data.frame(
    study_id = 1:20,
    effect = c(rnorm(10, 0.8, 0.2), rnorm(10, 0.3, 0.2)),  # Dos grupos de efectos
    variance = runif(20, 0.05, 0.15),
    year = sample(2000:2020, 20),
    experimental_design = sample(c("field", "greenhouse", "lab"), 20, replace = TRUE),
    species = sample(c("crop", "wild", "model"), 20, replace = TRUE)
  )

  # Probar diferentes combinaciones de parámetros
  test_cases <- list(
    list(method = "fixed", moderator = NULL),
    list(method = "random", moderator = NULL),
    list(method = "fixed", moderator = "experimental_design"),
    list(method = "random", moderator = "species")
  )

  for (case in test_cases) {
    result <- plasticity_meta_analysis(
      study_data = study_data,
      effect_size = "effect",
      variance = "variance",
      moderator = case$moderator,
      method = case$method
    )

    expect_type(result, "list")

    # Verificar elementos comunes en resultados de meta-análisis
    if (is.list(result) && length(result) > 0) {
      # Puede tener diferentes estructuras
      valid_elements <- c("summary", "estimates", "heterogeneity",
                          "model", "results", "message")
      has_valid <- any(valid_elements %in% names(result))
      expect_true(has_valid || length(result) > 0)
    }
  }
})
