test_that("plasticity_meta_analysis_extended works", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Datos válidos
  study_data <- data.frame(
    study = paste("Study", 1:8),
    es = rnorm(8, 0.6, 0.2),
    vi = runif(8, 0.1, 0.3),
    group = rep(c("A", "B"), each = 4)
  )

  # Llamar con TODOS los argumentos requeridos
  result <- plasticity_meta_analysis_extended(
    study_data = study_data,
    effect_size = "es",
    variance = "vi",
    study_labels = "study",
    method = "random"
  )

  expect_type(result, "list")
})
