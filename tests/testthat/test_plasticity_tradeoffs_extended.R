test_that("plasticity_tradeoffs_extended works", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Crear un resultado de plasticidad válido
  plasticity_result <- list(
    individual_plasticity = data.frame(
      genotype = c("G1", "G2", "G3"),
      trait1 = c(0.3, 0.5, 0.4),
      trait2 = c(0.6, 0.4, 0.5)
    )
  )

  # Llamar con TODOS los argumentos requeridos
  result <- plasticity_tradeoffs_extended(
    plasticity_result = plasticity_result,
    method = "correlation"
  )

  expect_type(result, "list")
})
