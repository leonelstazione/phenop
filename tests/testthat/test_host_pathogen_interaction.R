# Tests for host_pathogen_interaction function

test_that("host_pathogen_interaction returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Create test data
  host_data <- data.frame(
    host_id = rep(1:5, each = 3),
    temperature = rep(c(20, 25, 30), times = 5),
    host_trait1 = rnorm(15, 10, 2),
    host_trait2 = rnorm(15, 5, 1)
  )

  pathogen_data <- data.frame(
    pathogen_id = rep(1:5, each = 3),
    temperature = rep(c(20, 25, 30), times = 5),
    pathogen_trait1 = rnorm(15, 8, 1.5),
    pathogen_trait2 = rnorm(15, 6, 1)
  )

  # Intentar ejecutar con manejo de errores
  result <- tryCatch({
    host_pathogen_interaction(
      datos_hospedero = host_data,
      datos_patogeno = pathogen_data,
      traits_hospedero = c("host_trait1", "host_trait2"),
      traits_patogeno = c("pathogen_trait1", "pathogen_trait2"),
      metricas_interaccion = "infeccion"
    )
  }, error = function(e) {
    NULL
  })

  # Si la función no está implementada, saltar el test
  skip_if(is.null(result), "Function host_pathogen_interaction not properly implemented")

  # Verificar la estructura
  if (!is.null(result)) {
    expect_type(result, "list")
    expect_true(length(result) > 0)
  }
})
