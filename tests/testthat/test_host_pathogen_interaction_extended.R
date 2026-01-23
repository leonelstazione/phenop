# tests/testthat/test-host_pathogen_interaction_extended.R
test_that("host_pathogen_interaction_extended validates inputs", {
  # Datos sin columnas requeridas
  host_df <- data.frame(a = 1:10)
  path_df <- data.frame(b = 1:10)

  expect_error(
    host_pathogen_interaction_extended(
      host_data = host_df,  # FIJATE: host_data, no host_df
      pathogen_data = path_df,
      host_traits = "missing",
      pathogen_traits = "missing",
      environments = "env"  # ESTE PARÁMETRO FALTABA
    ),
    regexp = "Missing host columns"
  )
})

test_that("host_pathogen_interaction_extended works with correlation metric", {
  # Datos simulados reales
  host_data <- data.frame(
    group = rep(1:5, each = 4),
    temp = rep(c(20, 25), 10),
    resistance = rnorm(20),
    defense = rnorm(20)
  )

  pathogen_data <- data.frame(
    group = rep(1:5, each = 4),
    temp = rep(c(20, 25), 10),
    virulence = rnorm(20),
    toxicity = rnorm(20)
  )

  result <- host_pathogen_interaction_extended(
    host_data = host_data,
    pathogen_data = pathogen_data,
    host_traits = c("resistance", "defense"),
    pathogen_traits = c("virulence", "toxicity"),
    environments = "temp",
    interaction_metrics = c("correlation", "plasticity_matching"),
    host_group = "group",
    pathogen_group = "group"
  )

  # CORREGIDO: host_pathogen_interaction_extended (no host_pathogen)
  expect_s3_class(result, "host_pathogen_interaction_extended")
  expect_true("interaction_correlations" %in% names(result))
  expect_true("plasticity_matching" %in% names(result))
})

test_that("host_pathogen_interaction_extended handles safe_multidim_plasticity failure", {
  # Datos que harán fallar safe_multidim_plasticity
  # NO usar dataframe vacío, usar uno con datos pero que falle
  bad_data <- data.frame(
    a = 1:3,  # Agregar al menos una fila
    b = 4:6
  )

  result <- host_pathogen_interaction_extended(
    host_data = bad_data,
    pathogen_data = bad_data,
    host_traits = "a",
    pathogen_traits = "b",
    environments = "a",
    interaction_metrics = "correlation",
    host_group = "a",  # Especificar grupo
    pathogen_group = "b"
  )

  # Debería retornar estructura aunque falle safe_multidim_plasticity
  expect_s3_class(result, "host_pathogen_interaction_extended")
})
