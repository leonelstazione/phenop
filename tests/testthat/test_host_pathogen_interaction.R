# tests/testthat/test-host_pathogen_interaction.R

test_that("host_pathogen_interaction returns correct structure", {
  # Datos de prueba reales
  test_data <- data.frame(
    insect_population = rep(c("PopA", "PopB"), each = 3),
    fungus_strain = rep(c("StrainX", "StrainY"), times = 3),
    temperature = rep(c(20, 25, 30), 2),
    insect_size = c(10, 12, 14, 8, 9, 10),
    fungus_growth = c(1.0, 1.2, 1.4, 0.8, 0.9, 1.0)
  )

  result <- host_pathogen_interaction(  # SIN "simple"
    data = test_data,
    host_trait = "insect_size",
    pathogen_trait = "fungus_growth",
    environment = "temperature"
  )

  # Verificar la estructura básica
  expect_type(result, "list")
  expect_named(result, c("host_plasticity", "pathogen_plasticity", "difference",
                         "interpretation", "details"))

  # Verificar tipos
  expect_type(result$host_plasticity, "double")
  expect_type(result$pathogen_plasticity, "double")
  expect_type(result$difference, "double")
  expect_type(result$interpretation, "character")
  expect_type(result$details, "list")

  # Verificar que el cálculo de CV sea >= 0
  expect_true(result$host_plasticity >= 0)
  expect_true(result$pathogen_plasticity >= 0)

  # Verificar estructura de details
  expect_named(result$details, c("host", "pathogen"))
  expect_s3_class(result$details$host, "data.frame")
  expect_s3_class(result$details$pathogen, "data.frame")
})

test_that("host_pathogen_interaction validates input correctly", {
  # Datos sin columnas requeridas
  bad_data <- data.frame(wrong_col = 1:10)

  # Esto debería fallar porque las columnas no existen
  expect_error(
    host_pathogen_interaction(bad_data, "nonexistent", "nonexistent", "nonexistent"),  # SIN "simple"
    regexp = "column|not found|missing"
  )
})

test_that("host_pathogen_interaction handles NAs correctly", {
  # Datos con NA
  data_with_na <- data.frame(
    insect_population = c("A", "A", "B", "B"),
    fungus_strain = c("X", "X", "Y", "Y"),
    temperature = c(20, 25, 20, 25),
    insect_size = c(10, NA, 12, 13),
    fungus_growth = c(1.0, 1.1, NA, 1.3)
  )

  result <- host_pathogen_interaction(  # SIN "simple"
    data_with_na,
    host_trait = "insect_size",
    pathogen_trait = "fungus_growth",
    environment = "temperature"
  )

  # Debería funcionar (na.rm = TRUE está en el código)
  expect_type(result, "list")
  expect_true(is.numeric(result$host_plasticity))
  expect_true(is.numeric(result$pathogen_plasticity))
})

test_that("host_pathogen_interaction calculates CV correctly", {
  # Datos con variación conocida
  test_data <- data.frame(
    group = rep(c("A", "B"), each = 3),
    env = rep(1:3, 2),
    trait = c(10, 20, 30, 5, 10, 15)  # CV de A: sd=10/mean=20=0.5, CV de B: igual
  )

  result <- host_pathogen_interaction(  # SIN "simple"
    data = test_data,
    host_trait = "trait",
    pathogen_trait = "trait",
    environment = "env",
    host_group = "group",
    pathogen_group = "group"
  )

  # CV = sd/mean = 10/20 = 0.5 para ambos
  expect_equal(result$host_plasticity, 0.5, tolerance = 0.01)
  expect_equal(result$difference, 0, tolerance = 0.01)
})

test_that("host_pathogen_interaction returns correct interpretation", {
  # Mockear datos para cada caso de interpretación
  test_data <- data.frame(
    insect_population = rep(c("A", "B"), each = 3),
    fungus_strain = rep(c("X", "Y"), times = 3),
    temperature = rep(c(20, 25, 30), 2),
    insect_size = c(10, 12, 14, 8, 9, 10),
    fungus_growth = c(1.0, 1.2, 1.4, 0.8, 0.9, 1.0)
  )

  result <- host_pathogen_interaction(  # SIN "simple"
    test_data,
    host_trait = "insect_size",
    pathogen_trait = "fungus_growth",
    environment = "temperature"
  )

  # Verificar que haya una interpretación válida
  valid_interpretations <- c(
    "Host shows substantially higher plasticity",
    "Host shows moderately higher plasticity",
    "Pathogen shows moderately higher plasticity",
    "Pathogen shows substantially higher plasticity"
  )
  expect_true(result$interpretation %in% valid_interpretations)
})

test_that("host_pathogen_interaction works with custom group names", {  # Corregido typo "host_pathagon"
  test_data <- data.frame(
    host_id = rep(c("H1", "H2"), each = 3),
    pathogen_id = rep(c("P1", "P2"), times = 3),
    temp = rep(c(20, 25, 30), 2),
    host_trait = c(10, 12, 14, 8, 9, 10),
    pathogen_trait = c(1.0, 1.2, 1.4, 0.8, 0.9, 1.0)
  )

  result <- host_pathogen_interaction(  # SIN "simple"
    data = test_data,
    host_trait = "host_trait",
    pathogen_trait = "pathogen_trait",
    environment = "temp",
    host_group = "host_id",
    pathogen_group = "pathogen_id"
  )

  expect_type(result, "list")
  expect_named(result$details, c("host", "pathogen"))
  expect_equal(nrow(result$details$host), 2)  # 2 grupos
  expect_equal(nrow(result$details$pathogen), 2)
})
