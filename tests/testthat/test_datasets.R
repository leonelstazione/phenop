test_that("datos_demo has correct structure", {
  data(datos_demo)
  expect_s3_class(datos_demo, "data.frame")
  expect_equal(ncol(datos_demo), 8)
  expected_names <- c("insect_population", "fungus_strain", "temperature",
                      "humidity", "insect_size", "resistance",
                      "fungus_growth", "virulence")
  expect_named(datos_demo, expected_names)
})

test_that("demo_data exists and matches datos_demo", {
  data(demo_data)
  data(datos_demo)
  expect_s3_class(demo_data, "data.frame")
  expect_equal(dim(demo_data), dim(datos_demo))
  expect_equal(names(demo_data), names(datos_demo))
})

test_that("datasets have reasonable values", {
  data(datos_demo)
  data(demo_data)

  # Temperatura: 10-35°C (rango controlado)
  expect_true(all(datos_demo$temperature >= 10 & datos_demo$temperature <= 35),
              label = "datos_demo: temperatura 10-35°C")
  expect_true(all(demo_data$temperature >= 10 & demo_data$temperature <= 35),
              label = "demo_data: temperatura 10-35°C")

  # Humedad: 30-90% (rango controlado)
  expect_true(all(datos_demo$humidity >= 30 & datos_demo$humidity <= 90),
              label = "datos_demo: humedad 30-90%")
  expect_true(all(demo_data$humidity >= 30 & demo_data$humidity <= 90),
              label = "demo_data: humedad 30-90%")

  # Crecimiento hongo > 0
  expect_true(all(datos_demo$fungus_growth > 0),
              label = "datos_demo: crecimiento hongo > 0")
  expect_true(all(demo_data$fungus_growth > 0),
              label = "demo_data: crecimiento hongo > 0")
})

test_that("safe_multidim_plasticity works", {
  data(datos_demo)
  # Suprimir warnings de formula() deprecado
  suppressWarnings({
    result <- safe_multidim_plasticity(
      datos_demo,
      traits = "insect_size",
      environments = "temperature",
      groups = "insect_population"
    )
  })
  expect_true(inherits(result, "safe_multidim_result") ||
                inherits(result, "list") ||
                inherits(result, "data.frame"))
})
