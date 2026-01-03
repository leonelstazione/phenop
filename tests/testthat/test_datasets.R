# Tests for datasets in the package

test_that("demo_data has correct structure", {
  skip_if_not_installed("phenop")
  
  # Load dataset
  data(demo_data)
  
  # Test structure
  expect_s3_class(demo_data, "data.frame")
  expect_equal(ncol(demo_data), 8)
  expect_true(nrow(demo_data) > 0)
  
  # Check column names
  expected_cols <- c("insect_population", "fungus_strain", "temperature",
                     "humidity", "insect_size", "resistance",
                     "fungus_growth", "virulence")
  expect_named(demo_data, expected_cols)
  
  # Check data types
  expect_type(demo_data$insect_population, "character")
  expect_type(demo_data$fungus_strain, "character")
  expect_type(demo_data$temperature, "double")
  expect_type(demo_data$humidity, "double")
  expect_type(demo_data$insect_size, "double")
  expect_type(demo_data$resistance, "double")
  expect_type(demo_data$fungus_growth, "double")
  expect_type(demo_data$virulence, "double")
})

test_that("demo_data has reasonable values", {
  skip_if_not_installed("phenop")
  
  data(demo_data)
  
  # Check value ranges
  expect_true(all(demo_data$temperature >= 10 & demo_data$temperature <= 35))
  expect_true(all(demo_data$humidity >= 30 & demo_data$humidity <= 90))
  expect_true(all(demo_data$insect_size > 0))
  expect_true(all(demo_data$resistance > 0))
  expect_true(all(demo_data$fungus_growth > 0))
  expect_true(all(demo_data$virulence > 0))
  
  # Check for missing values
  expect_false(any(is.na(demo_data)))
})

test_that("dataset can be used in analysis functions", {
  skip_if_not_installed("phenop")
  
  data(demo_data)
  
  # Prepare data
  insect_data <- demo_data %>%
    dplyr::select(insect_population, temperature, humidity,
                  insect_size, resistance) %>%
    dplyr::distinct()
  
  # Should work without errors
  expect_error(
    safe_multidim_plasticity(
      insect_data,
      traits = c("insect_size", "resistance"),
      environments = c("temperature", "humidity"),
      groups = "insect_population"
    ),
    NA  # Expect no error
  )
})
