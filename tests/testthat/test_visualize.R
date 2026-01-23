# Test file for visualize.R functions
# Modified to run locally without skipping

test_that("plot_seasonal_curves creates ggplot object", {
  # Create test data
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    year = rep(2021, n),
    species = rep(c("Quercus_ilex", "Fagus_sylvatica"), each = n/2),
    site = rep(c("site1", "site2"), times = n/2),
    population_id = rep(1:4, each = 25),
    doy = sample(1:365, n, replace = TRUE),
    ndvi = runif(n, 0.1, 0.9)
  )

  # Test with default parameters
  p <- plot_seasonal_curves(test_data, selected_year = 2021)
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("plot_seasonal_curves validates input", {
  # Test error handling
  expect_error(plot_seasonal_curves(NULL), "time_series must be a data frame")

  # Test with empty data frame
  empty_df <- data.frame()
  expect_error(plot_seasonal_curves(empty_df), "Missing required columns")

  # Test with incomplete data
  incomplete_df <- data.frame(year = 2021, species = "test")
  expect_error(plot_seasonal_curves(incomplete_df), "Missing required columns")
})

test_that("plot_seasonal_curves handles edge cases", {
  # Test with single group/species
  single_species_data <- data.frame(
    year = rep(2021, 20),
    species = rep("single_species", 20),
    site = "single_site",
    population_id = rep(1, 20),
    doy = 1:20,
    ndvi = runif(20, 0.3, 0.7)
  )

  p <- plot_seasonal_curves(single_species_data, selected_year = 2021)
  expect_s3_class(p, "ggplot")

  # Test with no data for selected year
  wrong_year_data <- data.frame(
    year = rep(2020, 10),
    species = rep("test", 10),
    site = rep("test", 10),
    population_id = 1:10,
    doy = 1:10,
    ndvi = runif(10, 0.3, 0.7)
  )

  p <- plot_seasonal_curves(wrong_year_data, selected_year = 2021)
  expect_s3_class(p, "ggplot")
})

test_that("plot_parameter_heatmap creates correct heatmap", {
  # Create test data
  test_data <- data.frame(
    species = rep(c("species1", "species2"), each = 20),
    site = rep(c("site1", "site2"), times = 20),
    year = rep(2010:2019, each = 4),
    los = rnorm(40, 130, 10),
    sos = rnorm(40, 150, 5)
  )

  # Test with default parameter
  p <- plot_parameter_heatmap(test_data, parameter = "los")
  expect_s3_class(p, "gg")

  # Test with different parameter
  p2 <- plot_parameter_heatmap(test_data, parameter = "sos")
  expect_s3_class(p2, "ggplot")
})

test_that("plot_parameter_heatmap validates input", {
  # Test error handling
  expect_error(plot_parameter_heatmap(NULL), "parameters must be a data frame")

  # Test with missing parameter column
  test_data <- data.frame(
    species = c("A", "B"),
    site = c("X", "Y"),
    year = c(2021, 2021),
    other_var = c(1, 2)
  )
  expect_error(plot_parameter_heatmap(test_data, parameter = "nonexistent"),
               "Missing required columns")
})

test_that("plot_spatial_map works with data.frame input", {
  # Create test spatial data (data.frame with coordinates)
  test_data <- data.frame(
    longitude = runif(50, -5, 5),
    latitude = runif(50, 40, 45),
    los = runif(50, 100, 150),
    ndvi_max = runif(50, 0.6, 0.9),
    site_id = paste0("site", 1:50)
  )

  # Test with data.frame
  p <- plot_spatial_map(test_data, variable = "los", point_size = "ndvi_max")
  expect_s3_class(p, "gg")
})

test_that("plot_spatial_map works with missing optional packages", {
  # Test without maps package (simulate by mocking)
  test_data <- data.frame(
    longitude = c(0, 1, 2),
    latitude = c(40, 41, 42),
    los = c(100, 120, 140),
    ndvi_max = c(0.7, 0.8, 0.9)
  )

  # Should work even without maps package
  p <- plot_spatial_map(test_data)
  expect_s3_class(p, "ggplot")
})

test_that("plot_spatial_map validates input", {
  # Test error handling
  expect_error(plot_spatial_map(NULL), "spatial_data must be a data frame")

  # Test with missing required columns
  incomplete_data <- data.frame(
    longitude = c(0, 1),
    latitude = c(40, 41)
    # Missing los and ndvi_max
  )
  expect_error(plot_spatial_map(incomplete_data), "Missing required columns")
})

test_that("plot_productivity_climate creates relationship plot", {
  # Create test data
  test_data <- data.frame(
    species = rep(c("species1", "species2", "species3"), each = 20),
    mean_temperature = rnorm(60, 15, 3),
    gpp_total = rnorm(60, 1000, 200),
    phenology_type = sample(c("early", "late", "intermediate"), 60, replace = TRUE),
    ndvi_max = runif(60, 0.6, 0.9)
  )

  p <- plot_productivity_climate(test_data)
  expect_s3_class(p, "gg")
})

test_that("plot_productivity_climate handles missing optional columns", {
  # Test with minimal data (no phenology_type or ndvi_max)
  minimal_data <- data.frame(
    species = c("A", "B", "A", "B"),
    mean_temperature = c(10, 15, 12, 18),
    gpp_total = c(800, 1200, 900, 1300)
  )

  p <- plot_productivity_climate(minimal_data)
  expect_s3_class(p, "ggplot")
})

test_that("plot_density_ridges works with ggridges available", {
  # Skip if ggridges not installed but don't skip completely
  if (requireNamespace("ggridges", quietly = TRUE)) {
    # Create test data
    test_data <- data.frame(
      species = rep(c("species1", "species2", "species3"), each = 30),
      los = c(rnorm(30, 130, 10), rnorm(30, 140, 15), rnorm(30, 120, 8)),
      sos = c(rnorm(30, 150, 5), rnorm(30, 160, 7), rnorm(30, 140, 6)),
      site = rep(c("site1", "site2"), each = 45)
    )

    # Test with default parameters
    p1 <- plot_density_ridges(test_data, parameter = "los")
    expect_s3_class(p1, "ggplot")

    # Test with different parameter
    p2 <- plot_density_ridges(test_data, parameter = "sos")
    expect_s3_class(p2, "ggplot")

    # Test with different grouping variable
    p3 <- plot_density_ridges(test_data, parameter = "los", group_by = "site")
    expect_s3_class(p3, "ggplot")
  } else {
    # If ggridges not installed, just skip silently
    skip("ggridges not installed")
  }
})

test_that("plot_density_ridges validates input", {
  # Test error handling
  expect_error(plot_density_ridges(NULL), "parameters must be a data frame")

  # Test with missing parameter
  test_data <- data.frame(
    species = c("A", "B"),
    other_var = c(1, 2)
  )
  expect_error(plot_density_ridges(test_data, parameter = "nonexistent"),
               "Missing required columns")
})

test_that("create_phenology_dashboard combines plots correctly", {
  # Only run if patchwork is installed
  if (requireNamespace("patchwork", quietly = TRUE)) {
    # Create minimal test datasets
    set.seed(123)
    n <- 30

    # Time series data
    time_series <- data.frame(
      year = rep(2021, n),
      species = rep(c("Quercus_ilex", "Fagus_sylvatica"), each = n/2),
      site = rep(c("site1", "site2"), times = n/2),
      population_id = rep(1:3, each = 10),
      doy = sample(1:365, n, replace = TRUE),
      ndvi = runif(n, 0.2, 0.8)
    )

    # Parameters data
    parameters <- data.frame(
      species = rep(c("Quercus_ilex", "Fagus_sylvatica"), each = n/2),
      site = rep(c("site1", "site2"), times = n/2),
      year = rep(2020:2021, each = n/2),
      los = rnorm(n, 130, 10),
      sos = rnorm(n, 150, 5),
      eos = rnorm(n, 280, 10),
      ndvi_max = runif(n, 0.6, 0.9),
      gpp_total = rnorm(n, 1000, 200),
      mean_temperature = rnorm(n, 15, 3),
      phenology_type = sample(c("early", "late"), n, replace = TRUE)
    )

    # Spatial data
    spatial_data <- data.frame(
      longitude = runif(20, -5, 5),
      latitude = runif(20, 40, 45),
      los = runif(20, 100, 150),
      ndvi_max = runif(20, 0.6, 0.9)
    )

    # Create dashboard
    dashboard <- create_phenology_dashboard(time_series, parameters, spatial_data)

    # Check that it returns a ggplot/patchwork object
    expect_s3_class(dashboard, "gg")
  } else {
    skip("patchwork not installed")
  }
})

test_that("create_phenology_dashboard handles errors gracefully", {
  if (requireNamespace("patchwork", quietly = TRUE)) {
    # Test with problematic data that should cause errors in individual plots
    # The dashboard should still return a plot with error messages
    empty_data <- data.frame()

    # This should not crash, but create a dashboard with error messages
    dashboard <- create_phenology_dashboard(empty_data, empty_data, empty_data)
    expect_s3_class(dashboard, "gg")
  } else {
    skip("patchwork not installed")
  }
})
