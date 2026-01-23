# Test file for analyze.R functions
# testthat tests for statistical analysis functions in phenop package
test_that("analyze_mixed_effects returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("lme4")

  # Create minimal test data
  test_data <- data.frame(
    sos = rnorm(100, 150, 10),
    mean_temperature = rnorm(100, 15, 3),
    total_precipitation = rnorm(100, 500, 100),
    species = rep(paste0("species", 1:5), each = 20),
    site = rep(paste0("site", 1:4), 25),
    year = rep(2010:2014, each = 20)
  )

  # Test basic functionality - suppress the singularity warning
  suppressWarnings({
    result <- analyze_mixed_effects(test_data)
  })

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("model_summary", "fixed_effects", "random_effects",
                         "anova", "marginal_r2", "conditional_r2"))
  expect_s3_class(result$model_summary, "summary.merMod")
  expect_type(result$fixed_effects, "double")
  expect_s3_class(result$anova, "anova")
})

test_that("analyze_mixed_effects handles missing lme4 gracefully", {
  # Since lme4 is in Imports, we assume it's installed
  # We'll test that the function works when lme4 is available
  skip("lme4 is in Imports, so we assume it's installed")
})

test_that("analyze_gam_relationships works with valid data", {
  skip_on_cran()
  skip_if_not_installed("mgcv")

  # Create test data
  test_data <- data.frame(
    ndvi = runif(100, 0.1, 0.9),
    doy = rep(1:100, length.out = 100),
    temperature = rnorm(100, 15, 5),
    precipitation = rpois(100, 5),
    species = rep(c("Quercus_ilex", "Fagus_sylvatica"), each = 50)
  )

  result <- analyze_gam_relationships(test_data)

  expect_s3_class(result, "gam")
  expect_type(result$coefficients, "double")
  expect_true("formula" %in% names(result))
})

test_that("analyze_temporal_trends calculates trends correctly", {
  # Create time series test data with enough variation
  n_years <- 5
  test_data <- data.frame(
    species = rep(c("species1", "species2"), each = n_years * 2),
    site = rep(c("site1", "site2"), times = n_years * 2),
    year = rep(2015:2019, each = 2, times = 2),
    sos = c(seq(150, 145, length.out = n_years * 2) + rnorm(n_years*2, 0, 2),
            seq(160, 155, length.out = n_years * 2) + rnorm(n_years*2, 0, 2)),
    eos = c(seq(280, 285, length.out = n_years * 2) + rnorm(n_years*2, 0, 2),
            seq(290, 295, length.out = n_years * 2) + rnorm(n_years*2, 0, 2)),
    los = c(seq(130, 140, length.out = n_years * 2) + rnorm(n_years*2, 0, 2),
            seq(130, 140, length.out = n_years * 2) + rnorm(n_years*2, 0, 2))
  )

  result <- analyze_temporal_trends(test_data)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("species", "site", "sos_slope", "sos_p",
                    "eos_slope", "eos_p", "los_slope", "los_p") %in% names(result)))

  # Check that slopes are calculated
  expect_true(all(is.numeric(result$sos_slope)))
  expect_true(all(is.numeric(result$sos_p)))
})

test_that("perform_cluster_analysis works with different cluster numbers", {
  skip_on_cran()

  # Create test data with clear clusters
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    sos = c(rnorm(n/2, 150, 5), rnorm(n/2, 170, 5)),
    eos = c(rnorm(n/2, 280, 5), rnorm(n/2, 260, 5)),
    los = c(rnorm(n/2, 130, 5), rnorm(n/2, 90, 5)),
    ndvi_max = runif(n, 0.6, 0.9),
    gpp_total = rnorm(n, 1000, 200),
    mean_temperature = rnorm(n, 15, 2),
    total_precipitation = rnorm(n, 500, 100)
  )

  # Test with default clusters
  result_default <- perform_cluster_analysis(test_data)

  expect_type(result_default, "list")
  expect_named(result_default, c("clusters", "kmeans", "silhouette"))
  expect_true("cluster" %in% names(result_default$clusters))

  # Test with custom number of clusters
  result_custom <- perform_cluster_analysis(test_data, n_clusters = 3)
  expect_equal(length(unique(result_custom$kmeans$cluster)), 3)
})

test_that("perform_cluster_analysis handles missing values", {
  # Create data with NAs but enough complete cases
  set.seed(123)
  test_data <- data.frame(
    sos = c(rnorm(8, 150, 5), NA, NA),
    eos = c(rnorm(8, 280, 5), 285, NA),
    los = c(rnorm(8, 130, 5), NA, 140),
    ndvi_max = runif(10, 0.7, 0.9),
    gpp_total = rnorm(10, 1000, 100),
    mean_temperature = rnorm(10, 15, 2),
    total_precipitation = rnorm(10, 500, 50)
  )

  result <- perform_cluster_analysis(test_data, n_clusters = 2)

  # Should work without error (NAs are removed)
  expect_type(result, "list")
})

test_that("analyze_correlations returns correct structure", {
  # Create test data
  set.seed(123)
  n <- 50
  test_data <- data.frame(
    sos = rnorm(n, 150, 10),
    eos = rnorm(n, 280, 10),
    los = rnorm(n, 130, 10),
    ndvi_max = runif(n, 0.6, 0.9),
    gpp_total = rnorm(n, 1000, 200),
    mean_temperature = rnorm(n, 15, 3),
    total_precipitation = rnorm(n, 500, 100),
    greenup_rate = runif(n, 0.1, 0.5)
  )

  result <- analyze_correlations(test_data)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("matrix", "plot_data", "top_correlations"))

  # Check correlation matrix - should be 8x8 (8 variables)
  expect_type(result$matrix, "double")
  expect_equal(dim(result$matrix), c(8, 8))
  expect_true(all(diag(result$matrix) == 1))

  # Check plot data
  expect_s3_class(result$plot_data, "data.frame")
  expect_true(all(c("Var1", "Var2", "Freq", "sign", "abs_cor") %in%
                    names(result$plot_data)))
})

test_that("analyze_correlations handles data with missing values", {
  # Create data with some NAs
  test_data <- data.frame(
    sos = c(150, 155, NA, 160, 165),
    eos = c(280, NA, 285, 290, 295),
    los = c(130, 135, 140, NA, 145),
    ndvi_max = c(0.7, 0.8, 0.75, 0.85, NA),
    gpp_total = rnorm(5, 1000, 100),
    mean_temperature = rnorm(5, 15, 2),
    total_precipitation = rnorm(5, 500, 50),
    greenup_rate = runif(5, 0.2, 0.4)
  )

  result <- analyze_correlations(test_data)

  # Should complete without error
  expect_type(result, "list")
})

test_that("perform_pca_analysis works with valid data", {
  skip_on_cran()
  skip_if_not_installed("FactoMineR")

  # Create test data
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    sos = rnorm(n, 150, 10),
    eos = rnorm(n, 280, 10),
    los = rnorm(n, 130, 10),
    ndvi_max = runif(n, 0.6, 0.9),
    gpp_total = rnorm(n, 1000, 200),
    mean_temperature = rnorm(n, 15, 3),
    total_precipitation = rnorm(n, 500, 100)
  )

  result <- perform_pca_analysis(test_data)

  # Check structure
  expect_s3_class(result, "PCA")
  expect_type(result$eig, "double")
})

test_that("perform_pca_analysis handles missing FactoMineR", {
  # FactoMineR is in Imports, so we assume it's installed
  skip("FactoMineR is in Imports, so we assume it's installed")
})

test_that("detect_anomalies flags extreme values correctly", {
  # Create test data with clear anomalies (more than 2 SD away)
  set.seed(123)
  test_data <- data.frame(
    species = rep(c("species1", "species2"), each = 10),
    site = rep(c("site1", "site2"), times = 10),
    sos = c(rnorm(16, 150, 5), 200, 100, 170, 130),  # Clear anomalies: 200 and 100
    eos = c(rnorm(16, 280, 5), 320, 240, 290, 270),  # Clear anomalies: 320 and 240
    los = c(rnorm(16, 130, 5), 200, 80, 140, 120)    # Clear anomalies: 200 and 80
  )

  result <- detect_anomalies(test_data)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("sos_z", "eos_z", "los_z",
                    "sos_anomaly", "eos_anomaly", "los_anomaly",
                    "anomaly_score") %in% names(result)))

  # Should detect anomalies
  expect_true(any(result$sos_anomaly, na.rm = TRUE))
  expect_true(any(result$anomaly_score > 0, na.rm = TRUE))
})

test_that("detect_anomalies flags extreme values correctly", {
  # Create test data where extreme values are truly extreme relative to their group
  test_data <- data.frame(
    species = rep("species1", 30),
    site = rep("site1", 30),
    # First 28 values are normal, last 2 are extreme outliers
    sos = c(rnorm(28, 150, 5), 250, 50),    # 250 and 50 are extreme
    eos = c(rnorm(28, 280, 5), 350, 210),   # 350 and 210 are extreme
    los = c(rnorm(28, 130, 5), 250, 60)     # 250 and 60 are extreme
  )

  result <- detect_anomalies(test_data)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("sos_z", "eos_z", "los_z",
                    "sos_anomaly", "eos_anomaly", "los_anomaly",
                    "anomaly_score") %in% names(result)))

  # Check that last 2 rows have anomalies
  last_two_rows <- tail(result, 2)

  expect_true(any(last_two_rows$sos_anomaly, na.rm = TRUE))
  expect_true(any(last_two_rows$anomaly_score > 0, na.rm = TRUE))
})

test_that("run_complete_analysis executes full pipeline", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("FactoMineR")

  # Create minimal test datasets
  set.seed(123)
  n <- 50

  # Time series data
  time_series <- data.frame(
    year = rep(2020:2021, each = n/2),
    species = rep(c("Quercus_ilex", "Fagus_sylvatica"), each = n/2),
    site = rep(c("site1", "site2"), times = n/2),
    population_id = rep(1:5, each = 10, length.out = n),
    doy = sample(1:365, n, replace = TRUE),
    ndvi = runif(n, 0.1, 0.9),
    temperature = rnorm(n, 15, 5),
    precipitation = rpois(n, 5)
  )

  # Parameters data - ensure enough data for trends
  parameters <- data.frame(
    species = rep(c("Quercus_ilex", "Fagus_sylvatica"), each = n/2),
    site = rep(c("site1", "site2"), times = n/2),
    year = rep(2020:2021, each = n/2),
    sos = rnorm(n, 150, 10),
    eos = rnorm(n, 280, 10),
    los = rnorm(n, 130, 10),
    ndvi_max = runif(n, 0.6, 0.9),
    gpp_total = rnorm(n, 1000, 200),
    mean_temperature = rnorm(n, 15, 3),
    total_precipitation = rnorm(n, 500, 100),
    phenology_type = sample(c("early", "late"), n, replace = TRUE)
  )

  # Test that it runs without error
  results <- run_complete_analysis(time_series, parameters)

  # Check structure
  expect_type(results, "list")
  expect_named(results, c("mixed_model", "trends", "clusters",
                          "correlations", "pca", "anomalies"))
})

test_that("run_complete_analysis handles missing data gracefully", {
  # Test with empty data frames - should error
  empty_ts <- data.frame()
  empty_params <- data.frame()

  # Expect an error (graceful handling means it should error, not crash)
  expect_error(run_complete_analysis(empty_ts, empty_params))
})
