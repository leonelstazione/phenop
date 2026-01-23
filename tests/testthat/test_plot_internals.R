library(testthat)

test_that("plot_internals basic integration plot", {
  df <- data.frame(x = 1:5, y = 6:10)
  p <- plot_internals(df, type = "integration")
  expect_s3_class(p, "ggplot")
})

test_that("plot_internals basic radar plot", {
  df <- data.frame(x = 1:5, y = 6:10)
  p <- plot_internals(df, type = "radar")
  expect_s3_class(p, "ggplot")
})

test_that("plot_internals generates ggplot with NULL input", {
  p <- plot_internals(NULL, type = "integration")
  expect_s3_class(p, "ggplot")
})

test_that("plot_internals errors on invalid type", {
  expect_error(
    plot_internals(NULL, type = "invalid"),
    "Plot type not recognized"
  )
})


test_that("plot_internals requires ggplot2", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(plot_internals(NULL, type = "integration"), "ggplot")
})
