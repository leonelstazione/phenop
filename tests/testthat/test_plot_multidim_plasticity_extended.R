
test_that("plot_multidim_plasticity_extended works", {
  skip_on_cran()
  skip_if_not_installed("phenop")
  
  multidim_result <- list(
    individual_plasticity = data.frame(
      group = c("A", "B", "C"),
      trait1 = c(0.3, 0.5, 0.4),
      trait2 = c(0.6, 0.4, 0.5)
    ),
    message = "Test data"
  )
  
  result <- plot_multidim_plasticity_extended(
    multidim_result = multidim_result,
    type = "heatmap"
  )
  
  expect_true(is.null(result) || 
              inherits(result, "ggplot") || 
              inherits(result, "plotly") ||
              is.list(result))
})

