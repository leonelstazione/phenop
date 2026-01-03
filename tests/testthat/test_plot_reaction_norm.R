# Tests for plot_reaction_norm function

test_that("plot_reaction_norm creates reaction norm plots", {
  skip_on_cran()
  skip_if_not_installed("phenop")
  skip_if_not_installed("ggplot2")

  # Create test data
  test_data <- data.frame(
    gen = rep(c("A", "B", "C"), each = 4),
    env = rep(1:3, times = 4),
    trait = c(10, 12, 14,  # Genotype A
              8, 11, 13,   # Genotype B
              9, 10, 12,   # Genotype C
              11, 13, 15)  # Genotype A again
  )

  # Test basic plot
  plot_result <- plot_reaction_norm(
    data = test_data,
    environment = env,
    trait = trait,
    genotype = gen
  )

  expect_s3_class(plot_result, "ggplot")

  # Test with points and SE
  plot_with_se <- plot_reaction_norm(
    test_data,
    env,
    trait,
    gen,
    add_points = TRUE,
    add_se = TRUE
  )

  expect_s3_class(plot_with_se, "ggplot")
})

test_that("plot_reaction_norm handles missing data", {
  skip_if_not_installed("phenop")
  skip_if_not_installed("ggplot2")

  test_data <- data.frame(
    gen = c("A", "B", "A", "B"),
    env = c(1, 1, 2, 2),
    trait = c(10, NA, 12, 11)
  )

  # Should handle NAs gracefully
  expect_error(
    plot_reaction_norm(test_data, env, trait, gen),
    NA
  )
})
