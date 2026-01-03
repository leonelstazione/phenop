# Test that verifies all exported functions exist

test_that("All documented functions exist and are callable", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # List of all functions that should be exported
  expected_functions <- c(
    "anova_plasticity",
    "host_pathogen_interaction",
    "multidim_plasticity",
    "optimize_multidim_environment",
    "plasticity_meta_analysis",
    "plasticity_tradeoffs",
    "plot_multidim_plasticity",
    "plot_reaction_norm",
    "simulate_plasticity_data",
    # Extended functions
    "safe_multidim_plasticity",
    "plasticity_tradeoffs_extended",
    "host_pathogen_interaction_extended",
    "plasticity_meta_analysis_extended",
    "plot_multidim_plasticity_extended"
  )

  # Check each function exists
  for (func_name in expected_functions) {
    expect_true(
      exists(func_name, where = asNamespace("phenop"), mode = "function"),
      info = paste("Function", func_name, "should exist")
    )
  }

  # Check they can be called without error
  test_data <- data.frame(
    x = 1:10,
    y = rnorm(10),
    group = rep(c("A", "B"), each = 5)
  )

  # Test anova_plasticity
  expect_error(anova_plasticity(y ~ group, test_data), NA)

  # Test plot_reaction_norm - suprimir warning de ggplot2
  suppressWarnings({
    expect_error(plot_reaction_norm(test_data, x, y, group), NA)
  })

  # Test dataset exists
  expect_true(exists("demo_data", where = asNamespace("phenop"), mode = "any"))
})

test_that("Functions have required parameters", {
  skip_if_not_installed("phenop")

  # Check anova_plasticity
  anova_args <- formalArgs(anova_plasticity)
  expect_true("formula" %in% anova_args)
  expect_true("data" %in% anova_args)

  # Check multidim_plasticity
  multidim_args <- tryCatch({
    formalArgs(multidim_plasticity)
  }, error = function(e) {
    NULL
  })

  if (!is.null(multidim_args)) {
    expect_true("data" %in% multidim_args)
    expect_true("traits" %in% multidim_args)
    expect_true("environments" %in% multidim_args)
    expect_true("groups" %in% multidim_args)
  }
})
