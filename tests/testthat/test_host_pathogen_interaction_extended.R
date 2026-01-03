
test_that("host_pathogen_interaction_extended works", {
  skip_on_cran()
  skip_if_not_installed("phenop")
  
  # Datos válidos
  host_data <- data.frame(
    host_id = rep(1:3, each = 2),
    time = rep(1:2, times = 3),
    environment = rep(c("E1", "E2"), times = 3),
    trait = rnorm(6, 10, 2)
  )
  
  pathogen_data <- data.frame(
    pathogen_id = rep(1:3, each = 2),
    time = rep(1:2, times = 3),
    environment = rep(c("E1", "E2"), times = 3),
    virulence = rnorm(6, 8, 1.5)
  )
  
  result <- host_pathogen_interaction_extended(
    host_data = host_data,
    pathogen_data = pathogen_data,
    host_traits = "trait",
    pathogen_traits = "virulence",
    time_var = "time",
    environments = "environment"
  )
  
  expect_type(result, "list")
})

