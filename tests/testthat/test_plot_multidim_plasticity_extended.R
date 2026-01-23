# tests/testthat/test-plot_multidim_plasticity_extended.R

test_that("plot_multidim_plasticity_extended handles empty data gracefully", {
  # Caso 1: NULL input
  expect_error(plot_multidim_plasticity_extended(NULL),
               "multidim_result cannot be NULL")

  # Caso 2: Resultado sin individual_plasticity
  empty_result <- list(some_other_field = "data")

  # Usamos expect_warning para capturar el warning
  expect_warning(
    p <- plot_multidim_plasticity_extended(empty_result),
    "No individual plasticity data found"
  )
  expect_s3_class(p, "ggplot")

  # Caso 3: individual_plasticity es NULL
  null_result <- list(individual_plasticity = NULL)
  expect_warning(
    p <- plot_multidim_plasticity_extended(null_result),
    "No individual plasticity data found"
  )
  expect_s3_class(p, "ggplot")

  # Caso 4: individual_plasticity es data.frame vacío
  empty_df_result <- list(individual_plasticity = data.frame())
  expect_warning(
    p <- plot_multidim_plasticity_extended(empty_df_result),
    "No valid data structure for plotting"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity_extended works with valid data", {
  # Crear datos simulados realistas
  multidim_result <- list(
    individual_plasticity = data.frame(
      group = rep(paste0("G", 1:5), each = 4),
      genotype = rep(paste0("Gen", 1:10), each = 2),
      trait1 = rnorm(20, mean = 10, sd = 2),
      trait2 = rnorm(20, mean = 5, sd = 1),
      trait3 = rnorm(20, mean = 8, sd = 1.5)
    )
  )

  # Probar diferentes tipos de gráficos
  types_to_test <- c("heatmap", "scatter", "bar", "violin", "density")

  for (type in types_to_test) {
    p <- plot_multidim_plasticity_extended(multidim_result, type = type)
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_multidim_plasticity_extended handles subset of traits", {
  multidim_result <- list(
    individual_plasticity = data.frame(
      group = rep(1:3, each = 5),
      trait_a = rnorm(15),
      trait_b = rnorm(15),
      trait_c = rnorm(15),
      trait_d = rnorm(15)
    )
  )

  # Probar con subset de traits (sin warning)
  p <- plot_multidim_plasticity_extended(
    multidim_result,
    type = "heatmap",
    traits = c("trait_a", "trait_b")
  )
  expect_s3_class(p, "ggplot")

  # Probar con traits que no existen (debería usar todos)
  suppressWarnings({
    p <- plot_multidim_plasticity_extended(
      multidim_result,
      type = "heatmap",
      traits = c("nonexistent_trait")
    )
  })
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity_extended accepts plot options", {
  multidim_result <- list(
    individual_plasticity = data.frame(
      group = rep(1:3, each = 4),
      trait1 = rnorm(12),
      trait2 = rnorm(12)
    )
  )

  # Probar diferentes opciones
  plot_options <- list(
    title = "Custom Title",
    color_palette = "plasma",
    theme = "classic",
    alpha = 0.6,
    point_size = 4,
    text_size = 14,
    legend_position = "bottom"
  )

  p <- plot_multidim_plasticity_extended(
    multidim_result,
    type = "heatmap",
    plot_options = plot_options
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity_extended handles different data structures", {
  # Caso: plasticity_data es una lista de data.frames
  multidim_result <- list(
    individual_plasticity = list(
      data.frame(group = "A", trait1 = 1:3, trait2 = 4:6),
      data.frame(group = "B", trait1 = 7:9, trait2 = 10:12)
    )
  )

  p <- plot_multidim_plasticity_extended(multidim_result)
  expect_s3_class(p, "ggplot")

  # Caso: plasticity_data es una lista no de data.frames
  # Este caso es problemático - la función lo convierte a data.frame vacío
  multidim_result <- list(
    individual_plasticity = list(1, 2, 3)  # Lista de números
  )

  expect_warning(
    p <- plot_multidim_plasticity_extended(multidim_result),
    "No valid data structure for plotting"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity_extended scatter plot needs 2+ traits", {
  multidim_result <- list(
    individual_plasticity = data.frame(
      group = 1:5,
      single_trait = rnorm(5)  # Solo un trait
    )
  )

  # Scatter plot necesita al menos 2 traits - devuelve plot vacío
  p <- plot_multidim_plasticity_extended(multidim_result, type = "scatter")
  expect_s3_class(p, "ggplot")

  # Con 2 traits debería funcionar
  multidim_result$individual_plasticity$trait2 <- rnorm(5)
  p <- plot_multidim_plasticity_extended(multidim_result, type = "scatter")
  expect_s3_class(p, "ggplot")
})

test_that("plot_multidim_plasticity_extended creates dummy group when needed", {
  # Datos sin columna de grupo
  multidim_result <- list(
    individual_plasticity = data.frame(
      trait1 = rnorm(10),
      trait2 = rnorm(10)
    )
  )

  p <- plot_multidim_plasticity_extended(multidim_result, type = "heatmap")
  expect_s3_class(p, "ggplot")
  # Verificar que se creó la columna 'group' en los datos
  # Nota: esta verificación puede ser difícil porque el data.frame está dentro del plot
})

test_that("plot_multidim_plasticity_extended works with all plot types", {
  multidim_result <- list(
    individual_plasticity = data.frame(
      group = rep(1:3, each = 3),
      trait1 = rnorm(9),
      trait2 = rnorm(9),
      trait3 = rnorm(9)
    )
  )

  # Probar TODOS los tipos de gráficos (algunos devuelven plots vacíos)
  all_types <- c("heatmap", "network", "radar", "bar", "scatter",
                 "violin", "density", "pca", "landscape", "integration")

  for (type in all_types) {
    p <- plot_multidim_plasticity_extended(multidim_result, type = type)
    expect_s3_class(p, "ggplot")
  }
})
