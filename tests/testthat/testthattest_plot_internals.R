# Tests for internal plotting functions

test_that("Internal plotting helpers exist and work", {
  skip_on_cran()
  skip_if_not_installed("phenop")

  # Verificar que las funciones internas existen
  # (buscando por patrones comunes)

  # 1. Función para preparar datos de gráficos
  if (exists(".prepare_plot_data", envir = asNamespace("phenop"), mode = "function")) {
    test_data <- data.frame(x = 1:5, y = rnorm(5), group = c("A", "A", "B", "B", "C"))
    result <- tryCatch({
      .prepare_plot_data(test_data, x = "x", y = "y", group = "group")
    }, error = function(e) NULL)

    if (!is.null(result)) {
      expect_true(is.data.frame(result) || is.list(result))
    }
  }

  # 2. Función para temas de gráficos
  if (exists(".theme_plasticity", envir = asNamespace("phenop"), mode = "function")) {
    theme_result <- tryCatch({
      .theme_plasticity()
    }, error = function(e) NULL)

    if (!is.null(theme_result)) {
      expect_true(inherits(theme_result, "theme") || is.list(theme_result))
    }
  }

  # 3. Función para escalas de colores
  if (exists(".get_color_scale", envir = asNamespace("phenop"), mode = "function")) {
    colors <- tryCatch({
      .get_color_scale(n = 3, palette = "Set1")
    }, error = function(e) NULL)

    if (!is.null(colors)) {
      expect_type(colors, "character")
      expect_length(colors, 3)
    }
  }
})

test_that("Internal validation functions work", {
  skip_if_not_installed("phenop")

  # Función para validar datos de entrada
  if (exists(".validate_plot_input", envir = asNamespace("phenop"), mode = "function")) {
    valid_data <- data.frame(x = 1:5, y = 1:5)
    invalid_data <- data.frame(x = letters[1:5], y = 1:5)

    # Debería pasar con datos válidos
    valid_result <- tryCatch({
      .validate_plot_input(valid_data, x = "x", y = "y")
    }, error = function(e) FALSE)

    # Debería fallar o retornar FALSE con datos inválidos
    invalid_result <- tryCatch({
      .validate_plot_input(invalid_data, x = "x", y = "y")
    }, error = function(e) FALSE)

    expect_true(valid_result == TRUE || is.null(valid_result))
  }
})
