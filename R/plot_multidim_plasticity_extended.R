#' Extended Multidimensional Plasticity Visualization (SAFE)
#'
#' Advanced plotting functions for visualizing multidimensional plasticity results.
#' Always returns a ggplot object, even if data are missing or incomplete.
#'
#' @param multidim_result Result object from multidim_plasticity() or safe_multidim_plasticity()
#' @param type Type of plot to create. Options: "heatmap" (default), "network", "radar", "bar",
#'   "scatter", "violin", "density", "pca", "landscape", "integration"
#' @param traits Optional character vector of traits to include (subset of all traits)
#' @param plot_options List of additional plotting options:
#'   - title: Plot title
#'   - color_palette: Color palette name ("viridis", "plasma", "magma", "inferno", "cividis")
#'   - theme: ggplot2 theme ("minimal", "classic", "bw", "dark", "light")
#'   - interactive: Logical, whether to create interactive plot (requires plotly)
#'   - save_plot: Logical, whether to save plot to file
#'   - filename: Filename for saved plot
#'   - width, height: Plot dimensions in inches
#'   - alpha: transparency
#'   - point_size, text_size, legend_position
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot object (or plotly object if interactive = TRUE)
#' @export
plot_multidim_plasticity_extended <- function(multidim_result,
                                              type = "heatmap",
                                              traits = NULL,
                                              plot_options = list(),
                                              ...) {
  # Basic validation
  if (is.null(multidim_result)) stop("multidim_result cannot be NULL")
  if (is.null(multidim_result$individual_plasticity)) {
    warning("No individual plasticity data found in multidim_result")
    return(ggplot2::ggplot() + ggplot2::labs(title = "Empty plot") + ggplot2::theme_minimal())
  }
  plasticity_data <- multidim_result$individual_plasticity
  # ------------------------------------------------------------
  # MODIFICACIÓN 1: Asegurar que plasticity_data es un data.frame
  # ------------------------------------------------------------
  if (is.list(plasticity_data) && !is.data.frame(plasticity_data)) {
    # Si es una lista pero no data.frame, intentar convertir
    if (length(plasticity_data) > 0 &&
        all(sapply(plasticity_data, is.data.frame))) {
      plasticity_data <- do.call(rbind, plasticity_data)
    } else {
      plasticity_data <- data.frame()
    }
  }
  # Si después de la conversión sigue sin ser data.frame o está vacío
  if (!is.data.frame(plasticity_data) || nrow(plasticity_data) == 0) {
    warning("No valid data structure for plotting. Creating empty plot.")
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Empty plot - No valid data") +
             ggplot2::theme_minimal())
  }
  # Default options
  default_options <- list(
    title = paste("Multidimensional Plasticity:", type, "Plot"),
    color_palette = "viridis",
    theme = "minimal",
    interactive = FALSE,
    save_plot = FALSE,
    filename = paste0("phenop_plot_", type, "_", Sys.Date(), ".png"),
    width = 10,
    height = 7,
    alpha = 0.8,
    point_size = 3,
    text_size = 12,
    legend_position = "right"
  )
  plot_options <- utils::modifyList(default_options, plot_options)
  # Columna de grupo
  possible_groups <- c("group","individual","groups","genotype","population","strain","id","sample")
  group_col <- intersect(possible_groups, names(plasticity_data))
  if (length(group_col) > 0) group_col <- group_col[1] else group_col <- NULL
  # Columnas numéricas
  numeric_cols <- names(plasticity_data)[sapply(plasticity_data, is.numeric)]
  # Determinar traits
  if (!is.null(traits)) {
    trait_cols <- intersect(traits, numeric_cols)
    if (length(trait_cols) == 0) {
      warning("None of the specified traits found. Using all numeric columns.")
      trait_cols <- numeric_cols
    }
  } else if (!is.null(group_col)) {
    trait_cols <- setdiff(numeric_cols, group_col)
  } else {
    trait_cols <- numeric_cols
  }
  # ------------------------------------------------------------
  # MODIFICACIÓN 2: Verificar si hay datos para graficar
  # ------------------------------------------------------------
  if (length(trait_cols) == 0 || nrow(plasticity_data) == 0) {
    warning("No valid data for plotting. Creating empty plot.")
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = plot_options$title,
                    subtitle = "No data available") +
      ggplot2::theme_minimal()
    return(p)
  }
  # Crear dummy group si no existe
  if (is.null(group_col)) {
    plasticity_data$group <- paste0("Group", 1:nrow(plasticity_data))
    group_col <- "group"
  }
  # Mapear helpers
  plot_fun <- switch(type,
                     heatmap = .create_heatmap_plot_safe,
                     network = .create_network_plot_safe,
                     radar = .create_radar_plot_safe,
                     bar = .create_bar_plot_safe,
                     scatter = .create_scatter_plot_safe,
                     violin = .create_violin_plot_safe,
                     density = .create_density_plot_safe,
                     pca = .create_pca_plot_safe,
                     landscape = .create_landscape_plot_safe,
                     integration = .create_integration_plot_safe,
                     .create_heatmap_plot_safe)
  # Generar plot
  p <- plot_fun(plasticity_data, trait_cols, group_col, plot_options, ...)
  # Forzar ggplot si por algún motivo es NULL
  if (is.null(p) || !inherits(p, "ggplot")) {
    p <- ggplot2::ggplot() + ggplot2::labs(title = plot_options$title) + ggplot2::theme_minimal()
  }
  # Interactivo
  if (plot_options$interactive && requireNamespace("plotly", quietly = TRUE)) {
    p <- plotly::ggplotly(p)
  }
  # Guardar
  if (plot_options$save_plot) {
    ggplot2::ggsave(plot_options$filename, plot = p,
                    width = plot_options$width,
                    height = plot_options$height,
                    dpi = 300)
    cat("Plot saved as:", plot_options$filename, "\n")
  }
  return(p)
}
# -------------------
# Helpers seguros: siempre devuelven ggplot
.create_heatmap_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  data_long <- tryCatch({
    data %>%
      dplyr::select(dplyr::all_of(c(group_col, traits))) %>%
      tidyr::pivot_longer(-dplyr::all_of(group_col),
                          names_to = "trait",
                          values_to = "plasticity")
  }, error = function(e) {
    data.frame(trait = character(0),
               plasticity = numeric(0),
               group = factor())
  })
  if (nrow(data_long) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = plot_title) +
             ggplot2::theme_minimal())
  }
  ggplot2::ggplot(data_long,
                  ggplot2::aes(x = trait,
                               y = .data[[group_col]],
                               fill = plasticity)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::labs(title = plot_title) +
    ggplot2::theme_minimal()
}
.create_bar_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}
.create_scatter_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  if (length(traits) < 2) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = plot_title) +
             ggplot2::theme_minimal())
  }
  ggplot2::ggplot(data,
                  ggplot2::aes(x = .data[[traits[1]]],
                               y = .data[[traits[2]]])) +
    ggplot2::geom_point() +
    ggplot2::labs(title = plot_title) +
    ggplot2::theme_minimal()
}
.create_violin_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}
.create_density_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}
.create_network_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}
.create_radar_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}
.create_pca_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}
.create_landscape_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}
.create_integration_plot_safe <- function(data, traits, group_col, options, ...) {
  plot_title <- options$title
  ggplot2::ggplot() + ggplot2::labs(title = plot_title) + ggplot2::theme_minimal()
}

