#' Extended Multidimensional Plasticity Visualization
#'
#' Advanced plotting functions for visualizing multidimensional plasticity results.
#' This function creates various plot types including heatmaps, networks,
#' radar plots, and more, with extensive customization options.
#'
#' @param multidim_result Result object from multidim_plasticity() or safe_multidim_plasticity()
#' @param type Type of plot to create. Options:
#'   "heatmap" (default), "network", "radar", "bar", "scatter",
#'   "violin", "density", "pca", "landscape", "integration"
#' @param traits Optional character vector of traits to include (subset of all traits)
#' @param plot_options List of additional plotting options:
#'   - title: Plot title
#'   - color_palette: Color palette name ("viridis", "plasma", "magma", "inferno", "cividis")
#'   - theme: ggplot2 theme ("minimal", "classic", "bw", "dark", "light")
#'   - interactive: Logical, whether to create interactive plot (requires plotly)
#'   - save_plot: Logical, whether to save plot to file
#'   - filename: Filename for saved plot
#'   - width, height: Plot dimensions in inches
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot object (or plotly object if interactive = TRUE)
#'
#' @export
#'
#' @examples
#' data(demo_data)
#' 
#' # Run plasticity analysis
#' insect_data <- demo_data %>%
#'   dplyr::select(insect_population, temperature, humidity,
#'                 insect_size, resistance) %>%
#'   dplyr::distinct()
#'   
#' result <- safe_multidim_plasticity(
#'   insect_data,
#'   traits = c("insect_size", "resistance"),
#'   environments = c("temperature", "humidity"),
#'   groups = "insect_population"
#' )
#' 
#' # Create different types of plots
#' heatmap_plot <- plot_multidim_plasticity_extended(
#'   result,
#'   type = "heatmap",
#'   plot_options = list(title = "Plasticity Heatmap", color_palette = "viridis")
#' )
#' 
#' bar_plot <- plot_multidim_plasticity_extended(
#'   result,
#'   type = "bar",
#'   plot_options = list(title = "Average Plasticity by Trait")
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c theme_minimal
#'   labs theme element_text geom_bar coord_flip geom_point geom_smooth
#'   geom_violin geom_boxplot geom_density
#' @importFrom tidyr pivot_longer
plot_multidim_plasticity_extended <- function(multidim_result,
                                              type = "heatmap",
                                              traits = NULL,
                                              plot_options = list(),
                                              ...) {
  
  # Validate input
  if (is.null(multidim_result)) {
    stop("multidim_result cannot be NULL")
  }
  
  if (is.null(multidim_result$individual_plasticity)) {
    warning("No individual plasticity data found in multidim_result")
    return(NULL)
  }
  
  # Extract plasticity data
  plasticity_data <- multidim_result$individual_plasticity
  
  # Set default plot options
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
  
  # Merge user options with defaults
  plot_options <- modifyList(default_options, plot_options)
  
  # Identify trait columns
  numeric_cols <- names(plasticity_data)[sapply(plasticity_data, is.numeric)]
  group_cols <- c("group", "individual", "groups", "genotype", 
                  "population", "strain", "id", "sample")
  all_trait_cols <- setdiff(numeric_cols, group_cols)
  
  # Filter traits if specified
  if (!is.null(traits)) {
    trait_cols <- intersect(all_trait_cols, traits)
    if (length(trait_cols) == 0) {
      warning("None of the specified traits found. Using all available traits.")
      trait_cols <- all_trait_cols
    }
  } else {
    trait_cols <- all_trait_cols
  }
  
  if (length(trait_cols) == 0) {
    warning("No numeric trait columns found for plotting")
    return(NULL)
  }
  
  # Find group column
  group_col <- NULL
  for (col in group_cols) {
    if (col %in% names(plasticity_data)) {
      group_col <- col
      break
    }
  }
  
  # If no standard group column found, use first non-numeric column
  if (is.null(group_col)) {
    non_numeric <- names(plasticity_data)[!sapply(plasticity_data, is.numeric)]
    if (length(non_numeric) > 0) {
      group_col <- non_numeric[1]
    } else {
      # Create dummy group column
      plasticity_data$group <- paste0("Group", 1:nrow(plasticity_data))
      group_col <- "group"
    }
  }
  
  # Prepare data for plotting
  plot_data <- plasticity_data
  
  # Create plot based on type
  if (type == "heatmap") {
    plot_obj <- .create_heatmap_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "network") {
    plot_obj <- .create_network_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "radar") {
    plot_obj <- .create_radar_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "bar") {
    plot_obj <- .create_bar_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "scatter") {
    plot_obj <- .create_scatter_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "violin") {
    plot_obj <- .create_violin_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "density") {
    plot_obj <- .create_density_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "pca") {
    plot_obj <- .create_pca_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "landscape") {
    plot_obj <- .create_landscape_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else if (type == "integration") {
    plot_obj <- .create_integration_plot(plot_data, trait_cols, group_col, plot_options, ...)
    
  } else {
    warning("Plot type '", type, "' not recognized. Using 'heatmap' instead.")
    plot_obj <- .create_heatmap_plot(plot_data, trait_cols, group_col, plot_options, ...)
  }
  
  # Make interactive if requested
  if (plot_options$interactive) {
    if (requireNamespace("plotly", quietly = TRUE)) {
      plot_obj <- plotly::ggplotly(plot_obj)
    } else {
      warning("plotly package not installed. Install with: install.packages('plotly')")
    }
  }
  
  # Save plot if requested
  if (plot_options$save_plot) {
    ggplot2::ggsave(
      filename = plot_options$filename,
      plot = plot_obj,
      width = plot_options$width,
      height = plot_options$height,
      dpi = 300
    )
    cat("Plot saved as:", plot_options$filename, "\n")
  }
  
  return(plot_obj)
}

# Helper function: Heatmap plot
.create_heatmap_plot <- function(data, traits, group_col, options, ...) {
  
  # Prepare data in long format
  data_long <- data %>%
    dplyr::select(dplyr::all_of(c(group_col, traits))) %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(group_col),
      names_to = "trait",
      values_to = "plasticity"
    )
  
  # Convert group column to factor for proper ordering
  data_long[[group_col]] <- factor(data_long[[group_col]])
  
  # Create heatmap
  p <- ggplot2::ggplot(data_long, ggplot2::aes(x = trait, y = .data[[group_col]],
                                              fill = plasticity)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_viridis_c(
      option = options$color_palette,
      name = "Plasticity\nIndex",
      na.value = "grey50"
    ) +
    ggplot2::labs(
      title = options$title,
      x = "Traits",
      y = stringr::str_to_title(gsub("_", " ", group_col))
    ) +
    .get_ggplot_theme(options$theme) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = options$text_size * 0.8
      ),
      axis.text.y = ggplot2::element_text(size = options$text_size * 0.8),
      legend.position = options$legend_position,
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size * 1.2,
        face = "bold"
      )
    )
  
  return(p)
}

# Helper function: Bar plot
.create_bar_plot <- function(data, traits, group_col, options, ...) {
  
  # Calculate mean plasticity per trait
  trait_means <- colMeans(data[, traits, drop = FALSE], na.rm = TRUE)
  bar_data <- data.frame(
    trait = names(trait_means),
    mean_plasticity = as.numeric(trait_means),
    stringsAsFactors = FALSE
  )
  
  # Order by plasticity
  bar_data <- bar_data[order(bar_data$mean_plasticity, decreasing = FALSE), ]
  bar_data$trait <- factor(bar_data$trait, levels = bar_data$trait)
  
  # Create bar plot
  p <- ggplot2::ggplot(bar_data, ggplot2::aes(x = trait, y = mean_plasticity,
                                             fill = mean_plasticity)) +
    ggplot2::geom_bar(stat = "identity", color = "black", alpha = options$alpha) +
    ggplot2::scale_fill_viridis_c(
      option = options$color_palette,
      name = "Mean\nPlasticity"
    ) +
    ggplot2::labs(
      title = options$title,
      x = "Traits",
      y = "Mean Plasticity Index"
    ) +
    .get_ggplot_theme(options$theme) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = options$text_size * 0.8
      ),
      legend.position = options$legend_position,
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size * 1.2,
        face = "bold"
      )
    ) +
    ggplot2::coord_flip()
  
  return(p)
}

# Helper function: Scatter plot
.create_scatter_plot <- function(data, traits, group_col, options, ...) {
  
  # Need at least 2 traits for scatter plot
  if (length(traits) < 2) {
    warning("Need at least 2 traits for scatter plot")
    return(NULL)
  }
  
  # Use first two traits by default
  x_trait <- traits[1]
  y_trait <- traits[2]
  
  # Create scatter plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_trait]], 
                                         y = .data[[y_trait]],
                                         color = .data[[group_col]])) +
    ggplot2::geom_point(size = options$point_size, alpha = options$alpha) +
    ggplot2::scale_color_viridis_d(
      option = options$color_palette,
      name = stringr::str_to_title(gsub("_", " ", group_col))
    ) +
    ggplot2::labs(
      title = options$title,
      x = x_trait,
      y = y_trait
    ) +
    .get_ggplot_theme(options$theme) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size * 1.2,
        face = "bold"
      ),
      legend.position = options$legend_position
    )
  
  # Add trend line if enough points
  if (nrow(data) >= 3) {
    p <- p + ggplot2::geom_smooth(
      method = "lm", 
      se = TRUE, 
      color = "darkred", 
      fill = "pink", 
      alpha = 0.2,
      linewidth = 1
    )
  }
  
  return(p)
}

# Helper function: Violin plot
.create_violin_plot <- function(data, traits, group_col, options, ...) {
  
  # Prepare data in long format
  data_long <- data %>%
    dplyr::select(dplyr::all_of(c(group_col, traits))) %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(group_col),
      names_to = "trait",
      values_to = "plasticity"
    )
  
  # Create violin plot
  p <- ggplot2::ggplot(data_long, ggplot2::aes(x = trait, y = plasticity,
                                              fill = trait)) +
    ggplot2::geom_violin(alpha = options$alpha) +
    ggplot2::geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
    ggplot2::scale_fill_viridis_d(
      option = options$color_palette,
      name = "Traits"
    ) +
    ggplot2::labs(
      title = options$title,
      x = "Traits",
      y = "Plasticity Index"
    ) +
    .get_ggplot_theme(options$theme) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = options$text_size * 0.8
      ),
      legend.position = "none",
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size * 1.2,
        face = "bold"
      )
    )
  
  return(p)
}

# Helper function: Density plot
.create_density_plot <- function(data, traits, group_col, options, ...) {
  
  # Prepare data in long format
  data_long <- data %>%
    dplyr::select(dplyr::all_of(c(group_col, traits))) %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(group_col),
      names_to = "trait",
      values_to = "plasticity"
    )
  
  # Create density plot
  p <- ggplot2::ggplot(data_long, ggplot2::aes(x = plasticity, fill = trait,
                                              color = trait)) +
    ggplot2::geom_density(alpha = options$alpha * 0.5) +
    ggplot2::scale_fill_viridis_d(option = options$color_palette) +
    ggplot2::scale_color_viridis_d(option = options$color_palette) +
    ggplot2::labs(
      title = options$title,
      x = "Plasticity Index",
      y = "Density"
    ) +
    .get_ggplot_theme(options$theme) +
    ggplot2::theme(
      legend.position = options$legend_position,
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size * 1.2,
        face = "bold"
      )
    )
  
  return(p)
}

# Helper function: Network plot (simplified - correlation network)
.create_network_plot <- function(data, traits, group_col, options, ...) {
  
  # Calculate correlation matrix
  cor_matrix <- cor(data[, traits, drop = FALSE], use = "pairwise.complete.obs")
  
  # Convert to long format
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("trait1", "trait2", "correlation")
  
  # Remove self-correlations and NA values
  cor_data <- cor_data[cor_data$trait1 != cor_data$trait2 & 
                        !is.na(cor_data$correlation), ]
  
  # Create network plot (as heatmap for simplicity)
  p <- ggplot2::ggplot(cor_data, ggplot2::aes(x = trait1, y = trait2,
                                             fill = correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = round(correlation, 2)), 
                      color = "black", size = options$text_size * 0.6) +
    ggplot2::scale_fill_gradient2(
      low = "#2E86AB", mid = "white", high = "#A23B72",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    ggplot2::labs(
      title = options$title,
      subtitle = "Correlations between trait plasticities",
      x = "",
      y = ""
    ) +
    .get_ggplot_theme(options$theme) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = options$text_size * 0.8
      ),
      axis.text.y = ggplot2::element_text(size = options$text_size * 0.8),
      legend.position = options$legend_position,
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size * 1.2,
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size,
        face = "italic"
      )
    )
  
  return(p)
}

# Helper function: Radar plot (placeholder - requires ggradar or similar)
.create_radar_plot <- function(data, traits, group_col, options, ...) {
  message("Radar plot: This plot type requires additional packages (ggradar).\n",
          "Install with: remotes::install_github('ricardo-bion/ggradar')\n",
          "Returning a bar plot instead.")
  return(.create_bar_plot(data, traits, group_col, options, ...))
}

# Helper function: PCA plot
.create_pca_plot <- function(data, traits, group_col, options, ...) {
  
  # Perform PCA
  pca_data <- data[, traits, drop = FALSE]
  pca_data <- pca_data[complete.cases(pca_data), ]
  
  if (nrow(pca_data) < 2) {
    warning("Insufficient data for PCA")
    return(NULL)
  }
  
  pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)
  pca_scores <- as.data.frame(pca_result$x[, 1:2])
  
  # Add group information
  pca_scores[[group_col]] <- data[[group_col]][complete.cases(data[, traits, drop = FALSE])]
  
  # Calculate variance explained
  var_explained <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 1)
  
  # Create PCA plot
  p <- ggplot2::ggplot(pca_scores, ggplot2::aes(x = PC1, y = PC2,
                                               color = .data[[group_col]])) +
    ggplot2::geom_point(size = options$point_size, alpha = options$alpha) +
    ggplot2::scale_color_viridis_d(
      option = options$color_palette,
      name = stringr::str_to_title(gsub("_", " ", group_col))
    ) +
    ggplot2::labs(
      title = options$title,
      x = paste0("PC1 (", var_explained[1], "% variance)"),
      y = paste0("PC2 (", var_explained[2], "% variance)")
    ) +
    .get_ggplot_theme(options$theme) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = options$text_size * 1.2,
        face = "bold"
      ),
      legend.position = options$legend_position
    )
  
  return(p)
}

# Helper function: Landscape plot (placeholder)
.create_landscape_plot <- function(data, traits, group_col, options, ...) {
  message("Landscape plot: Advanced visualization in development.")
  return(.create_heatmap_plot(data, traits, group_col, options, ...))
}

# Helper function: Integration plot (placeholder)
.create_integration_plot <- function(data, traits, group_col, options, ...) {
  message("Integration plot: Advanced visualization in development.")
  return(.create_network_plot(data, traits, group_col, options, ...))
}

# Helper function: Get ggplot theme
.get_ggplot_theme <- function(theme_name) {
  themes <- list(
    minimal = ggplot2::theme_minimal(),
    classic = ggplot2::theme_classic(),
    bw = ggplot2::theme_bw(),
    dark = ggplot2::theme_dark(),
    light = ggplot2::theme_light(),
    gray = ggplot2::theme_gray()
  )
  
  if (theme_name %in% names(themes)) {
    return(themes[[theme_name]])
  } else {
    return(ggplot2::theme_minimal())
  }
}

#' Print method for plot objects
#'
#' @param x A plot object
#' @param ... Additional arguments passed to print
#'
#' @export
print.plot_multidim_result <- function(x, ...) {
  if (inherits(x, "ggplot")) {
    print(x)
  } else if (inherits(x, "plotly")) {
    print(x)
  } else {
    cat("Plot object of class:", paste(class(x), collapse = ", "), "\n")
  }
  invisible(x)
}

