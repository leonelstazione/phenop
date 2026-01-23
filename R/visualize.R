# ==============================================================================
# R/visualize.R - Visualization functions for phenop package
# ==============================================================================
#' Plot Seasonal Phenological Curves
#'
#' Creates multi-panel plots showing seasonal NDVI dynamics
#' across species and sites.
#'
#' @param time_series A data frame containing phenological time series
#' @param selected_species Vector of species to include (NULL for all)
#' @param selected_year Year to plot (default: 2021)
#' @param n_populations Number of populations to include (default: 3)
#' @return A ggplot object
#' @export
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom stats loess
#'
#' @examples
#' \dontrun{
#' data(pheno_time_series)
#' plot_seasonal_curves(pheno_time_series, selected_year = 2021)
#' }
plot_seasonal_curves <- function(time_series,
                                 selected_species = NULL,
                                 selected_year = 2021,
                                 n_populations = 3) {
  # Input validation
  if (!is.data.frame(time_series)) {
    stop("time_series must be a data frame")
  }

  required_cols <- c("year", "species", "site", "population_id", "doy", "ndvi")
  missing_cols <- setdiff(required_cols, names(time_series))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter data
  plot_data <- time_series %>%
    dplyr::filter(.data$year == selected_year)

  if (!is.null(selected_species)) {
    plot_data <- plot_data %>%
      dplyr::filter(.data$species %in% selected_species)
  }

  if (nrow(plot_data) == 0) {
    message("No data available for selected criteria")
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data available") +
             theme_void())
  }

  # Select subset of populations
  available_pops <- unique(plot_data$population_id)
  n_pops <- min(n_populations, length(available_pops))
  selected_pops <- available_pops[seq_len(n_pops)]
  plot_data <- plot_data %>%
    dplyr::filter(.data$population_id %in% selected_pops)

  # Create plot
  p <- ggplot(plot_data, aes(x = .data$doy, y = .data$ndvi,
                             color = as.factor(.data$population_id))) +
    geom_line(linewidth = 0.8, alpha = 0.7) +
    geom_smooth(aes(group = .data$species), method = "loess",
                se = FALSE, color = "black", linewidth = 1.2, linetype = "dashed") +
    facet_grid(.data$species ~ .data$site, scales = "free_y") +
    scale_color_viridis_d(option = "plasma") +
    labs(title = paste("Seasonal NDVI Dynamics - Year", selected_year),
         subtitle = "Colored lines: Individual populations | Dashed: Species average",
         x = "Day of Year",
         y = "NDVI",
         color = "Population ID") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank())

  return(p)
}

#' Plot Phenological Parameter Heatmap
#'
#' Creates a heatmap showing variation in phenological parameters
#' across species, sites, and years.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @param parameter Name of parameter to plot (default: "los")
#' @param title Plot title (optional)
#' @return A ggplot object
#' @export
#' @importFrom dplyr group_by summarise
plot_parameter_heatmap <- function(parameters,
                                   parameter = "los",
                                   title = "Phenological Parameter Variation") {
  # Input validation
  if (!is.data.frame(parameters)) {
    stop("parameters must be a data frame")
  }

  required_cols <- c("species", "site", "year", parameter)
  missing_cols <- setdiff(required_cols, names(parameters))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Prepare data
  heatmap_data <- parameters %>%
    dplyr::group_by(.data$species, .data$site, .data$year) %>%
    dplyr::summarise(value = mean(.data[[parameter]], na.rm = TRUE),
                     .groups = 'drop')

  if (nrow(heatmap_data) == 0) {
    message("No data available after summarization")
    return(ggplot() +
             annotate("text", x = 0.5, y = 0.5, label = "No data available") +
             theme_void())
  }

  # Create heatmap
  p <- ggplot(heatmap_data, aes(x = .data$year, y = .data$site, fill = .data$value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    facet_wrap(~ .data$species, ncol = 2) +
    scale_fill_viridis_c(option = "magma", name = toupper(parameter)) +
    scale_x_continuous(breaks = function(x) seq(floor(x[1]), ceiling(x[2]), by = 1)) +
    labs(title = title,
         subtitle = paste("Mean", parameter, "by species, site, and year"),
         x = "Year",
         y = "Site") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_rect(fill = "grey90", color = NA))

  return(p)
}

#' Plot Spatial Phenology Map
#'
#' Creates a spatial map showing phenological patterns across locations.
#'
#' @param spatial_data Spatial data frame (sf object or data.frame with coordinates)
#' @param variable Variable to map as color (default: "los")
#' @param point_size Variable to map as point size (default: "ndvi_max")
#' @param title Plot title (optional)
#' @return A ggplot object
#' @export
plot_spatial_map <- function(spatial_data,
                             variable = "los",
                             point_size = "ndvi_max",
                             title = "Spatial Phenology Patterns") {
  # Input validation
  if (!is.data.frame(spatial_data)) {
    stop("spatial_data must be a data frame")
  }

  # Try to handle sf objects if available
  if (requireNamespace("sf", quietly = TRUE) && inherits(spatial_data, "sf")) {
    coords <- sf::st_coordinates(spatial_data)
    spatial_data$longitude <- coords[,1]
    spatial_data$latitude <- coords[,2]
  }

  # Check required columns
  required_cols <- c("longitude", "latitude", variable, point_size)
  missing_cols <- setdiff(required_cols, names(spatial_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Create base plot
  p <- ggplot(spatial_data,
              aes(x = .data$longitude, y = .data$latitude,
                  color = .data[[variable]], size = .data[[point_size]])) +
    geom_point(alpha = 0.6) +
    coord_quickmap(xlim = range(spatial_data$longitude, na.rm = TRUE) + c(-1, 1),
                   ylim = range(spatial_data$latitude, na.rm = TRUE) + c(-1, 1)) +
    scale_color_viridis_c(option = "viridis", name = variable) +
    scale_size_continuous(range = c(1, 4), name = point_size) +
    labs(title = title,
         subtitle = paste("Color:", variable, "| Size:", point_size),
         x = "Longitude",
         y = "Latitude") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          panel.background = element_rect(fill = "lightblue", color = NA))

  # Try to add map borders if maps package is available
  if (requireNamespace("maps", quietly = TRUE)) {
    world_map <- map_data("world")
    p <- p +
      geom_polygon(data = world_map, aes(x = .data$long, y = .data$lat, group = .data$group),
                   fill = "grey90", color = "grey70", alpha = 0.3, inherit.aes = FALSE)
  }

  return(p)
}

#' Plot Productivity vs Climate Relationships
#'
#' Shows relationships between productivity (GPP) and climate variables.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @return A ggplot object
#' @export
plot_productivity_climate <- function(parameters) {
  # Input validation
  if (!is.data.frame(parameters)) {
    stop("parameters must be a data frame")
  }

  required_cols <- c("mean_temperature", "gpp_total", "species")
  missing_cols <- setdiff(required_cols, names(parameters))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Add phenology_type if missing
  if (!"phenology_type" %in% names(parameters)) {
    parameters$phenology_type <- "unknown"
  }

  # Add ndvi_max if missing
  if (!"ndvi_max" %in% names(parameters)) {
    parameters$ndvi_max <- 0.7
  }

  p <- ggplot(parameters, aes(x = .data$mean_temperature, y = .data$gpp_total)) +
    geom_point(aes(color = .data$phenology_type, size = .data$ndvi_max), alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred",
                fill = "pink", alpha = 0.3) +
    facet_wrap(~ .data$species, scales = "free_y") +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.8) +
    scale_size_continuous(range = c(1, 5)) +
    labs(title = "Productivity-Climate Relationships",
         subtitle = "Gross Primary Production vs Mean Temperature",
         x = "Mean Annual Temperature (C)",
         y = "Total GPP",
         color = "Phenology Type",
         size = "Max NDVI") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          panel.grid.minor = element_blank())

  return(p)
}

#' Plot Density Ridges of Phenological Parameters
#'
#' Creates density ridge plots showing distributions of phenological parameters.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @param parameter Name of parameter to plot (default: "los")
#' @param group_by Grouping variable (default: "species")
#' @param title Plot title (optional)
#' @return A ggplot object
#' @export
plot_density_ridges <- function(parameters,
                                parameter = "los",
                                group_by = "species",
                                title = NULL) {
  # Input validation
  if (!is.data.frame(parameters)) {
    stop("parameters must be a data frame")
  }

  required_cols <- c(parameter, group_by)
  missing_cols <- setdiff(required_cols, names(parameters))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (requireNamespace("ggridges", quietly = TRUE)) {
    if (is.null(title)) {
      title <- paste("Distribution of", parameter, "by", group_by)
    }

    p <- ggplot(parameters, aes(x = .data[[parameter]], y = .data[[group_by]],
                                fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
      scale_fill_viridis_c(option = "turbo", name = parameter) +
      labs(title = title,
           x = parameter,
           y = group_by) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  } else {
    # Fallback to regular density plot if ggridges not available
    message("Package 'ggridges' not available. Using regular density plot.")

    if (is.null(title)) {
      title <- paste("Distribution of", parameter, "by", group_by)
    }

    p <- ggplot(parameters, aes(x = .data[[parameter]], fill = .data[[group_by]])) +
      geom_density(alpha = 0.5) +
      scale_fill_viridis_d(option = "turbo") +
      labs(title = title,
           x = parameter,
           y = "Density",
           fill = group_by) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")
  }

  return(p)
}

#' Create Phenology Dashboard
#'
#' Generates a comprehensive dashboard with multiple phenological visualizations.
#'
#' @param time_series Time series data
#' @param parameters Extracted parameters data
#' @param spatial_data Spatial data
#' @param selected_year Year for seasonal curves (default: 2021)
#' @return A patchwork object combining multiple plots
#' @export
#' @importFrom patchwork plot_layout plot_annotation
create_phenology_dashboard <- function(time_series, parameters, spatial_data,
                                       selected_year = 2021) {
  # Input validation
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for creating dashboards.")
  }

  # Helper function to create a placeholder plot
  create_placeholder <- function(message) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = message,
               size = 4, color = "darkred") +
      theme_void() +
      labs(title = message)
  }

  # Function to safely create a plot with better error handling
  safe_create_plot <- function(func, ...) {
    tryCatch({
      result <- func(...)
      # Verify it's a ggplot object
      if (!inherits(result, "gg")) {
        return(create_placeholder("Invalid plot object"))
      }
      return(result)
    }, error = function(e) {
      return(create_placeholder(paste("Error:", e$message)))
    }, warning = function(w) {
      # Try to create plot even with warnings
      tryCatch({
        result <- func(...)
        if (!inherits(result, "gg")) {
          return(create_placeholder("Invalid plot object"))
        }
        return(result)
      }, error = function(e) {
        return(create_placeholder(paste("Error:", e$message)))
      })
    })
  }

  # Create individual plots with robust error handling
  plots <- list()

  # Plot 1: Seasonal curves
  plots$p1 <- safe_create_plot(
    function() plot_seasonal_curves(time_series, selected_year = selected_year)
  )

  # Plot 2: Heatmap
  plots$p2 <- safe_create_plot(
    function() plot_parameter_heatmap(parameters, parameter = "los")
  )

  # Plot 3: Spatial map
  plots$p3 <- safe_create_plot(
    function() plot_spatial_map(spatial_data, variable = "los")
  )

  # Plot 4: Productivity climate
  plots$p4 <- safe_create_plot(
    function() plot_productivity_climate(parameters)
  )

  # Plot 5: Density ridges for SOS
  plots$p5 <- safe_create_plot(
    function() plot_density_ridges(parameters, parameter = "sos")
  )

  # Plot 6: Density ridges for EOS
  plots$p6 <- safe_create_plot(
    function() plot_density_ridges(parameters, parameter = "eos")
  )

  # Ensure all plots are valid ggplot objects
  for (plot_name in names(plots)) {
    if (!inherits(plots[[plot_name]], "gg")) {
      plots[[plot_name]] <- create_placeholder(paste("Invalid plot:", plot_name))
    }
  }

  # Combine plots - use a more robust method
  tryCatch({
    # First row: p1 + p2
    row1 <- tryCatch({
      plots$p1 + plots$p2
    }, error = function(e) {
      plots$p1 + create_placeholder("Plot 2 failed")
    })

    # Second row: p3 + p4
    row2 <- tryCatch({
      plots$p3 + plots$p4
    }, error = function(e) {
      plots$p3 + create_placeholder("Plot 4 failed")
    })

    # Third row: p5 + p6
    row3 <- tryCatch({
      plots$p5 + plots$p6
    }, error = function(e) {
      plots$p5 + create_placeholder("Plot 6 failed")
    })

    # Combine rows
    dashboard <- row1 / row2 / row3

    # Add annotation
    dashboard <- dashboard +
      patchwork::plot_annotation(
        title = "Comprehensive Phenological Analysis Dashboard",
        subtitle = "Synthetic dataset demonstration for phenop package",
        caption = paste("Generated on", Sys.Date()),
        theme = theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(size = 10, color = "grey50")
        )
      )

    return(dashboard)

  }, error = function(e) {
    # Fallback: return a single plot with error message
    error_plot <- create_placeholder(paste("Dashboard creation failed:", e$message))
    return(error_plot +
             patchwork::plot_annotation(
               title = "Dashboard Error",
               theme = theme(plot.title = element_text(color = "darkred"))
             ))
  })
}
