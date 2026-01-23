#' Multidimensional Plasticity Visualization
#'
#' Creates innovative visualizations for multidimensional plasticity analysis
#' including correlation networks, plasticity landscapes, and trait integration plots.
#'
#' @param multidim_result Result from multidim_plasticity() function
#' @param type Type of plot: "network", "landscape", "integration", "radar"
#' @param traits Optional: subset of traits to display
#' @param plot_options Additional plotting options
#'
#' @return ggplot object or list of plots
#' @export
#'
#' @examples
#' # Usar demo_data en lugar de fungi_insect_data
#' data(demo_data)
#' # Preparar datos para multidim_plasticity
#' result <- safe_multidim_plasticity(
#'   demo_data,
#'   traits = c("insect_size", "resistance"),
#'   environments = c("temperature", "humidity"),
#'   groups = "insect_population"
#' )
#' plot_multidim_plasticity(result, "network")
plot_multidim_plasticity <- function(multidim_result, type = "network",
                                     traits = NULL, plot_options = list()) {
  # Basic validation
  if (is.null(multidim_result)) stop("multidim_result cannot be NULL")
  if (!is.list(multidim_result)) stop("multidim_result must be a list")
  # Determinar traits disponibles
  available_traits <- names(multidim_result$individual_plasticity)
  if (is.null(available_traits) || length(available_traits) == 0) {
    stop("No traits found in multidim_result$individual_plasticity")
  }
  if (is.null(traits)) {
    traits <- available_traits
  } else {
    # Filtrar traits que existen
    traits <- intersect(traits, available_traits)
    if (length(traits) == 0) {
      stop("None of the specified traits found in the result")
    }
  }
  switch(type,
         "network" = .plot_plasticity_network(multidim_result, traits, plot_options),
         "landscape" = .plot_plasticity_landscape(multidim_result, traits, plot_options),
         "integration" = .plot_integration(multidim_result, traits, plot_options),
         "radar" = .plot_plasticity_radar(multidim_result, traits, plot_options),
         stop("Unknown plot type. Use: network, landscape")
  )
}
# Internal function: Plasticity correlation network
# Internal function: Plasticity correlation network
.plot_plasticity_network <- function(result, traits, options) {
  # Check that existe la matriz de correlaciones
  if (is.null(result$plasticity_correlations)) {
    warning("No plasticity_correlations found, creating empty plot")
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Plasticity Correlation Network",
                           subtitle = "No correlation data available") +
             ggplot2::theme_minimal())
  }
  # Obtener la matriz de correlaciones
  cor_matrix <- result$plasticity_correlations
  # Si la matriz no tiene dimnames, asignarlos basados en traits
  if (is.null(rownames(cor_matrix)) || is.null(colnames(cor_matrix))) {
    # Si la matriz es cuadrada, asignar nombres
    if (nrow(cor_matrix) == ncol(cor_matrix)) {
      # Usar los traits si tenemos suficientes, de lo contrario crear nombres genéricos
      if (length(traits) == nrow(cor_matrix)) {
        rownames(cor_matrix) <- traits
        colnames(cor_matrix) <- traits
      } else {
        rownames(cor_matrix) <- paste0("Trait", 1:nrow(cor_matrix))
        colnames(cor_matrix) <- paste0("Trait", 1:ncol(cor_matrix))
      }
    }
  }
  # Ahora podemos indexar con traits
  # Pero primero verificar que los traits existen en los nombres
  available_traits <- rownames(cor_matrix)
  if (is.null(available_traits)) {
    # Si aún no hay nombres, no podemos indexar por character
    # Usar índice numérico si traits son números
    if (is.numeric(traits) && all(traits <= nrow(cor_matrix))) {
      cor_matrix <- cor_matrix[traits, traits, drop = FALSE]
      # Asignar nombres para el resto del proceso
      rownames(cor_matrix) <- traits
      colnames(cor_matrix) <- traits
    } else {
      warning("Cannot index matrix without dimnames using character traits")
      return(ggplot2::ggplot() +
               ggplot2::labs(title = "Plasticity Correlation Network",
                             subtitle = "Cannot index matrix") +
               ggplot2::theme_minimal())
    }
  } else {
    # Filtrar solo traits que existen
    traits <- intersect(traits, available_traits)
    if (length(traits) == 0) {
      warning("No matching traits found in correlation matrix")
      return(ggplot2::ggplot() +
               ggplot2::labs(title = "Plasticity Correlation Network",
                             subtitle = "No matching traits") +
               ggplot2::theme_minimal())
    }
    cor_matrix <- cor_matrix[traits, traits, drop = FALSE]
  }
  # Check that hay datos
  if (nrow(cor_matrix) == 0 || ncol(cor_matrix) == 0) {
    warning("Empty correlation matrix, creating empty plot")
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Plasticity Correlation Network") +
             ggplot2::theme_minimal())
  }
  # Convertir a data.frame
  cor_df <- as.data.frame(cor_matrix)
  cor_df$trait1 <- rownames(cor_df)
  # Pivotar - manejar caso de una sola columna
  if (ncol(cor_df) == 1) {  # Solo trait1
    network_data <- data.frame(
      trait1 = traits,
      trait2 = traits,
      correlation = 1
    )
  } else {
    network_data <- cor_df %>%
      tidyr::pivot_longer(
        cols = -trait1,
        names_to = "trait2",
        values_to = "correlation"
      ) %>%
      dplyr::filter(trait1 != trait2)
  }
  # Crear plot
  network_plot <- ggplot2::ggplot(network_data,
                                  ggplot2::aes(x = trait1, y = trait2, fill = correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                  midpoint = 0, limits = c(-1, 1)) +
    ggplot2::labs(title = "Plasticity Correlation Network",
                  subtitle = "Correlations between trait plasticities",
                  x = "", y = "", fill = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  # Agregar texto si hay datos
  if (nrow(network_data) > 0 && !all(is.na(network_data$correlation))) {
    network_plot <- network_plot +
      ggplot2::geom_text(ggplot2::aes(label = round(correlation, 2)),
                         color = "white", size = 3)
  }
  return(network_plot)
}
# Internal function: Plasticity landscape
.plot_plasticity_landscape <- function(result, traits, options) {
  # Verificar estructura de individual_plasticity
  if (is.null(result$individual_plasticity)) {
    warning("No individual_plasticity data found")
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Plasticity Landscape") +
             ggplot2::theme_minimal())
  }
  # Calcular plasticidad promedio para cada trait
  plasticity_values <- sapply(traits, function(trait) {
    trait_data <- result$individual_plasticity[[trait]]
    if (is.null(trait_data)) return(NA)
    # Manejar diferentes estructuras
    if (is.list(trait_data) && "plasticity_norm" %in% names(trait_data)) {
      # Estructura: list(plasticity_norm = vector, plasticity_raw = vector)
      if (is.numeric(trait_data$plasticity_norm)) {
        mean(trait_data$plasticity_norm, na.rm = TRUE)
      } else {
        NA
      }
    } else if (is.list(trait_data) && is.numeric(trait_data[[1]])) {
      # Estructura: list(vector1, vector2, ...)
      mean(unlist(trait_data), na.rm = TRUE)
    } else if (is.numeric(trait_data)) {
      # Estructura: vector directo
      mean(trait_data, na.rm = TRUE)
    } else {
      NA
    }
  })
  # Crear data.frame
  landscape_df <- data.frame(
    trait = traits,
    plasticity = plasticity_values,
    importance = seq_along(traits)  # Placeholder
  )
  # Filtrar NA
  landscape_df <- landscape_df[!is.na(landscape_df$plasticity), ]
  if (nrow(landscape_df) == 0) {
    warning("No valid plasticity values found")
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Plasticity Landscape") +
             ggplot2::theme_minimal())
  }
  # Crear plot
  landscape_plot <- ggplot2::ggplot(landscape_df,
                                    ggplot2::aes(x = trait, y = plasticity, size = importance)) +
    ggplot2::geom_point(ggplot2::aes(color = plasticity), alpha = 0.7) +
    ggplot2::scale_color_gradient(low = "blue", high = "red") +
    ggplot2::labs(title = "Plasticity Landscape",
                  subtitle = "Multidimensional plasticity profile",
                  x = "Traits", y = "Plasticity Index") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  return(landscape_plot)
}
# Stubs para funciones no implementadas
.plot_integration <- function(result, traits, options) {
  warning("Integration plot not implemented yet")
  ggplot2::ggplot() +
    ggplot2::labs(title = "Integration Plot (Not implemented)") +
    ggplot2::theme_minimal()
}
.plot_plasticity_radar <- function(result, traits, options) {
  warning("Radar plot not implemented yet")
  ggplot2::ggplot() +
    ggplot2::labs(title = "Radar Plot (Not implemented)") +
    ggplot2::theme_minimal()
}

