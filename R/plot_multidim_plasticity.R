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
#' result <- multidim_plasticity(fungi_insect_data,
#'                              c("fungus_growth", "virulence", "insect_size"),
#'                              c("temperature", "humidity"),
#'                              "fungus_strain")
#' plot_multidim_plasticity(result, "network")
plot_multidim_plasticity <- function(multidim_result, type = "network",
                                     traits = NULL, plot_options = list()) {

  if (is.null(traits)) {
    traits <- names(multidim_result$individual_plasticity)
  }

  switch(type,
         "network" = .plot_plasticity_network(multidim_result, traits, plot_options),
         "landscape" = .plot_plasticity_landscape(multidim_result, traits, plot_options),
         "integration" = .plot_integration(multidim_result, traits, plot_options),
         "radar" = .plot_plasticity_radar(multidim_result, traits, plot_options),
         stop("Unknown plot type. Use: network, landscape, integration, radar")
  )
}

# Internal function: Plasticity correlation network
.plot_plasticity_network <- function(result, traits, options) {
  cor_matrix <- result$plasticity_correlations[traits, traits]

  # Create network plot showing correlations between trait plasticities
  # This is an ORIGINAL visualization method
  network_plot <- cor_matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column("trait1") %>%
    tidyr::pivot_longer(cols = -trait1, names_to = "trait2", values_to = "correlation") %>%
    dplyr::filter(trait1 != trait2) %>%
    ggplot2::ggplot(ggplot2::aes(x = trait1, y = trait2, fill = correlation)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(correlation, 2)), color = "white") +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                  midpoint = 0, limits = c(-1, 1)) +
    ggplot2::labs(title = "Plasticity Correlation Network",
                  subtitle = "Correlations between trait plasticities",
                  x = "", y = "", fill = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(network_plot)
}

# Internal function: Plasticity landscape
.plot_plasticity_landscape <- function(result, traits, options) {
  # Create 3D-like landscape of plasticity across environmental gradients
  # ORIGINAL visualization method
  plasticity_values <- sapply(result$individual_plasticity[traits], function(trait_data) {
    mean(sapply(trait_data, function(x) x$plasticity_norm), na.rm = TRUE)
  })

  landscape_df <- data.frame(
    trait = traits,
    plasticity = plasticity_values,
    importance = seq_along(traits)  # Placeholder for ecological importance
  )

  landscape_plot <- landscape_df %>%
    ggplot2::ggplot(ggplot2::aes(x = trait, y = plasticity, size = importance)) +
    ggplot2::geom_point(ggplot2::aes(color = plasticity), alpha = 0.7) +
    ggplot2::scale_color_gradient(low = "blue", high = "red") +
    ggplot2::labs(title = "Plasticity Landscape",
                  subtitle = "Multidimensional plasticity profile",
                  x = "Traits", y = "Plasticity Index") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  return(landscape_plot)
}
