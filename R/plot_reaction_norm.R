#' Plot Reaction Norms for Phenotypic Plasticity
#'
#' Creates reaction norm plots to visualize genotype-environment interactions.
#'
#' @param data A data frame containing the phenotypic data
#' @param environment Column name for environment variable(unquoted)
#' @param trait Column name for phenotypic trait (unquoted)
#' @param genotype Column name for genotype grouping (unquoted)
#' @param add_points Whether to add individual data points (default: TRUE)
#' @param add_se Whether to add standard error bars (default: TRUE)
#'
#' @return A ggplot object with reaction norms
#' @export
#'
#' @examples
#' datos <- data.frame(
#'   gen = rep(c("A", "B"), each = 6),
#'   env = rep(1:3, each = 2, times = 2),
#'   trait = c(10, 12, 15, 16, 20, 22, 8, 9, 12, 14, 18, 20)
#' )
#' plot_reaction_norm(datos, env, trait, gen)
plot_reaction_norm <- function(data, environment, trait, genotype,
                                    add_points = TRUE, add_se = TRUE) {

  # Calcular promedios y errores estandar
  plot_data <- data %>%
    dplyr::group_by({{genotype}}, {{environment}}) %>%
    dplyr::summarise(
      mean_trait = mean({{trait}}, na.rm = TRUE),
      se = stats::sd({{trait}}, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  # Crear el grafico base
  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = {{environment}},
                                    y = mean_trait,
                                    color = {{genotype}},
                                    group = {{genotype}})) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(
      title = "Normas de Reaccion Fenotipica",
      x = "Ambiente",
      y = "Valor del Rasgo",
      color = "Genotipo"
    ) +
    ggplot2::theme_minimal()

  # Agregar barras de error si se solicita
  if (add_se) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = mean_trait - se, ymax = mean_trait + se),
      width = 0.1
    )
  }

  # Agregar puntos si se solicita
  if (add_points) {
    p <- p + ggplot2::geom_point(size = 3)
  }

  return(p)
}
