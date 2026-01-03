# Definir variables globales para evitar notas en R CMD check
utils::globalVariables(c(
  "trait", "plasticity", "importance", "trait1", "trait2", "correlation",
  "mean_trait", "se", ".plot_integration", ".plot_plasticity_radar"
))
