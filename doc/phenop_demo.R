## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 5,
  fig.align = "center",
  out.width = "90%"
)

## ----load-package-------------------------------------------------------------
library(phenop)
library(dplyr)
library(ggplot2)

# Check package version
packageVersion("phenop")

## ----demo-data----------------------------------------------------------------
# Load demo dataset
data(demo_data)

# View dataset structure
str(demo_data)

# First few rows
head(demo_data)

# Summary statistics
summary(demo_data)

## ----data-prep----------------------------------------------------------------
# Prepare insect (host) data
insect_data <- demo_data %>%
  select(insect_population, temperature, humidity,
         insect_size, resistance) %>%
  distinct()

# Prepare fungus (pathogen) data  
fungus_data <- demo_data %>%
  select(fungus_strain, temperature, humidity,
         fungus_growth, virulence) %>%
  distinct()

cat("Insect dataset dimensions:", dim(insect_data), "\n")
cat("Fungus dataset dimensions:", dim(fungus_data), "\n")

## ----safe-analysis------------------------------------------------------------
# Run safe multidimensional plasticity analysis
insect_results <- safe_multidim_plasticity(
  insect_data,
  traits = c("insect_size", "resistance"),
  environments = c("temperature", "humidity"),
  groups = "insect_population",
  na.action = "omit"
)

# View results
print(insect_results)

# Access individual components
cat("\nMultidimensional Plasticity Index (MPI):", 
    round(insect_results$multidimensional_index$index, 3), "\n")
cat("Number of groups analyzed:", insect_results$safe_analysis$n_groups, "\n")

## ----tradeoffs----------------------------------------------------------------
# Analyze trade-offs between plasticity indices
tradeoff_results <- plasticity_tradeoffs_extended(
  insect_results,
  method = "correlation",
  threshold = -0.5,
  p_value_threshold = 0.05
)

print(tradeoff_results)

## ----interaction--------------------------------------------------------------
# Analyze host-pathogen interactions
interaction_results <- host_pathogen_interaction_extended(
  host_data = insect_data,
  pathogen_data = fungus_data,
  host_traits = c("insect_size", "resistance"),
  pathogen_traits = c("fungus_growth", "virulence"),
  interaction_metrics = c("correlation", "plasticity_matching"),
  environments = c("temperature", "humidity"),
  host_group = "insect_population",
  pathogen_group = "fungus_strain"
)

print(interaction_results)

## ----meta-analysis------------------------------------------------------------
# Create simulated meta-analysis data
set.seed(123)
n_studies <- 12

meta_data <- data.frame(
  study = paste("Study", 1:n_studies),
  effect_size = rnorm(n_studies, mean = 0.6, sd = 0.25),
  variance = runif(n_studies, 0.05, 0.15),
  year = 2010:2021,
  organism = sample(c("Insect", "Fungus", "Plant"), n_studies, replace = TRUE),
  trait_type = sample(c("Morphological", "Physiological", "Life History"), 
                     n_studies, replace = TRUE)
)

# Run meta-analysis
meta_results <- plasticity_meta_analysis_extended(
  study_data = meta_data,
  effect_size = "effect_size",
  variance = "variance",
  study_labels = "study",
  method = "random",
  moderator = c("year", "organism")
)

print(meta_results)

## ----visualization------------------------------------------------------------
# Create heatmap
heatmap_plot <- plot_multidim_plasticity_extended(
  insect_results,
  type = "heatmap",
  plot_options = list(
    title = "Plasticity Heatmap - Insect Traits",
    color_palette = "viridis",
    theme = "minimal"
  )
)

print(heatmap_plot)

## ----help, eval=FALSE---------------------------------------------------------
# # View help for specific functions
# ?safe_multidim_plasticity
# ?plasticity_tradeoffs_extended
# ?host_pathogen_interaction_extended
# 
# # View all available functions
# help(package = "phenop")

## ----session-info-------------------------------------------------------------
sessionInfo()

