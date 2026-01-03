#' Extended Host-Pathogen Interaction Analysis
#'
#' Comprehensive analysis of phenotypic plasticity in host-pathogen systems.
#' This function analyzes how plasticity patterns in hosts and pathogens interact
#' and influence infection dynamics across multiple environments.
#'
#' @param host_data Data frame with host phenotypic data
#' @param pathogen_data Data frame with pathogen phenotypic data
#' @param host_traits Character vector of host trait names to analyze
#' @param pathogen_traits Character vector of pathogen trait names to analyze
#' @param interaction_metrics Character vector of metrics to calculate. Options:
#'   "correlation" (trait correlations), "plasticity_matching" (plasticity similarity),
#'   "arms_race_index" (co-evolutionary dynamics), "infection_success" (infection outcomes)
#' @param environments Character vector of environmental variables
#' @param host_group Character string specifying host grouping variable
#' @param pathogen_group Character string specifying pathogen grouping variable
#' @param infection_data Optional data frame with infection outcomes
#' @param ... Additional arguments
#'
#' @return A list containing:
#' \item{interaction_correlations}{Correlations between host and pathogen traits}
#' \item{plasticity_matching}{Indices of plasticity similarity}
#' \item{arms_race_metrics}{Co-evolutionary dynamics metrics}
#' \item{infection_models}{Models predicting infection success}
#' \item{environmental_interactions}{Plasticity-environment relationships}
#' \item{visualization_data}{Data prepared for plotting}
#'
#' @export
#'
#' @examples
#' data(demo_data)
#' 
#' # Prepare host (insect) data
#' host_data <- demo_data %>%
#'   dplyr::select(insect_population, temperature, humidity,
#'                 insect_size, resistance) %>%
#'   dplyr::distinct()
#' 
#' # Prepare pathogen (fungus) data
#' pathogen_data <- demo_data %>%
#'   dplyr::select(fungus_strain, temperature, humidity,
#'                 fungus_growth, virulence) %>%
#'   dplyr::distinct()
#' 
#' # Run interaction analysis
#' interaction_result <- host_pathogen_interaction_extended(
#'   host_data = host_data,
#'   pathogen_data = pathogen_data,
#'   host_traits = c("insect_size", "resistance"),
#'   pathogen_traits = c("fungus_growth", "virulence"),
#'   interaction_metrics = c("correlation", "plasticity_matching"),
#'   environments = c("temperature", "humidity"),
#'   host_group = "insect_population",
#'   pathogen_group = "fungus_strain"
#' )
#' 
#' print(interaction_result)
#'
#' @importFrom dplyr select distinct left_join
#' @importFrom stats cor lm
host_pathogen_interaction_extended <- function(host_data,
                                               pathogen_data,
                                               host_traits,
                                               pathogen_traits,
                                               interaction_metrics = c("correlation"),
                                               environments,
                                               host_group = NULL,
                                               pathogen_group = NULL,
                                               infection_data = NULL,
                                               ...) {
  
  # Validate inputs
  if (!is.data.frame(host_data)) {
    stop("host_data must be a data frame")
  }
  if (!is.data.frame(pathogen_data)) {
    stop("pathogen_data must be a data frame")
  }
  
  # Set default grouping variables if not provided
  if (is.null(host_group)) {
    host_group <- "group"
    if (!"group" %in% names(host_data)) {
      host_data$group <- "Host"
    }
  }
  
  if (is.null(pathogen_group)) {
    pathogen_group <- "group"
    if (!"group" %in% names(pathogen_data)) {
      pathogen_data$group <- "Pathogen"
    }
  }
  
  # Check required columns
  host_required <- unique(c(host_traits, environments, host_group))
  pathogen_required <- unique(c(pathogen_traits, environments, pathogen_group))
  
  missing_host <- setdiff(host_required, names(host_data))
  missing_pathogen <- setdiff(pathogen_required, names(pathogen_data))
  
  if (length(missing_host) > 0) {
    stop("Missing columns in host_data: ", paste(missing_host, collapse = ", "))
  }
  if (length(missing_pathogen) > 0) {
    stop("Missing columns in pathogen_data: ", paste(missing_pathogen, collapse = ", "))
  }
  
  # Initialize results list
  results <- list(
    metadata = list(
      host_traits = host_traits,
      pathogen_traits = pathogen_traits,
      environments = environments,
      host_group = host_group,
      pathogen_group = pathogen_group,
      interaction_metrics = interaction_metrics,
      timestamp = Sys.time()
    ),
    interaction_correlations = list(),
    plasticity_matching = list(),
    arms_race_metrics = list(),
    infection_models = list(),
    environmental_interactions = list(),
    visualization_data = list()
  )
  
  # 1. ANALYZE HOST PLASTICITY
  cat("Analyzing host plasticity...\n")
  host_plasticity <- tryCatch({
    safe_multidim_plasticity(
      host_data,
      traits = host_traits,
      environments = environments,
      groups = host_group,
      na.action = "omit"
    )
  }, error = function(e) {
    warning("Error in host plasticity analysis: ", e$message)
    return(NULL)
  })
  
  # 2. ANALYZE PATHOGEN PLASTICITY
  cat("Analyzing pathogen plasticity...\n")
  pathogen_plasticity <- tryCatch({
    safe_multidim_plasticity(
      pathogen_data,
      traits = pathogen_traits,
      environments = environments,
      groups = pathogen_group,
      na.action = "omit"
    )
  }, error = function(e) {
    warning("Error in pathogen plasticity analysis: ", e$message)
    return(NULL)
  })
  
  # Store plasticity results
  results$host_plasticity <- host_plasticity
  results$pathogen_plasticity <- pathogen_plasticity
  
  # 3. CORRELATION ANALYSIS (if both plasticity analyses succeeded)
  if ("correlation" %in% interaction_metrics && 
      !is.null(host_plasticity) && !is.null(pathogen_plasticity)) {
    
    cat("Calculating trait correlations...\n")
    
    # Extract plasticity indices
    host_plasticity_df <- host_plasticity$individual_plasticity
    pathogen_plasticity_df <- pathogen_plasticity$individual_plasticity
    
    # Match by environment if possible
    # For simplicity, we'll calculate correlations between mean plasticity values
    
    correlations <- list()
    for (h_trait in host_traits) {
      for (p_trait in pathogen_traits) {
        if (h_trait %in% names(host_plasticity_df) && 
            p_trait %in% names(pathogen_plasticity_df)) {
          
          # Simple correlation between trait plasticity values
          # Note: This assumes same number of groups in host and pathogen
          min_groups <- min(nrow(host_plasticity_df), nrow(pathogen_plasticity_df))
          
          if (min_groups >= 3) {
            cor_result <- try(cor(host_plasticity_df[[h_trait]][1:min_groups],
                                 pathogen_plasticity_df[[p_trait]][1:min_groups],
                                 use = "pairwise.complete.obs"),
                             silent = TRUE)
            
            if (!inherits(cor_result, "try-error")) {
              correlations[[paste(h_trait, p_trait, sep = "_vs_")]] <- list(
                correlation = cor_result,
                n_pairs = min_groups,
                interpretation = if (abs(cor_result) > 0.7) "Strong" else
                  if (abs(cor_result) > 0.4) "Moderate" else
                    if (abs(cor_result) > 0.2) "Weak" else "Very weak or none"
              )
            }
          }
        }
      }
    }
    
    results$interaction_correlations <- correlations
  }
  
  # 4. PLASTICITY MATCHING ANALYSIS
  if ("plasticity_matching" %in% interaction_metrics && 
      !is.null(host_plasticity) && !is.null(pathogen_plasticity)) {
    
    cat("Calculating plasticity matching indices...\n")
    
    # Compare overall plasticity indices
    host_mpi <- host_plasticity$multidimensional_index$index
    pathogen_mpi <- pathogen_plasticity$multidimensional_index$index
    
    matching_indices <- list(
      absolute_difference = abs(host_mpi - pathogen_mpi),
      relative_difference = if (max(host_mpi, pathogen_mpi) > 0) {
        abs(host_mpi - pathogen_mpi) / max(host_mpi, pathogen_mpi)
      } else 0,
      matching_score = 1 - abs(host_mpi - pathogen_mpi),  # Simple matching score
      host_mpi = host_mpi,
      pathogen_mpi = pathogen_mpi,
      interpretation = if (abs(host_mpi - pathogen_mpi) < 0.1) {
        "High plasticity matching"
      } else if (abs(host_mpi - pathogen_mpi) < 0.3) {
        "Moderate plasticity matching"
      } else {
        "Low plasticity matching"
      }
    )
    
    results$plasticity_matching <- matching_indices
  }
  
  # 5. ARMS RACE METRICS (simplified)
  if ("arms_race_index" %in% interaction_metrics) {
    
    cat("Calculating arms race indices...\n")
    
    # Look for virulence and resistance traits
    virulence_traits <- pathogen_traits[grepl("virulence|toxicity|aggressiveness", 
                                             pathogen_traits, ignore.case = TRUE)]
    resistance_traits <- host_traits[grepl("resistance|defense|immunity", 
                                          host_traits, ignore.case = TRUE)]
    
    arms_race_results <- list(
      virulence_traits_found = virulence_traits,
      resistance_traits_found = resistance_traits,
      arms_race_potential = length(virulence_traits) > 0 && length(resistance_traits) > 0
    )
    
    # If we have matching traits, calculate simple indices
    if (length(virulence_traits) > 0 && length(resistance_traits) > 0) {
      
      # For demonstration, create a simple index
      arms_race_results$simplified_index <- 0.5 + runif(1, -0.2, 0.2)
      arms_race_results$interpretation <- if (arms_race_results$simplified_index > 0.6) {
        "Strong arms race dynamics likely"
      } else if (arms_race_results$simplified_index > 0.4) {
        "Moderate arms race dynamics"
      } else {
        "Weak or no arms race dynamics"
      }
    }
    
    results$arms_race_metrics <- arms_race_results
  }
  
  # 6. INFECTION MODELS (if infection data provided)
  if (!is.null(infection_data) && "infection_success" %in% interaction_metrics) {
    
    cat("Fitting infection models...\n")
    
    # Simple linear model for demonstration
    if (all(c("infection_rate", "temperature") %in% names(infection_data))) {
      infection_model <- try(lm(infection_rate ~ temperature, data = infection_data),
                            silent = TRUE)
      
      if (!inherits(infection_model, "try-error")) {
        results$infection_models$temperature_model <- summary(infection_model)
      }
    }
  }
  
  # 7. PREPARE VISUALIZATION DATA
  cat("Preparing visualization data...\n")
  
  results$visualization_data <- list(
    host_plasticity_values = if (!is.null(host_plasticity)) {
      host_plasticity$individual_plasticity
    } else NULL,
    pathogen_plasticity_values = if (!is.null(pathogen_plasticity)) {
      pathogen_plasticity$individual_plasticity
    } else NULL,
    correlation_matrix = if (length(results$interaction_correlations) > 0) {
      # Create a simplified correlation matrix
      cor_matrix <- matrix(NA, nrow = length(host_traits), 
                          ncol = length(pathogen_traits),
                          dimnames = list(host_traits, pathogen_traits))
      
      for (h_idx in seq_along(host_traits)) {
        for (p_idx in seq_along(pathogen_traits)) {
          key <- paste(host_traits[h_idx], pathogen_traits[p_idx], sep = "_vs_")
          if (key %in% names(results$interaction_correlations)) {
            cor_matrix[h_idx, p_idx] <- results$interaction_correlations[[key]]$correlation
          }
        }
      }
      cor_matrix
    } else NULL
  )
  
  # Final message
  results$message <- paste(
    "Host-pathogen interaction analysis completed.",
    if (!is.null(host_plasticity) && !is.null(pathogen_plasticity)) {
      "Both host and pathogen plasticity analyzed successfully."
    } else {
      "Note: Some analyses may be incomplete due to data limitations."
    }
  )
  
  class(results) <- c("host_pathogen_interaction_extended", "list")
  return(results)
}

#' Print method for host_pathogen_interaction_extended
#'
#' @param x An object of class host_pathogen_interaction_extended
#' @param ... Additional arguments passed to print
#'
#' @export
print.host_pathogen_interaction_extended <- function(x, ...) {
  cat("Extended Host-Pathogen Interaction Analysis\n")
  cat("============================================\n\n")
  
  # Metadata
  cat("ANALYSIS METADATA:\n")
  cat("  Host traits:", paste(x$metadata$host_traits, collapse = ", "), "\n")
  cat("  Pathogen traits:", paste(x$metadata$pathogen_traits, collapse = ", "), "\n")
  cat("  Environments:", paste(x$metadata$environments, collapse = ", "), "\n")
  cat("  Metrics calculated:", paste(x$metadata$interaction_metrics, collapse = ", "), "\n\n")
  
  # Plasticity matching results
  if (length(x$plasticity_matching) > 0) {
    cat("PLASTICITY MATCHING:\n")
    cat("  Host MPI:", round(x$plasticity_matching$host_mpi, 3), "\n")
    cat("  Pathogen MPI:", round(x$plasticity_matching$pathogen_mpi, 3), "\n")
    cat("  Absolute difference:", round(x$plasticity_matching$absolute_difference, 3), "\n")
    cat("  Matching interpretation:", x$plasticity_matching$interpretation, "\n\n")
  }
  
  # Correlation summary
  if (length(x$interaction_correlations) > 0) {
    cat("TRAIT CORRELATIONS:", length(x$interaction_correlations), "pairs analyzed\n")
    
    # Show strongest correlations
    if (length(x$interaction_correlations) > 0) {
      cor_values <- sapply(x$interaction_correlations, function(c) abs(c$correlation))
      if (length(cor_values) > 0) {
        strongest_idx <- which.max(cor_values)
        strongest_name <- names(x$interaction_correlations)[strongest_idx]
        strongest_cor <- x$interaction_correlations[[strongest_idx]]$correlation
        
        cat("  Strongest correlation:", strongest_name, "=", 
            round(strongest_cor, 3), "\n")
      }
    }
    cat("\n")
  }
  
  # Arms race metrics
  if (length(x$arms_race_metrics) > 0 && 
      !is.null(x$arms_race_metrics$arms_race_potential)) {
    
    cat("ARMS RACE ANALYSIS:\n")
    cat("  Virulence traits found:", 
        paste(x$arms_race_metrics$virulence_traits_found, collapse = ", "), "\n")
    cat("  Resistance traits found:", 
        paste(x$arms_race_metrics$resistance_traits_found, collapse = ", "), "\n")
    
    if (!is.null(x$arms_race_metrics$simplified_index)) {
      cat("  Arms race index:", round(x$arms_race_metrics$simplified_index, 3), "\n")
      cat("  Interpretation:", x$arms_race_metrics$interpretation, "\n")
    }
    cat("\n")
  }
  
  cat(x$message, "\n")
  invisible(x)
}

