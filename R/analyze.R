# ==============================================================================
# R/analyze.R - Statistical analysis functions for phenop package
# ==============================================================================
#' Analyze Phenology with Mixed Effects Models
#'
#' Fits mixed effects models to analyze phenological timing.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @return A list with model results
#' @export
#' @importFrom lme4 lmer fixef ranef
#' @importFrom mgcv gam
#' @importFrom dplyr group_by do ungroup select mutate filter arrange slice_head
#' @importFrom performance r2
#' @importFrom stats lm anova cor dist na.omit sd coef
#' @importFrom FactoMineR PCA
#' @importFrom cluster silhouette
#' @importFrom stats kmeans
#'
#' @examples
#' \dontrun{
#' data(pheno_parameters)
#' results <- analyze_mixed_effects(pheno_parameters)
#' }
analyze_mixed_effects <- function(parameters) {
  # Check if lme4 is available
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for mixed effects models")
  }
  model <- lme4::lmer(sos ~ mean_temperature + total_precipitation +
                        (1|species) + (1|site) + (1|year),
                      data = parameters)
  # Extract results
  model_summary <- summary(model)
  fixed_effects <- lme4::fixef(model)
  random_effects <- lme4::ranef(model)
  # Calculate R-squared if possible
  if (requireNamespace("performance", quietly = TRUE)) {
    r2 <- performance::r2(model)
    marginal_r2 <- r2$R2_marginal
    conditional_r2 <- r2$R2_conditional
  } else {
    marginal_r2 <- NA
    conditional_r2 <- NA
    message("Install 'performance' package for R-squared values")
  }
  results <- list(
    model_summary = model_summary,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    anova = anova(model),
    marginal_r2 = marginal_r2,
    conditional_r2 = conditional_r2
  )
  return(results)
}
#' Analyze GAM relationships
#'
#' Fits Generalized Additive Models to capture non-linear relationships.
#'
#' @param time_series_subset A subset of the time series data
#' @return A GAM model object
#' @export
#' @examples
#' \dontrun{
#' data(pheno_time_series)
#' subset <- pheno_time_series[pheno_time_series$species == "Quercus_ilex", ]
#' gam_model <- analyze_gam_relationships(subset)
#' }
analyze_gam_relationships <- function(time_series_subset) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' is required for GAM models")
  }
  gam_model <- mgcv::gam(ndvi ~ s(doy, bs = "cc", k = 12) +
                           s(temperature, k = 10) +
                           s(precipitation, k = 10) +
                           species,
                         data = time_series_subset,
                         family = stats::gaussian(),
                         method = "REML")
  return(gam_model)
}
#' Analyze Temporal Trends in Phenology
#'
#' Calculates linear trends in phenological parameters over years.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @return A data frame with trend statistics
#' @export
#' @examples
#' \dontrun{
#' data(pheno_parameters)
#' trends <- analyze_temporal_trends(pheno_parameters)
#' }
analyze_temporal_trends <- function(parameters) {
  # Helper function to safely extract slope and p-value
  safe_lm <- function(formula, data) {
    # Check if we have enough data
    if (nrow(data) < 2 || length(unique(data$year)) < 2) {
      return(list(slope = NA_real_, p = NA_real_))
    }

    tryCatch({
      mod <- stats::lm(formula, data = data)
      if (length(stats::coef(mod)) > 1) {
        slope <- stats::coef(mod)[2]
        p_value <- summary(mod)$coefficients[2, 4]
        return(list(slope = slope, p = p_value))
      } else {
        return(list(slope = NA_real_, p = NA_real_))
      }
    }, error = function(e) {
      return(list(slope = NA_real_, p = NA_real_))
    })
  }

  # Calculate trends for each group
  trends <- parameters %>%
    dplyr::group_by(species, site) %>%
    dplyr::summarise(
      # SOS trend
      sos_result = list(safe_lm(sos ~ year, data = pick(everything()))),
      sos_slope = sos_result[[1]]$slope,
      sos_p = sos_result[[1]]$p,

      # EOS trend
      eos_result = list(safe_lm(eos ~ year, data = pick(everything()))),
      eos_slope = eos_result[[1]]$slope,
      eos_p = eos_result[[1]]$p,

      # LOS trend
      los_result = list(safe_lm(los ~ year, data = pick(everything()))),
      los_slope = los_result[[1]]$slope,
      los_p = los_result[[1]]$p,

      .groups = 'drop'
    ) %>%
    dplyr::select(-sos_result, -eos_result, -los_result)

  return(trends)
}
#' Perform Cluster Analysis on Phenological Types
#'
#' Groups populations based on phenological characteristics using k-means.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @param n_clusters Number of clusters (default: 4)
#' @return A list with clustering results
#' @export
#' @examples
#' \dontrun{
#' data(pheno_parameters)
#' clusters <- perform_cluster_analysis(pheno_parameters, n_clusters = 3)
#' }
perform_cluster_analysis <- function(parameters, n_clusters = 4) {
  # Prepare data for clustering
  cluster_data <- parameters %>%
    dplyr::select(sos, eos, los, ndvi_max, gpp_total,
                  mean_temperature, total_precipitation)

  # Identify complete rows (without NAs)
  complete_rows <- stats::complete.cases(cluster_data)
  cluster_data <- cluster_data[complete_rows, ] %>%
    scale()

  # K-means clustering
  set.seed(123)
  kmeans_result <- stats::kmeans(cluster_data, centers = n_clusters, nstart = 25)

  # Add cluster assignments only to complete rows
  parameters$cluster <- NA
  parameters$cluster[complete_rows] <- as.factor(kmeans_result$cluster)

  # Calculate silhouette width if cluster package is available
  if (requireNamespace("cluster", quietly = TRUE) && nrow(cluster_data) > 1) {
    dist_matrix <- stats::dist(cluster_data)
    sil_width <- cluster::silhouette(kmeans_result$cluster, dist_matrix)
  } else {
    sil_width <- NULL
  }

  return(list(
    clusters = parameters,
    kmeans = kmeans_result,
    silhouette = sil_width
  ))
}
#' Analyze Correlations Between Phenological Parameters
#'
#' Calculates correlation matrix between key phenological variables.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @return A list with correlation results
#' @export
#' @examples
#' \dontrun{
#' data(pheno_parameters)
#' corr_results <- analyze_correlations(pheno_parameters)
#' }
analyze_correlations <- function(parameters) {
  # Select key variables for correlation
  cor_vars <- parameters %>%
    dplyr::select(sos, eos, los, ndvi_max, gpp_total,
                  mean_temperature, total_precipitation,
                  dplyr::any_of("greenup_rate")) %>%
    stats::na.omit()
  # Calculate correlation matrix
  cor_matrix <- stats::cor(cor_vars, use = "complete.obs")
  # Melt for visualization
  cor_data <- as.data.frame(as.table(cor_matrix)) %>%
    dplyr::filter(Var1 != Var2) %>%
    dplyr::mutate(
      sign = ifelse(Freq > 0, "positive", "negative"),
      abs_cor = abs(Freq)
    )
  return(list(
    matrix = cor_matrix,
    plot_data = cor_data,
    top_correlations = cor_data %>%
      dplyr::arrange(dplyr::desc(abs_cor)) %>%
      dplyr::slice_head(n = 10)
  ))
}
#' Perform Principal Component Analysis
#'
#' Performs PCA on phenological parameters to identify main axes of variation.
#'
#' @param parameters A data frame containing extracted phenological parameters
#' @return A PCA result object
#' @export
#' @examples
#' \dontrun{
#' data(pheno_parameters)
#' pca_results <- perform_pca_analysis(pheno_parameters)
#' }
perform_pca_analysis <- function(parameters) {
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    stop("Package 'FactoMineR' is required for PCA")
  }
  # Prepare data
  pca_data <- parameters %>%
    dplyr::select(sos, eos, los, ndvi_max, gpp_total,
                  mean_temperature, total_precipitation) %>%
    stats::na.omit() %>%
    scale()
  # Perform PCA
  pca_result <- FactoMineR::PCA(pca_data, graph = FALSE)
  return(pca_result)
}
#' Detect Anomalies in Phenological Parameters
#'
#' Identifies anomalous observations based on z-scores and IQR methods.
#'
#' @param parameters A data frame containing extracted phenological parameters.
#'   Must include columns: species, site, sos, eos, los.
#' @param threshold Numeric threshold for z-score anomaly detection (default = 2.5).
#'   Observations with z-scores beyond ±threshold are flagged as anomalies.
#' @return A data frame with anomaly flags including:
#'   \item{sos_anomaly}{Logical flag for start-of-season anomalies}
#'   \item{eos_anomaly}{Logical flag for end-of-season anomalies}
#'   \item{los_anomaly}{Logical flag for length-of-season anomalies}
#'   \item{anomaly_score}{Numeric score (0-3) of total anomalies detected}
#'   \item{sos_z, eos_z, los_z}{Z-scores for each parameter}
#' @export
#' @examples
#' \dontrun{
#' data(pheno_parameters)
#' anomalies <- detect_anomalies(pheno_parameters)
#' anomalies_strict <- detect_anomalies(pheno_parameters, threshold = 3.0)
#' }
detect_anomalies <- function(parameters, threshold = 2.5) {
  # Ensure we have las columnas necesarias
  required_cols <- c("species", "site", "sos", "eos", "los")
  missing_cols <- setdiff(required_cols, names(parameters))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  anomalies <- parameters %>%
    dplyr::group_by(species, site) %>%
    dplyr::mutate(
      # Método 1: Usar mediana y MAD (Median Absolute Deviation) - más robusto
      sos_median = stats::median(sos, na.rm = TRUE),
      sos_mad = stats::mad(sos, na.rm = TRUE),
      sos_mad = ifelse(sos_mad == 0, 1, sos_mad),  # Evitar división por 0
      sos_z_mad = (sos - sos_median) / sos_mad,

      eos_median = stats::median(eos, na.rm = TRUE),
      eos_mad = stats::mad(eos, na.rm = TRUE),
      eos_mad = ifelse(eos_mad == 0, 1, eos_mad),
      eos_z_mad = (eos - eos_median) / eos_mad,

      los_median = stats::median(los, na.rm = TRUE),
      los_mad = stats::mad(los, na.rm = TRUE),
      los_mad = ifelse(los_mad == 0, 1, los_mad),
      los_z_mad = (los - los_median) / los_mad,

      # Método 2: Percentiles (más simple)
      sos_anomaly_perc = sos < stats::quantile(sos, 0.05, na.rm = TRUE) |
        sos > stats::quantile(sos, 0.95, na.rm = TRUE),
      eos_anomaly_perc = eos < stats::quantile(eos, 0.05, na.rm = TRUE) |
        eos > stats::quantile(eos, 0.95, na.rm = TRUE),
      los_anomaly_perc = los < stats::quantile(los, 0.05, na.rm = TRUE) |
        los > stats::quantile(los, 0.95, na.rm = TRUE),

      # Método 3: IQR (Interquartile Range)
      sos_iqr_lower = stats::quantile(sos, 0.25, na.rm = TRUE) - 1.5 * stats::IQR(sos, na.rm = TRUE),
      sos_iqr_upper = stats::quantile(sos, 0.75, na.rm = TRUE) + 1.5 * stats::IQR(sos, na.rm = TRUE),
      sos_anomaly_iqr = sos < sos_iqr_lower | sos > sos_iqr_upper,

      # Combinar métodos: Si cualquiera de los métodos detecta una anomalía, es anomalía
      sos_anomaly = sos_anomaly_perc | sos_anomaly_iqr | abs(sos_z_mad) > threshold,
      eos_anomaly = eos_anomaly_perc | abs(eos_z_mad) > threshold,
      los_anomaly = los_anomaly_perc | abs(los_z_mad) > threshold,

      # Para mantener compatibilidad con tests anteriores
      sos_z = (sos - mean(sos, na.rm = TRUE)) / ifelse(sd(sos, na.rm = TRUE) == 0, 1, sd(sos, na.rm = TRUE)),
      eos_z = (eos - mean(eos, na.rm = TRUE)) / ifelse(sd(eos, na.rm = TRUE) == 0, 1, sd(eos, na.rm = TRUE)),
      los_z = (los - mean(los, na.rm = TRUE)) / ifelse(sd(los, na.rm = TRUE) == 0, 1, sd(los, na.rm = TRUE)),

      # Overall anomaly score
      anomaly_score = as.numeric(sos_anomaly) +
        as.numeric(eos_anomaly) +
        as.numeric(los_anomaly)
    ) %>%
    dplyr::ungroup() %>%
    # Limpiar columnas auxiliares
    dplyr::select(-sos_median, -sos_mad, -sos_z_mad, -sos_anomaly_perc,
                  -sos_iqr_lower, -sos_iqr_upper, -sos_anomaly_iqr,
                  -eos_median, -eos_mad, -eos_z_mad, -eos_anomaly_perc,
                  -los_median, -los_mad, -los_z_mad, -los_anomaly_perc)

  # Asegurar que no haya NA
  anomalies <- anomalies %>%
    dplyr::mutate(
      sos_anomaly = ifelse(is.na(sos_anomaly), FALSE, sos_anomaly),
      eos_anomaly = ifelse(is.na(eos_anomaly), FALSE, eos_anomaly),
      los_anomaly = ifelse(is.na(los_anomaly), FALSE, los_anomaly),
      anomaly_score = ifelse(is.na(anomaly_score), 0, anomaly_score)
    )
  return(anomalies)
}
#' Run Complete Analysis Pipeline
#'
#' Executes a comprehensive analysis pipeline on phenological data.
#'
#' @param time_series Time series data (not always used, but kept for consistency)
#' @param parameters A data frame containing extracted phenological parameters
#' @return A list with all analysis results
#' @export
#' @examples
#' \dontrun{
#' data(pheno_time_series)
#' data(pheno_parameters)
#' results <- run_complete_analysis(pheno_time_series, pheno_parameters)
#' }
run_complete_analysis <- function(time_series, parameters) {
  cat("=== PHENOLOGICAL ANALYSIS PIPELINE ===\n\n")
  # 1. Mixed effects modeling
  cat("1. Running mixed effects model...\n")
  mixed_results <- analyze_mixed_effects(parameters)
  cat("   Marginal R2:", round(mixed_results$marginal_r2, 3), "\n")
  cat("   Conditional R2:", round(mixed_results$conditional_r2, 3), "\n\n")
  # 2. Trend analysis
  cat("2. Analyzing temporal trends...\n")
  trends <- analyze_temporal_trends(parameters)
  sig_trends <- trends %>%
    dplyr::filter(sos_p < 0.05 | eos_p < 0.05 | los_p < 0.05)
  cat("   Significant trends found:", nrow(sig_trends), "\n\n")
  # 3. Cluster analysis
  cat("3. Performing cluster analysis...\n")
  clusters <- perform_cluster_analysis(parameters)
  cat("   Cluster sizes:\n")
  print(table(clusters$clusters$cluster))
  cat("\n")
  # 4. Correlation analysis
  cat("4. Analyzing correlations...\n")
  corr_results <- analyze_correlations(parameters)
  cat("   Top correlations:\n")
  print(corr_results$top_correlations)
  # 5. PCA analysis
  cat("\n5. Performing PCA...\n")
  pca_results <- perform_pca_analysis(parameters)
  cat("   Variance explained by first 2 components:",
      round(sum(pca_results$eig[1:2, 2]), 1), "%\n")
  # 6. Anomaly detection
  cat("\n6. Detecting anomalies...\n")
  anomalies <- detect_anomalies(parameters)
  cat("   Anomalies detected:", sum(anomalies$anomaly_score > 0), "\n")
  return(list(
    mixed_model = mixed_results,
    trends = trends,
    clusters = clusters,
    correlations = corr_results,
    pca = pca_results,
    anomalies = anomalies
  ))
}

