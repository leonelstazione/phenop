#' Phenological Time Series Dataset
#'
#' A comprehensive synthetic dataset of weekly phenological observations
#' for 15 populations, 5 species, 7 sites, and 6 years (2018-2023).
#'
#' @format A data frame with X rows and 29 variables:
#' \describe{
#'   \item{observation_id}{Unique identifier for each observation}
#'   \item{population_id}{Population identifier}
#'   \item{species}{Species name or identifier}
#'   \item{site}{Site name or identifier}
#'   \item{year}{Year of observation}
#'   \item{date}{Date of observation (YYYY-MM-DD)}
#'   \item{doy}{Day of year (1-365/366)}
#'   \item{month}{Month of observation (1-12)}
#'   \item{season}{Seasonal classification}
#'   \item{latitude}{Latitude coordinate in decimal degrees}
#'   \item{longitude}{Longitude coordinate in decimal degrees}
#'   \item{elevation}{Elevation above sea level (meters)}
#'   \item{soil_type}{Classification of soil type}
#'   \item{soil_pH}{Soil pH measurement}
#'   \item{slope}{Terrain slope in degrees}
#'   \item{ndvi}{Normalized Difference Vegetation Index value}
#'   \item{evi}{Enhanced Vegetation Index value}
#'   \item{gpp}{Gross Primary Production value}
#'   \item{lai}{Leaf Area Index value}
#'   \item{temperature}{Temperature measurement (°C)}
#'   \item{precipitation}{Precipitation measurement (mm)}
#'   \item{gdd}{Growing Degree Days accumulated}
#'   \item{vegetation_health}{Vegetation health index value}
#'   \item{sos_adj}{Adjusted start of season indicator}
#'   \item{eos_adj}{Adjusted end of season indicator}
#'   \item{budburst}{Budburst phenophase indicator}
#'   \item{flowering}{Flowering phenophase indicator}
#'   \item{senescence}{Senescence phenophase indicator}
#'   \item{quality_flag}{Data quality flag}
#' }
#' @source Generated synthetically for the phenop package
#' @examples
#' data(pheno_time_series)
#' summary(pheno_time_series$ndvi)
#' ggplot2::ggplot(pheno_time_series, ggplot2::aes(x = doy, y = ndvi)) +
#'   ggplot2::geom_line(alpha = 0.3)
"pheno_time_series"
#' Extracted Phenological Parameters
#'
#' Derived phenological metrics extracted from time series analysis.
#'
#' @format A data frame with X rows and 44 variables:
#' \describe{
#'   \item{parameter_id}{Unique identifier for each parameter record}
#'   \item{site}{Site name or identifier where observations were made}
#'   \item{species}{Species name or identifier}
#'   \item{year}{Year of observation}
#'   \item{population_id}{Population identifier for the observed group}
#'   \item{n_observations}{Number of individual observations aggregated}
#'   \item{sos}{Start of season (day of year when growth begins)}
#'   \item{eos}{End of season (day of year when growth ends)}
#'   \item{pos}{Peak of season (day of year of maximum growth)}
#'   \item{los}{Length of season (number of days between SOS and EOS)}
#'   \item{ndvi_max}{Maximum Normalized Difference Vegetation Index value}
#'   \item{ndvi_min}{Minimum Normalized Difference Vegetation Index value}
#'   \item{ndvi_amplitude}{Difference between maximum and minimum NDVI}
#'   \item{ndvi_mean}{Mean Normalized Difference Vegetation Index value}
#'   \item{ndvi_sd}{Standard deviation of NDVI values}
#'   \item{ndvi_cv}{Coefficient of variation of NDVI values}
#'   \item{ndvi_integral}{Cumulative NDVI over the growing season}
#'   \item{gpp_total}{Total Gross Primary Production (cumulative)}
#'   \item{gpp_max}{Maximum daily Gross Primary Production}
#'   \item{lai_max}{Maximum Leaf Area Index value}
#'   \item{greenup_rate}{Rate of green-up (rate of increase in vegetation)}
#'   \item{senescence_rate}{Rate of senescence (rate of vegetation decline)}
#'   \item{mean_temperature}{Mean temperature during the growing season (°C)}
#'   \item{total_precipitation}{Total precipitation during growing season (mm)}
#'   \item{mean_gdd}{Mean Growing Degree Days}
#'   \item{total_gdd}{Total Growing Degree Days accumulated}
#'   \item{budburst_doy}{Day of year when budburst occurred}
#'   \item{flowering_doy}{Day of year when flowering occurred}
#'   \item{senescence_doy}{Day of year when senescence began}
#'   \item{data_completeness}{Percentage of complete data records}
#'   \item{elevation}{Elevation above sea level (meters)}
#'   \item{latitude}{Latitude coordinate in decimal degrees}
#'   \item{longitude}{Longitude coordinate in decimal degrees}
#'   \item{soil_type}{Classification of soil type}
#'   \item{soil_pH}{Soil pH measurement}
#'   \item{slope}{Terrain slope in degrees}
#'   \item{phenology_type}{Type of phenology (e.g., early, late, etc.)}
#'   \item{productivity_class}{Productivity classification (low, medium, high)}
#'   \item{sos_anomaly}{Deviation of SOS from long-term average}
#'   \item{eos_anomaly}{Deviation of EOS from long-term average}
#'   \item{los_anomaly}{Deviation of LOS from long-term average}
#'   \item{water_use_efficiency}{Water use efficiency metric}
#'   \item{temperature_efficiency}{Temperature use efficiency metric}
#'   \item{sos_trend}{Temporal trend in SOS over years}
#'   \item{eos_trend}{Temporal trend in EOS over years}
#' }
"pheno_parameters"
#' Spatial Phenological Data
#'
#' Gridded spatial data for phenological mapping and spatial analysis.
#'
#' @format A data frame with X rows and 24 variables:
#' \describe{
#'   \item{site_id}{Unique identifier for the spatial site}
#'   \item{longitude}{Longitude coordinate in decimal degrees}
#'   \item{latitude}{Latitude coordinate in decimal degrees}
#'   \item{elevation}{Elevation above sea level (meters)}
#'   \item{mean_annual_temp}{Mean annual temperature (°C)}
#'   \item{annual_precipitation}{Total annual precipitation (mm)}
#'   \item{soil_type}{Classification of soil type}
#'   \item{soil_pH}{Soil pH measurement}
#'   \item{soil_organic_carbon}{Soil organic carbon content (g/kg)}
#'   \item{sos}{Start of season (day of year)}
#'   \item{eos}{End of season (day of year)}
#'   \item{los}{Length of season (days)}
#'   \item{ndvi_max}{Maximum Normalized Difference Vegetation Index value}
#'   \item{land_cover}{Land cover classification type}
#'   \item{forest_type}{Forest type classification}
#'   \item{protected_area}{Indicates if the site is in a protected area}
#'   \item{years_since_disturbance}{Years since last disturbance event}
#'   \item{disturbance_type}{Type of disturbance (fire, logging, etc.)}
#'   \item{climate_zone}{Climate zone classification}
#'   \item{phenology_class}{Phenology classification category}
#'   \item{productivity}{Productivity metric value}
#'   \item{productivity_class}{Productivity classification category}
#'   \item{species_richness}{Number of species recorded at the site}
#'   \item{aboveground_carbon}{Aboveground carbon stock (Mg/ha)}
#' }
"pheno_spatial"

