phenop: Multidimensional Analysis of Phenotypic Plasticity

https://img.shields.io/badge/R-%253E%253D%25204.0.0-blue

https://img.shields.io/badge/license-MIT-green

https://img.shields.io/badge/status-active-success

https://img.shields.io/github/issues/leonelstazione/phenop

https://img.shields.io/github/stars/leonelstazione/phenop

<!-- Add when you have CI setup: --><!-- \[!\[R-CMD-check](https://github.com/leonelstazione/phenop/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/leonelstazione/phenop/actions/workflows/R-CMD-check.yaml) -->

Overview

phenop is an R package providing novel methods for analyzing phenotypic plasticity across multiple traits and environmental variables, with special focus on insect-fungus host-pathogen systems. It implements original multidimensional plasticity indices and interaction analyses not available in other packages.

The package bridges the gap between ecological, evolutionary, and phenotypic studies by offering comprehensive tools for plasticity analysis in eco-evolutionary contexts.

Key Features

Multidimensional Plasticity Indices - Novel metrics for assessing plasticity across multiple traits

Host-Pathogen Interaction Analysis - Specialized methods for insect-fungus systems

Environmental Optimization - Find optimal conditions considering multiple traits

Reaction Norm Visualization - Advanced plotting for genotype-environment interactions

Meta-Analysis Tools - Comprehensive analysis of plasticity studies

Trade-off Analysis - Quantify trade-offs between plasticities of different traits

Data Simulation - Generate realistic data for plasticity studies

Installation
From GitHub (Development Version)
# Install devtools if not already installed

if (!require("devtools")) install.packages("devtools")

\# Install phenop from GitHub

devtools::install\_github("leonelstazione/phenop")



Local Installation (from source)

\# Download the package source and install

devtools::install\_local("path/to/phenop")



Quick Start



\# Load the package

library(phenop)



\# Generate simulated plasticity data

sim\_data <- simulate\_plasticity\_data(

&nbsp; n\_genotypes = 10,

&nbsp; n\_environments = 5, 

&nbsp; n\_traits = 3

)



\# Perform G×E ANOVA for plasticity analysis

anova\_results <- anova\_plasticity(

&nbsp; data = sim\_data,

&nbsp; response = "trait\_value",

&nbsp; genotype = "genotype", 

&nbsp; environment = "environment"

)



\# Visualize reaction norms

plot\_reaction\_norm(

&nbsp; data = sim\_data,

&nbsp; trait = "trait1",

&nbsp; genotype = "genotype",

&nbsp; environment = "environment"

)



\# Calculate multidimensional plasticity index

mpi\_results <- multidim\_plasticity(

&nbsp; data = sim\_data,

&nbsp; traits = c("trait1", "trait2", "trait3"),

&nbsp; environment = "environment",

&nbsp; genotype = "genotype"

)



Main Functions



Core Analysis Functions

multidim\_plasticity() - Calculate Multidimensional Plasticity Index (MPI)

safe\_multidim\_plasticity() - Robust version with error handling

anova\_plasticity() - G×E ANOVA for phenotypic plasticity

plasticity\_tradeoffs() - Identify trade-offs between plasticities

plasticity\_meta\_analysis() - Meta-analysis of plasticity studies



Host-Pathogen Analysis

host\_pathogen\_interaction() - Analyze phenotypic plasticity in host-pathogen systems

host\_pathogen\_interaction\_extended() - Comprehensive analysis with extended features



Environmental Optimization

optimize\_multidim\_environment() - Find optimal environmental conditions

environmental\_optimization() - General environmental optimization



Visualization

plot\_multidim\_plasticity() - Visualize multidimensional plasticity

plot\_reaction\_norm() - Create reaction norm plots

plot\_multidim\_plasticity\_extended() - Advanced SAFE visualizations



Data Management

simulate\_plasticity\_data() - Generate realistic simulated data

Multiple datasets - pheno\_parameters, pheno\_spatial, pheno\_time\_series



Package Structure

phenop/

├── R/                          # Source code

│   ├── analyze.R              # Statistical analysis functions

│   ├── multidim\_plasticity.R  # Multidimensional plasticity indices

│   ├── host\_pathogen\_interaction.R  # Host-pathogen analysis

│   ├── visualize.R            # Visualization functions

│   └── ... (25+ additional files)

├── data/                      # Package datasets

│   ├── pheno\_parameters.rda   # Phenological parameters dataset

│   ├── pheno\_spatial.rda      # Spatial phenology data

│   └── pheno\_time\_series.rda  # Time series data

├── tests/                     # Test suite

│   └── testthat/              # Unit tests

├── vignettes/                 # Tutorials and examples

│   ├── phenop\_vignette.Rmd    # Main tutorial

│   └── phenop\_vignette\_complete.Rmd  # Complete demonstration

└── man/                       # Documentation


Example Analysis


Complete Workflow Example

library(phenop)

library(ggplot2)



\# Load example data

data("pheno\_parameters")



\# Analyze plasticity across environments

plasticity\_results <- multidim\_plasticity(

&nbsp; data = pheno\_parameters,

&nbsp; traits = c("sos", "eos", "los", "ndvi\_max"),

&nbsp; environment = "year",

&nbsp; genotype = "site"

)



\# Visualize results

plot\_multidim\_plasticity(plasticity\_results)



\# Perform meta-analysis of plasticity

meta\_results <- plasticity\_meta\_analysis(

&nbsp; data = plasticity\_results,

&nbsp; effect\_size = "plasticity\_index",

&nbsp; study\_id = "site"

)



\# Analyze trade-offs

tradeoff\_results <- plasticity\_tradeoffs(

&nbsp; data = plasticity\_results,

&nbsp; traits = c("sos", "eos", "los")

)



Host-Pathogen System Analysis



\# Analyze insect-fungus interactions

host\_pathogen\_results <- host\_pathogen\_interaction(

&nbsp; data = fungi\_insect\_data,

&nbsp; host\_trait = "host\_size",

&nbsp; pathogen\_trait = "infection\_rate",

&nbsp; environment = "temperature"

)



\# Extended analysis with multiple traits

extended\_results <- host\_pathogen\_interaction\_extended(

&nbsp; data = fungi\_insect\_data,

&nbsp; host\_traits = c("host\_size", "immune\_response"),

&nbsp; pathogen\_traits = c("infection\_rate", "sporulation"),

&nbsp; environmental\_gradient = "temperature"

)



Datasets Included



pheno\_parameters

Size: ~1000 observations, 44 variables

Description: Comprehensive dataset of phenological parameters across multiple sites and species

Variables: Includes SOS, EOS, LOS, NDVI metrics, GPP, environmental variables, soil properties

Use: General plasticity analysis, environmental correlations

pheno\_spatial

Size: 150 spatial features, 24 variables

Description: Spatial dataset with geographical and ecological context

Variables: Coordinates, elevation, climate, land cover, disturbance history

Use: Spatial analysis, landscape-scale plasticity studies

pheno\_time\_series

Size: Time series data with 29 variables

Description: Temporal observations of phenological metrics

Variables: Daily/seasonal measurements, vegetation indices, climate data

Use: Temporal trends, seasonality analysis, time-series plasticity

fungi\_insect\_data

Size: Simulated dataset for host-pathogen interactions

Description: Phenotypic plasticity in fungus-insect host-pathogen systems

Variables: Host traits, pathogen traits, environmental conditions

Use: Host-pathogen coevolution, infection dynamics



Technical Details


Dependencies

Imports:

&nbsp; dplyr (>= 1.0.0), ggplot2 (>= 3.4.0), lme4 (>= 1.1.0),

&nbsp; mgcv (>= 1.8.0), viridis (>= 0.6.0), patchwork (>= 1.1.0),

&nbsp; ggridges (>= 0.5.0), maps (>= 3.4.0), performance (>= 0.10.0),

&nbsp; FactoMineR (>= 2.4.0), cluster (>= 2.1.0), MASS (>= 7.3.0),

&nbsp; magrittr (>= 2.0.3), plotly (>= 4.10.0), sf, tidyr, stats, methods



System Requirements

R version >= 4.0.0

100MB disk space (primarily for example datasets)

Standard R installation capabilities

Performance

Optimized for datasets with up to 10^5 observations

Parallel processing capabilities for large analyses

Memory-efficient implementations for multidimensional calculations



Documentation
Vignettes

\# Access built-in tutorials

vignette("phenop\_vignette", package = "phenop")

vignette("phenop\_vignette\_complete", package = "phenop")



Help System
# Access function documentation

?multidim\_plasticity

?plot\_reaction\_norm

?pheno\_parameters

\# List all exported functions

ls("package:phenop")



Testing
The package includes comprehensive tests covering:

Functionality verification

Input validation

Output structure checks

Edge case handling

Performance benchmarks

Run tests with:

devtools::test("phenop")


Contributing
We welcome contributions! Please follow these steps:

Fork the repository on GitHub

Clone your fork locally

Create a branch for your feature (git checkout -b feature/amazing-feature)

Commit your changes (git commit -m 'Add amazing feature')

Push to the branch (git push origin feature/amazing-feature)

Open a Pull Request



Contribution Guidelines

Follow the tidyverse style guide

Add tests for new functionality

Update documentation accordingly

Ensure compatibility with existing functions



Citation


If you use phenop in your research, please cite:



@software{phenop\_package,

&nbsp; title = {phenop: Multidimensional Analysis of Phenotypic Plasticity},

&nbsp; author = {Leonel Stazione},

&nbsp; year = {2026},

&nbsp; url = {https://github.com/leonelstazione/phenop},

&nbsp; note = {R package version 0.1.0}

}



License
This package is licensed under the MIT License. See the LICENSE file for details.



Bug Reports and Issues
Please report bugs, issues, or feature requests on the GitHub Issues page.



Contact
Author: Leonel Stazione
Email: leonelstazione@hotmail.com
GitHub: leonelstazione



Acknowledgments

This package was developed as part of research on phenotypic plasticity in insect-fungus systems. Special thanks to contributors and testers.

