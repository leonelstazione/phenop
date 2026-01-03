# phenop: Multidimensional Phenotypic Plasticity Analysis

<img src="man/figures/logo.png" align="right" height="139" alt="phenop logo" />

[![R-CMD-check](https://github.com/yourusername/phenop/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourusername/phenop/actions/workflows/R-CMD-check.yaml)
![tests](https://img.shields.io/badge/tests-90%20passed%2C%200%20failed-brightgreen)
![coverage](https://img.shields.io/badge/coverage-17.28%25-yellow)

An R package for analyzing phenotypic plasticity, genotype-environment interactions (G×E), and multidimensional adaptation patterns in evolutionary biology and ecology studies.

## Installation

```r
# Install from GitHub
devtools::install_github("yourusername/phenop")  # Replace "yourusername" with your GitHub username

# Load the package
library(phenop)

Quick Start

r
# 1. Generate example data
data <- simulate_plasticity_data(
  n_genotypes = 5,
  n_environments = 3,
  n_traits = 2
)

# 2. ANOVA analysis of plasticity
result_anova <- anova_plasticity(
  formula = trait1 ~ genotype * environment,
  data = data,
  return_type = "anova"
)

# 3. Calculate multidimensional plasticity index
result_mpi <- multidim_plasticity(
  data = data,
  traits = c("trait1", "trait2"),
  environments = "environment",
  groups = "genotype"
)

# 4. Visualize reaction norms
plot_reaction_norm(
  data = data,
  environment = environment,
  trait = trait1,
  genotype = genotype
)

Main Functions

Function	Description	Status
anova_plasticity()	ANOVA for G×E interactions	✅ 100% test coverage
multidim_plasticity()	Multidimensional Plasticity Index (MPI)	✅ 93.94% test coverage
plot_reaction_norm()	Reaction norm plots	✅ 100% test coverage
simulate_plasticity_data()	Plasticity data simulation	✅ 100% test coverage
plasticity_meta_analysis()	Meta-analysis of plasticity effects	🟡 0% coverage (in development)
optimize_multidim_environment()	Environment optimization	🟡 0% coverage (in development)
host_pathogen_interaction()	Host-pathogen interaction analysis	🟡 In development
plasticity_tradeoffs()	Trade-off analysis	✅ 17.78% coverage
safe_multidim_plasticity()	Error-tolerant plasticity analysis	✅ 51.28% coverage

Detailed Examples

Multidimensional Plasticity Analysis
r
# Calculate MPI for multiple traits
mpi_result <- multidim_plasticity(
  data = your_data,
  traits = c("growth_rate", "resistance", "fecundity"),
  environments = "temperature",
  groups = "population",
  weights = c(0.5, 0.3, 0.2)  # Optional weights
)

# View results
print(mpi_result$multidimensional_index)  # Global index
print(mpi_result$individual_plasticity)   # Plasticity by group
Plasticity Meta-Analysis
r
# Synthesize plasticity effects across studies
meta_result <- plasticity_meta_analysis(
  study_data = meta_data,
  effect_size = "hedges_g",
  variance = "variance",
  moderator = "taxon",
  method = "random"
)

# Visualize effects
# plot(meta_result)  # If plotting method exists
Environment Optimization
r
# Find optimal conditions for multiple traits
optimal <- optimize_multidim_environment(
  data = experimental_data,
  traits = c("yield", "quality", "stress_tolerance"),
  environments = c("temperature", "humidity", "nutrients"),
  groups = "genotype",
  optimization_goal = "compromise"
)

print(optimal$optimal_conditions)
Host-Pathogen Interaction Analysis
r
# Analyze host-pathogen interactions
interaction_result <- host_pathogen_interaction_extended(
  host_data = host_data,
  pathogen_data = pathogen_data,
  host_traits = c("resistance", "growth_rate"),
  pathogen_traits = c("virulence", "transmission_rate"),
  time_var = "time_point",
  environments = "treatment"
)

Project Status

Test Results
r
devtools::test()
# [ FAIL 0 | WARN 0 | SKIP 3 | PASS 90 ]
Code Coverage
r
library(covr)
package_coverage()
# phenop Coverage: 17.28%
# Key functions:
# - anova_plasticity.R: 100.00%
# - multidim_plasticity.R: 93.94%
# - plot_reaction_norm.R: 100.00%
# - simulate_plasticity_data.R: 100.00%
# - safe_multidim_plasticity.R: 51.28%

Package Structure

text
phenop/
├── R/                          # Source code
│   ├── anova_plasticity.R      # Fully tested
│   ├── multidim_plasticity.R   # 93.94% coverage
│   ├── plot_reaction_norm.R    # Fully tested
│   ├── simulate_plasticity_data.R  # Fully tested
│   ├── safe_multidim_plasticity.R  # 51.28% coverage
│   ├── plasticity_meta_analysis.R  # In development
│   ├── optimize_multidim_environment.R  # In development
│   ├── host_pathogen_interaction_extended.R  # In development
│   ├── plasticity_tradeoffs.R  # 17.78% coverage
│   ├── plasticity_tradeoffs_extended.R  # In development
│   ├── plasticity_meta_analysis_extended.R  # In development
│   ├── plot_multidim_plasticity.R  # 46.51% coverage
│   ├── plot_multidim_plasticity_extended.R  # In development
│   └── plot_internals.R        # Internal functions
├── tests/                      # Test suite
│   └── testthat/
│       ├── test_anova_plasticity.R
│       ├── test_multidim_plasticity.R
│       ├── test_plot_reaction_norm.R
│       ├── test_simulate_plasticity_data.R
│       ├── test_safe_multidim_plasticity.R
│       ├── test_plasticity_meta_analysis.R
│       ├── test_optimize_multidim_environment.R
│       ├── test_host_pathogen_interaction_extended.R
│       ├── test_plasticity_tradeoffs.R
│       ├── test_plasticity_tradeoffs_extended.R
│       ├── test_plasticity_meta_analysis_extended.R
│       ├── test_plot_multidim_plasticity.R
│       ├── test_plot_multidim_plasticity_extended.R
│       └── test_plot_internals.R
├── man/                        # Documentation
├── vignettes/                  # Tutorials (coming soon)
├── LICENSE                     # MIT License
└── README.md                   # This file

Development

Running Tests
r
# All tests
devtools::test()

# Specific test file
testthat::test_file("tests/testthat/test_multidim_plasticity.R")

# Tests by pattern
devtools::test(filter = "multidim")
Checking Coverage
r
library(covr)
cov <- package_coverage()
print(cov)                     # Summary
report(cov)                    # Interactive HTML report
zero_coverage(cov)             # Uncovered lines
Building Documentation
r
devtools::document()          # Update .Rd files
devtools::install()           # Install locally
devtools::check()             # Full package check
Adding New Features
r
# Create new function
usethis::use_r("new_function")

# Create corresponding test
usethis::use_test("new_function")

# Run tests for new function
devtools::test(filter = "new_function")

Learn More
Vignettes (Coming Soon)
r
# Install with vignettes
devtools::install_github("yourusername/phenop", build_vignettes = TRUE)

# View available vignettes
vignette(package = "phenop")
vignette("introduction-phenop")
Function Reference
r
# Help for specific functions
?anova_plasticity
?multidim_plasticity
?plot_reaction_norm
?simulate_plasticity_data
?safe_multidim_plasticity
Example Datasets
r
# List available datasets
data(package = "phenop")

# Load example dataset
data("demo_data")

Contributing

Contributions are welcome! Please:

Report bugs or suggestions as issues

Submit pull requests for improvements

Suggest new features

Contribution Guidelines
r
# 1. Fork the repository
# 2. Clone your fork locally
# 3. Create a branch for your feature
# 4. Develop with tests
# 5. Run devtools::check()
# 6. Submit pull request
Development Setup
r
# Clone and setup
git clone https://github.com/yourusername/phenop.git
cd phenop
Rscript -e "devtools::install_deps()"
Rscript -e "devtools::load_all()"

Citation

If you use phenop in your research, please cite:

Leonel Stazione (2026). phenop: Multidimensional Phenotypic Plasticity Analysis. R package version 0.1.0.

BibTeX entry:

bibtex
@Manual{phenop2026,
  title = {phenop: Multidimensional Phenotypic Plasticity Analysis},
  author = {LEonel Stazione},
  year = {2026},
  note = {R package version 0.1.0},
  url = {https://github.com/yourusername/phenop},
}

License

MIT License - see LICENSE file for details.

Contact

Author: Leonel Stazione

GitHub: @leonelstazione

Issues: Report issues

Email: leoneldaniel.stazione@unifi.it

Roadmap

Version 0.2.0 (Planned)
Improve coverage to >50%

Add vignettes and tutorials

Implement missing function features

Add more example datasets

Version 1.0.0 (Future)
Complete all function implementations

Achieve >80% test coverage

CRAN submission

Comprehensive documentation