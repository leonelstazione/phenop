---
title: "phenop: An R package for multidimensional analysis of phenotypic plasticity in eco-evolutionary contexts"
tags:
  - R
  - phenotypic plasticity
  - phenology
  - eco-evolutionary dynamics
  - multidimensional analysis
authors:
  - name: Leonel Stazione
    orcid: "0000-0002-5415-8124"
    affiliation: "1"
affiliations:
  - index: 1
    name: Department of Agricultural, Food, Environmental and Forestry Science and Technology (DAGRI), University of Florence, Italy
date: 23 January 2025
bibliography: paper.bib
---

## Summary

Phenotypic plasticity—the ability of organisms to modify their phenotype in response to environmental variation—is a fundamental mechanism shaping ecological interactions, evolutionary trajectories, and organismal responses to global change [@Scheiner1993; @Pigliucci2006; @Ghalambor2007]. Plastic responses often involve coordinated changes across multiple traits and environments, yet most analytical approaches treat plasticity as a univariate phenomenon, limiting biological interpretation and cross-study comparability.

**phenop** is an open-source R package designed to analyze phenotypic plasticity as a multidimensional property across traits, environmental gradients, and biological hierarchies. The package provides standardized tools to quantify, compare, and visualize coordinated plastic responses using original indices such as the Multidimensional Plasticity Index (MPI), alongside statistical and graphical methods tailored to eco-evolutionary research. phenop supports applications ranging from phenological responses to climate variation [@Nicotra2010; @Fitchett2015; @Menzel2006] to host–pathogen interaction studies, offering a unified and reproducible framework for plasticity analysis within the R ecosystem [@RCoreTeam2024].

## Statement of need

Research on phenotypic plasticity spans evolutionary ecology, climate change biology, agriculture, and conservation science. Plasticity metrics are widely used to predict species persistence under environmental change, evaluate adaptive potential, and identify resilient genotypes in managed systems [@Nicotra2010; @Ghalambor2007]. Despite this broad relevance, researchers face persistent analytical challenges. First, plasticity is frequently analyzed using single traits, ignoring coordinated multivariate responses. Second, available analytical tools are fragmented across general-purpose statistical and visualization packages. Third, the absence of standardized multidimensional metrics limits reproducibility and synthesis across studies.

Existing R packages provide partial solutions. Mixed-effects modeling frameworks support reaction norm analysis but do not implement plasticity-specific indices [@Bates2015]. Phenology-focused approaches emphasize temporal dynamics of individual traits [@Fitchett2015], while general multivariate tools lack ecology-oriented plasticity interpretations [@Legendre2012]. As a result, researchers often assemble custom workflows that are difficult to validate, reuse, or extend.

phenop addresses these limitations by offering a dedicated framework for multidimensional plasticity analysis. It integrates data preparation, plasticity quantification, statistical comparison, and visualization within a cohesive package. The target audience includes ecologists, evolutionary biologists, agricultural scientists, and environmental researchers working with phenotypic datasets involving multiple traits, environments, and grouping structures. By standardizing core analyses while remaining flexible to diverse study designs, phenop supports reproducible and comparable plasticity research.

## State of the field

Analytical approaches to phenotypic plasticity traditionally rely on reaction norms modeled using linear or mixed-effects models [@Bates2015]. While effective for single-trait analyses, these methods do not capture coordinated trait responses across environmental gradients. Multivariate statistical frameworks provide dimensionality reduction and ordination techniques [@Legendre2012], but they are not designed to quantify plasticity as a biological property.

Several R packages address specific aspects of plasticity-related research, such as phenological timing or environmental trend analysis [@Fitchett2015; @Menzel2006], but none provide an integrated solution for multidimensional plasticity. This fragmentation forces researchers to choose between analytical flexibility and biological interpretability.

phenop complements existing tools by focusing explicitly on plasticity as a multidimensional phenomenon. Rather than replacing established statistical frameworks, it builds upon them by introducing plasticity-specific indices, validation procedures, and visualization strategies tailored to ecological and evolutionary questions. This build-oriented approach provides a unique scholarly contribution by enabling analyses that are not readily achievable with existing packages alone.

## Software design

phenop follows a modular design that separates core plasticity calculations, statistical analysis, and visualization. This architecture supports flexible workflows while ensuring consistent data structures and outputs. Core functions implement multidimensional plasticity indices, including the Multidimensional Plasticity Index (MPI), which quantifies coordinated trait responses across environmental conditions.

The package emphasizes robustness and transparency. Input validation and informative error messages guide users through common data and design issues. Statistical assumptions and limitations are documented for each analytical function, supporting appropriate method selection. Visualization tools are designed specifically for communicating plasticity patterns, such as reaction norm plots and multidimensional heatmaps, and integrate with established R visualization standards [@Wickham2016; @Wickham2019].

phenop is implemented entirely in R (version ≥ 4.0.0) and integrates with widely used packages for data manipulation, modeling, and graphics [@RCoreTeam2024; @Bates2015; @Wood2004; @Wood2017; @Le2008; @Oksanen2022]. Performance considerations focus on typical ecological datasets, with optional parallelization for computationally intensive procedures. The codebase follows consistent naming and documentation standards to facilitate maintenance, extension, and community contributions.

## Research impact statement

phenop addresses a recognized methodological gap in plasticity research by providing standardized tools for multidimensional analysis. Its impact lies in enabling reproducible, comparable studies of coordinated trait responses—an increasingly important focus in eco-evolutionary research [@Nicotra2010; @Pigliucci2006].

The package includes curated example datasets and complete analytical workflows that demonstrate applications across biological contexts, including phenological responses to environmental variation and host–pathogen interaction systems. These materials provide immediate utility for researchers and support transparent benchmarking of methods. phenop has been developed openly on GitHub, with community-facing documentation and contribution guidelines that support early adoption and external use.

By lowering technical barriers to multidimensional plasticity analysis, phenop facilitates research on climate change responses, disease dynamics, and agricultural resilience. Its design prioritizes near-term applicability and reproducibility, positioning the software as a practical resource for current and future plasticity studies.

## AI usage disclosure

No generative AI tools were used in the development of the phenop software, the writing of this manuscript, or the preparation of any supporting materials.

## Acknowledgements

The author acknowledges institutional support from the University of Florence. No external funding was received specifically for this software.

## References
