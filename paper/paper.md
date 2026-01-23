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
    orcid: 0000-0002-5415-8124
    affiliation: 1
affiliations:
  - index: 1
    name: Department of Agricultural, Food, Environmental and Forestry Science and Technology (DAGRI), University of Florence, Italy
date: 23 January 2026
bibliography: paper.bib
---

## Summary

Phenotypic plasticity describes the ability of organisms to modify their phenotype in response to environmental variation and is a central concept in ecology and evolution [@Nicotra2010; @Pigliucci2006]. These responses often involve coordinated changes across multiple traits rather than isolated adjustments of single characteristics. However, most analytical tools available in R focus on univariate reaction norms or trait-specific models, limiting inference in studies where plasticity emerges from multivariate trait interactions.

**phenop** is an R package designed to analyze phenotypic plasticity as a multidimensional phenomenon across traits, environmental gradients, and hierarchical biological structures. The package provides integrated workflows for exploratory analysis, statistical modeling, and visualization of plasticity patterns. It is intended for researchers in ecology, evolution, agriculture, and environmental sciences who require reproducible tools to study complex phenotypic responses in eco-evolutionary contexts.

## Statement of need

Phenotypic plasticity plays a central role in ecological and evolutionary research, particularly in studies of phenology, climate change responses, host–pathogen interactions, and adaptive strategies under environmental stress [@Scheiner1993; @Ghalambor2007]. Standard analytical approaches typically rely on reaction norms or mixed-effects models applied to individual traits, which limits their ability to capture coordinated plastic responses across trait complexes [@Bates2015].

Several R packages support components of plasticity analysis, such as phenological modeling or hierarchical regression frameworks. However, these tools generally require extensive custom integration and do not provide standardized metrics or workflows for multidimensional plasticity [@Fitchett2015]. As a result, researchers often develop ad hoc solutions that reduce reproducibility and comparability across studies.

**phenop** addresses this gap by providing a unified framework for analyzing phenotypic plasticity across multiple traits and environmental variables. The target audience includes ecologists, evolutionary biologists, and applied researchers working with phenotypic, phenological, or environmental datasets who require multivariate, reproducible, and extensible analytical tools.

## State of the field

Existing software for plasticity analysis in R primarily focuses on single-trait reaction norms, phenological trend estimation, or generalized regression frameworks [@Menzel2006; @Fitchett2015]. While these approaches are well established, they do not explicitly model plasticity as a multivariate property emerging from coordinated trait responses. In addition, no widely used package provides integrated tools tailored to eco-evolutionary systems such as host–pathogen interactions, where plasticity can influence infection dynamics and fitness outcomes.

Rather than extending existing single-purpose packages, **phenop** was developed as a dedicated framework for multidimensional plasticity analysis. This design choice enables standardized estimation of multivariate plasticity metrics, consistent visualization of plasticity spaces, and seamless integration of hierarchical and non-linear models within a single analytical workflow, building on established principles of multivariate analysis and numerical ecology [@Legendre2012].

## Software design

The design of **phenop** emphasizes modularity, reproducibility, and interpretability. Analytical components are organized as interoperable modules that can be combined into flexible workflows while maintaining consistent data validation and output structures. This approach allows users to perform exploratory analyses, apply model-based inference, and visualize results without extensive custom coding.

The package prioritizes robust statistical methods and transparent assumptions, supporting both linear and non-linear relationships as well as hierarchical data structures through established modeling frameworks [@Bates2015; @Wood2017]. Comprehensive automated testing and extensive documentation, including worked examples and vignettes, were implemented to ensure software reliability and facilitate reuse by the research community.

## Research impact statement

**phenop** provides a reusable and extensible framework for studying phenotypic plasticity in complex eco-evolutionary systems. It supports applications ranging from phenological responses to climate change [@Nicotra2010; @Menzel2006] and agricultural stress experiments to host–pathogen interaction studies and evolutionary ecology research [@Ghalambor2007].

The package includes curated example datasets and reproducible analysis pipelines to lower barriers to adoption and promote methodological consistency. Its open-source development model follows best practices established in the R ecosystem [@Wickham2019], supporting near-term reuse in research and teaching contexts and facilitating future community-driven extensions.

## Acknowledgements

The author acknowledges institutional support from the University of Florence. No external funding was received specifically for this software.

## References
