
<!-- README.md is generated from README.Rmd. Please edit that file -->

## About

This repository contains all data and R code to reproduce the data
processing, analysis, and visualization for our study 
*“Non-native plants attain native levels of microherbivory richness with time and range expansion”*.

## Repository Structure

- **`R-Scripts/`**: Contains nine R scripts for data processing and
  analysis.

  - **`00-preamble.R`**: Loads necessary packages and custom functions.
  - **`01-data-prep.R`**: Loads the microherbivore data, identifies nonnative plants, etc.
  - **`02-plant-origin.R`**: Test the difference between native and non-native plants.
  - **`03-introduction-date.R`**: Gets data on introduction date ready.
  - **`04-range-size.R`**: Quantifies nonnatives range sizes in Europe using GBIF.
  - **`05-relatedness.R`**: Are congeners present in Europe?
  - **`06-range-proximity.R`**: How far away is the native range of non-native plants?
  - **`07-Statistical-analysis.R`**: Models and visualization of all drivers of microherbivory richness on non-natives.
  - **`08-non-native-generalists.R`**: Are non-natives interacting with more generalist microherbivores?
  - **`09-germanflora-traits.R`**: Checks for variables potentially confounding the range size effect.

- **`Data/`**: Contains all input, output, and intermediary data files.

- **`Figures/`**: Contains all figures generated in the analysis.

## Contact

Please contact me at <ingmar.staude@uni-leipzig.de> if you have further
questions.
