# RCodeDissertation
# R Code for Dissertation Analysis

This repository contains all R scripts used for the dissertation project analyzing the effect of agricultural activities on the risk of RVF outbreaks in Uganda. The scripts are organized by research Aim and by task (sample size calculations, data cleaning, data processing, and analysis).

## Project Structure

- `R Code/`: All R scripts organized by research Aim  
- `Data/`: Input datasets are not included in this repo so file size and sensitivity
- `Output/`: Generated results, figures, and tables  

## File Naming Convention

Files are named according to the research Aim and task, for example:

| File Name           | Description                                      |
|--------------------|--------------------------------------------------|
| `Aim1_sample_size.R` | Sample size calculations for Aim 1             |
| `Aim1_data_cleaning.R` | Data cleaning and processing for Aim 1       |
| `Aim1_analysis.R`   | Analysis scripts for Aim 1                       |
| `Aim2_sample_size.R` | Sample size calculations for Aim 2             |
| `Aim2_data_cleaning.R` | Data cleaning and processing for Aim 2       |
| `Aim2_analysis.R`   | Analysis scripts for Aim 2                       |


## Installation

1. Install R (>= 4.3) from [CRAN](https://cran.r-project.org/)  
2. Install required packages:
```r
install.packages(c(
  "tidyverse",
  "data.table",
  "ggplot2",
  "lubridate",
  "sf",
  "terra",
  "xgboost",
  "caret",
  "doParallel",
  "ranger",
  "raster"
))

