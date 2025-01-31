# Supplementary Statistical Functions

This repository contains a collection of supplementary statistical functions designed to enhance data analysis and visualization in R. The functions leverage various R packages to provide advanced statistical tools and visualizations.

## Installation

Before using the functions, ensure that you have installed the necessary R packages. You can install them using the following command:

```r
install.packages(c("ggprism", "ggplot2", "ggpubr", "gridExtra", "RColorBrewer", "rstatix", "ggmosaic", "ggstatsplot", "e1071"))
```

Then, load the libraries in your R script:

```r
library(ggprism)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(RColorBrewer)
library(rstatix)
library(ggmosaic)
library(ggstatsplot)
library(e1071)
```

## Usage

This repository provides various functions for statistical analysis and visualization. Below is a brief overview:

- **ggprism**: Enhances ggplot2 plots with scientific journal-style themes.
- **ggplot2**: Creates elegant data visualizations.
- **ggpubr**: Simplifies publication-ready plots.
- **gridExtra**: Combines multiple plots into a grid layout.
- **RColorBrewer**: Provides color palettes for better visual aesthetics.
- **rstatix**: Facilitates easy and tidy statistical analysis.
- **ggmosaic**: Creates mosaic plots for categorical data.
- **ggstatsplot**: Generates statistical plots with ggplot2.
- **e1071**: Includes functions for machine learning and statistical modeling.

## Examples

To create a basic ggplot2 visualization:

```r

set.seed(12345)   
source("Extended_Statistical_Toolkit_functions.R")


##########################################
##### Continuous data with outliers ######

# Generate a normal distribution
data <- rnorm(100, mean = 50, sd = 10)  

# Compute IQR
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)
IQR_val <- Q3 - Q1

# Define outlier thresholds
lower_bound <- Q1 - 3 * IQR_val
upper_bound <- Q3 + 3 * IQR_val

# Introduce outliers beyond 3×IQR
outliers <- c(runif(3, min = lower_bound - 10, max = lower_bound - 5), 
              runif(3, min = upper_bound + 5, max = upper_bound + 10))

# Combine the original data with outliers
data_with_outliers <- c(data, outliers)

rare_values <- outliers_rare_values_detection(x = data_with_outliers, detenction_method = "non_parametric", N_IQR = 3, min_percent_cat = 1 )	
print(rare_values)
boxplot(data_with_outliers)

########################################
##### Discrete data with outliers ######

data_cat <- rep(c("a","b"),100)
data_cat <- c(data_cat,"c")
rare_values_cat <- outliers_rare_values_detection(x = data_cat, detenction_method = "non_parametric", N_IQR = 3, min_percent_cat = 1 )	
print(rare_values_cat)
table(data_cat)


```

## Contributing

Contributions are welcome! Feel free to submit issues, feature requests, or pull requests to improve this repository.

## Contact

For any questions or collaboration opportunities, please reach out via GitHub issues.

---



