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
df <- data.frame(Group = rep(c("A", "B"), each = 10), Value = rnorm(20))

p <- ggplot(df, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  theme_prism()

print(p)
```

## Contributing

Contributions are welcome! Feel free to submit issues, feature requests, or pull requests to improve this repository.

## Contact

For any questions or collaboration opportunities, please reach out via GitHub issues.

---



