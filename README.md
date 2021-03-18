
# packaging

<!-- badges: start -->
<!-- badges: end -->

`packaging` includes four functions whose uses are tied to statistical inference and statistical prediction. The package includes functions for conducting t-tests, producing linear regression models, finding the cross-validation misclassification error for k-nearest neighbors, and finding the cross-validation mean squared error for random forest models. This package also comes with the `palmerpenguins` and `gapminder` datasets.

## Installation

To use this package, copy:

```r
install.packages("packaging")
library(packaging)
```
Or, install straight from GitHub:

```r
# install.packages("devtools")
devtools::install_github("samcolgan/packaging")
library(packaging)
```

## Use

This vignette demonstrates some sample uses of the functions in this package.
Copy the code below to access the package and view the vignette.

```r
devtools::install_github("samcolgan/packaging", build_vignette = TRUE, build_opts = c())
library(packaging)

# Use this to view the vignette in the packaging HTML help
help(package = "packaging", help_type = "html")

# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "packaging")
```
