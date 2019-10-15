
<!-- README.md is generated from README.Rmd. Please edit that file -->
bis557
======

<!-- badges: start -->
<!-- badges: end -->
The goal of bis557 is to collate my function implementations, vignettes, and other files from Professor Michael Kane's Computational Statistics (BIS557) taught Fall 2019.

Installation
------------

You can install the development version of bis557 from [GitHub](https://github.com/WillDuke/bis557.git) with:

``` r
# install.packages("devtools")
devtools::install_github("WillDuke/bis557")
```

Example
-------

This package includes a variety of functions including a minimal implementation of ridge\_regression:

``` r
library(bis557)
data("iris")
ridge_regression(Sepal.Length ~ ., iris, lambda = 0.1)
#>            [,1]
#> [1,]  1.9824769
#> [2,]  0.5602867
#> [3,]  0.8115254
#> [4,] -0.3672007
#> [5,] -0.5675260
#> [6,] -0.8207683
#> attr(,"formula")
#> Sepal.Length ~ .
#> attr(,"class")
#> [1] "matrix"           "ridge_regression"
```

Vignettes in this package include answers to homework problems from the CASL book and class as well as common use-cases for some of the functions in the package.
