
<!-- README.md is generated from README.Rmd. Please edit that file -->
bis557
======

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/WillDuke/bis557.svg?branch=master)](https://travis-ci.org/WillDuke/bis557) <!-- badges: end -->

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
data("iris")
ridge_regression(Sepal.Length ~ ., iris, lambda = 0.1)
#>       Sepal.Width      Petal.Length       Petal.Width Speciesversicolor 
#>         0.2185481         1.4280167        -0.2345565        -0.3184238 
#>  Speciesvirginica 
#>        -0.4518442 
#> attr(,"formula")
#> Sepal.Length ~ .
#> attr(,"class")
#> [1] "numeric"          "ridge_regression"
```

Vignettes in this package include answers to homework problems from the CASL book and class as well as common use-cases for some of the functions in the package.
