
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cursory

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/halpo/cursory.svg?branch=master)](https://travis-ci.org/halpo/cursory)
[![`Codecov` test
coverage](https://codecov.io/gh/halpo/cursory/branch/master/graph/badge.svg)](https://codecov.io/gh/halpo/cursory?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of cursory is to make it easier to summarize data and look at
your variables. It builds off [`dplyr`](http://dplyr.tidyverse.org) and
[`purrr`](http://purrr.tidyverse.org). It is also compatible with
[`dbplyr`](http://dbplyr.tidyverse.org) and remote data.

## Installation

You can install the released version of cursory from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("cursory")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("halpo/cursory")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)
library(cursory)
data(iris)

## basic summary statistics for each variable in a data frame.
cursory_all(group_by(iris, Species), lst(mean, median)) %>% ungroup() 
```

| Variable     | Species    |  mean | median |
| :----------- | :--------- | ----: | -----: |
| Sepal.Length | setosa     | 5.006 |   5.00 |
| Sepal.Length | versicolor | 5.936 |   5.90 |
| Sepal.Length | virginica  | 6.588 |   6.50 |
| Sepal.Width  | setosa     | 3.428 |   3.40 |
| Sepal.Width  | versicolor | 2.770 |   2.80 |
| Sepal.Width  | virginica  | 2.974 |   3.00 |
| Petal.Length | setosa     | 1.462 |   1.50 |
| Petal.Length | versicolor | 4.260 |   4.35 |
| Petal.Length | virginica  | 5.552 |   5.55 |
| Petal.Width  | setosa     | 0.246 |   0.20 |
| Petal.Width  | versicolor | 1.326 |   1.30 |
| Petal.Width  | virginica  | 2.026 |   2.00 |

``` r

## summary statistics for only numeric variables. 
cursory_if(iris, is.numeric, lst(Mean = mean, 'Std. Deviation' = sd))
```

| Variable     |     Mean | Std. Deviation |
| :----------- | -------: | -------------: |
| Sepal.Length | 5.843333 |      0.8280661 |
| Sepal.Width  | 3.057333 |      0.4358663 |
| Petal.Length | 3.758000 |      1.7652982 |
| Petal.Width  | 1.199333 |      0.7622377 |

``` r

## summary statistics for specific variables. 
cursory_at(iris, vars(ends_with("Length")), var)
```

| Variable     |       var |
| :----------- | --------: |
| Sepal.Length | 0.6856935 |
| Petal.Length | 3.1162779 |

# `table_1`

The `cursory` package also provides a `table_1` function that allows for
describing variables of a dataset for different subsets automatically.
This is useful in creating the very common demographics “table 1”.

``` r
table_1(iris, Species)
```

| Variable     | Level  | (All) | setosa | versicolor | virginica |
| :----------- | :----- | :---- | :----- | :--------- | :-------- |
| Sepal.Length | Min    | 4.300 | 4.300  | 4.900      | 4.900     |
|              | Median | 5.800 | 5.000  | 5.900      | 6.500     |
|              | Mean   | 5.843 | 5.006  | 5.936      | 6.588     |
|              | Max    | 7.900 | 5.800  | 7.000      | 7.900     |
|              | SD     | 0.828 | 0.352  | 0.516      | 0.636     |
| Sepal.Width  | Min    | 2.000 | 2.300  | 2.000      | 2.200     |
|              | Median | 3.000 | 3.400  | 2.800      | 3.000     |
|              | Mean   | 3.057 | 3.428  | 2.770      | 2.974     |
|              | Max    | 4.400 | 4.400  | 3.400      | 3.800     |
|              | SD     | 0.436 | 0.379  | 0.314      | 0.322     |
| Petal.Length | Min    | 1.000 | 1.000  | 3.000      | 4.500     |
|              | Median | 4.300 | 1.500  | 4.300      | 5.500     |
|              | Mean   | 3.758 | 1.462  | 4.260      | 5.552     |
|              | Max    | 6.900 | 1.900  | 5.100      | 6.900     |
|              | SD     | 1.765 | 0.174  | 0.470      | 0.552     |
| Petal.Width  | Min    | 0.100 | 0.100  | 1.000      | 1.400     |
|              | Median | 1.300 | 0.200  | 1.300      | 2.000     |
|              | Mean   | 1.199 | 0.246  | 1.326      | 2.026     |
|              | Max    | 2.500 | 0.600  | 1.800      | 2.500     |
|              | SD     | 0.762 | 0.105  | 0.198      | 0.275     |

The `table_1()` function also tags the Variable column as a `dontrepeat`
class column which make repeating values in columns not appear when
formatted, so that tables are easier to read.
