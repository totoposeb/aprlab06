
# aprlab06

<!-- badges: start -->
[![R-CMD-check](https://github.com/totoposeb/aprlab06/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/totoposeb/aprlab06/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository contains an R package with three algorithms that solve the [knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem): `brute_force_knapsack()`, `knapsack_dynamic()` and `greedy_knapsack()`

## Installation

You can install the development version of aprlab06 from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
options(pkg.build_vignettes = TRUE)
pak::pak("totoposeb/aprlab06")
```

## Example

This is a basic example which shows you how to solve the knapsack problem with the `knapsack_objects` data:

``` r
library(aprlab06)
brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
greedy_knapsack(x = knapsack_objects[1:800, ], W = 3500)
```
