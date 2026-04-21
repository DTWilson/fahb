# Plot prior distributions

Given the hyperparameters encoded in a `fahb_problem` object, return
plots of the three prior distributions and a plot of the prior
predictive distribution of the recruitment rate at a randomly selected
site.

## Usage

``` r
check_priors(problem)
```

## Arguments

- problem:

  an object of class `fahb_problem`.

## Value

a list of `ggplot2` plots.

## Examples

``` r
problem <- fahb_problem()
check_priors(problem)
#> [[1]]

#> 
#> [[2]]

#> 
#> [[3]]

#> 
#> [[4]]
#> Warning: Removed 200 rows containing non-finite outside the scale range
#> (`stat_density()`).

#> 
```
