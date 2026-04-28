# Generate probabilistic forecasts of trial recruitment

Generate probabilistic forecasts of trial recruitment

## Usage

``` r
forecast(problem, n_sims = 10^4, overwrite = FALSE)
```

## Arguments

- problem:

  an object of class `fahb_problem`.

- n_sims:

  number of replicates to use in the simulation.

- overwrite:

  boolean indicating if we want to overwrite any simulation data
  currently held (defaults to FALSE).

## Value

an object of class `fahb_problem`.

## Examples

``` r
problem <- fahb_problem()
problem <- forecast(problem, n_sims = 10^3)
```
