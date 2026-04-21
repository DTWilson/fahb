# Build a `fahb` design object

Given a `fahb_problem` object, find efficient progression decision
rules. These can include rules of the standard "progression criteria
form", or rules based on a Bayesian analysis of the pilot trial data, or
both.

## Usage

``` r
fahb_design(problem, quietly = TRUE)
```

## Arguments

- problem:

  an object of class `fahb_problem`.

- quietly:

  if this argument is set to FASLE then information about which steps
  have been completed will be printed to the console. Defaults to TRUE.

## Value

an object of class `fahb_design`.

## Examples

``` r
problem <- forecast(fahb_problem())
fahb_design(problem)
#> Searching for efficient progression criteria...
#> Approximating Bayesian operating characteristics...
#> Standard progression criteria
#> 
#>     FPR        FNR        n_p         m_p        r_p
#> 1   0.0 0.86880265 21.3552635  0.81522734  8.7466348
#> 11  0.1 0.46402431  8.2955576 -0.38310956  7.4143933
#> 21  0.2 0.31791189  6.7866628  1.39870227  6.1379261
#> 41  0.4 0.17704737  1.2592333  0.24815936  5.5555748
#> 51  0.5 0.13520232  1.1537850 -0.36004287  5.0054985
#> 71  0.7 0.06035078 -0.4994518 -0.98294471  3.6320801
#> 81  0.8 0.03563044 -0.5706294 -0.73072608  2.9241932
#> 91  0.9 0.01850573 -0.1996063 -0.33565730  1.6410583
#> 101 1.0 0.00000000 -0.4333531 -0.08941742 -0.7541021
#> 
#> Bayesian approximation
#> 
#>     FPR        FNR      T_p
#> 1   0.0 1.00000000 1.340823
#> 11  0.1 0.42604613 3.132955
#> 21  0.2 0.30672559 3.300352
#> 41  0.4 0.16102748 3.582629
#> 51  0.5 0.11973484 3.700791
#> 71  0.7 0.06007458 3.910858
#> 81  0.8 0.03659716 4.012609
#> 91  0.9 0.01822953 4.147183
#> 101 1.0 0.00000000 4.544340
#> 
#> FPR - False Positive Rate
#> FNR - False Negative Rate
#> 
#> n_p, m_p, r_p - Probabilistic thresholds for standard
#>                 progression criteria on the number recruited,
#>                 number of sites opened, and the recruitment rate
#>                 (participants per site per year) respectively
#> 
#> T_p - Bayesian decision rule threshold for the posterior predictive
#>       expected time until full recruitment
```
