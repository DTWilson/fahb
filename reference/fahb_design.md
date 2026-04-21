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
#>     FPR        FNR         n_p          m_p        r_p
#> 1   0.0 0.84726225 18.38199713  0.230190696 10.0827529
#> 11  0.1 0.44421573 10.00592370 -0.824482852  6.2421678
#> 21  0.2 0.32715795  8.30281463  0.002833049  5.2831274
#> 41  0.4 0.16522574  3.68355257 -0.403824576  5.2098599
#> 51  0.5 0.11980239 -0.88971182 -0.938334683  4.9197882
#> 71  0.7 0.05255935 -0.18674477 -0.663199579  3.5715915
#> 81  0.8 0.03471936  0.55176354 -0.716540332  2.8911946
#> 91  0.9 0.01770276  0.78253384  0.976924484  1.6874374
#> 101 1.0 0.00000000 -0.01104311 -0.966902639 -0.9864445
#> 
#> Bayesian approximation
#> 
#>     FPR        FNR      T_p
#> 1   0.0 1.00000000 1.378080
#> 11  0.1 0.41539728 3.124582
#> 21  0.2 0.29573213 3.291221
#> 41  0.4 0.16124605 3.582838
#> 51  0.5 0.11280362 3.707817
#> 71  0.7 0.05736243 3.906502
#> 81  0.8 0.03375875 4.009049
#> 91  0.9 0.01811445 4.101982
#> 101 1.0 0.00000000 4.262211
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
