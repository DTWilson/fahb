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
#> Standard progression criteria
#> 
#>     FPR        FNR        n_p        m_p        r_p
#> 1   0.0 0.86342010 21.7707812 -0.9767788  6.1372284
#> 11  0.1 0.45745872  8.4808138 -0.1531660  7.2288871
#> 21  0.2 0.32815561  7.1262237  0.3993844  6.1081142
#> 41  0.4 0.17674223  2.6233294 -0.9998051  5.4518486
#> 51  0.5 0.12888329  4.0758541 -0.2470655  4.3932635
#> 71  0.7 0.05569549 -0.1564637 -0.2551430  3.5735046
#> 81  0.8 0.03680381  1.4315587  1.1183513  2.7139837
#> 91  0.9 0.02029107  0.1912580 -0.9395525  1.7163388
#> 101 1.0 0.00000000 -0.2385445 -0.2160022 -0.8934275
#> 
#> Bayesian approximation
#> 
#>     FPR        FNR      T_p
#> 1   0.0 1.00000000 1.389426
#> 11  0.1 0.42975091 3.128587
#> 21  0.2 0.30170725 3.312301
#> 41  0.4 0.16176882 3.593996
#> 51  0.5 0.11894766 3.716472
#> 71  0.7 0.05849426 3.936929
#> 81  0.8 0.03680381 4.025725
#> 91  0.9 0.01483347 4.105334
#> 101 1.0 0.00000000 4.387029
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
