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
problem <- forecast(fahb_problem(), n_sims = 500)
fahb_design(problem)
#> Standard progression criteria
#> 
#>     FPR        FNR        n_p        m_p        r_p
#> 1   0.0 0.75136612 16.8772428  1.1274486  3.8351325
#> 11  0.1 0.46174863  4.4350729  3.2596766  7.4832026
#> 21  0.2 0.31147541  8.1299921  1.1851716  4.4659384
#> 41  0.4 0.12841530  3.8520453 -0.7482546  4.4056872
#> 51  0.5 0.09836066  0.7679427  2.1779381  3.8540530
#> 71  0.7 0.06284153  1.4183957  1.2379523  3.1665629
#> 81  0.8 0.03825137  1.4184781  1.1338973  2.6796628
#> 91  0.9 0.01639344  1.2209585  1.1616479  1.1432414
#> 101 1.0 0.00000000  0.1215247 -0.8231258 -0.9920766
#> 
#> Bayesian approximation
#> 
#>     FPR        FNR      T_p
#> 1   0.0 1.00000000 1.610074
#> 11  0.1 0.51092896 3.035218
#> 21  0.2 0.34699454 3.256845
#> 41  0.4 0.15027322 3.579466
#> 51  0.5 0.10655738 3.728152
#> 71  0.7 0.06284153 3.882449
#> 81  0.8 0.05191257 3.980638
#> 91  0.9 0.01912568 4.132130
#> 101 1.0 0.00273224 4.353757
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
