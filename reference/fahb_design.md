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
#>     FPR        FNR         n_p          m_p        r_p
#> 1   0.0 0.84716459 20.18122245 -0.278143277  8.5807397
#> 11  0.1 0.45587828  7.90948099 -0.423520544  7.4329207
#> 21  0.2 0.32295989  7.90948099 -0.664581560  5.5612174
#> 41  0.4 0.17123098  2.71569274  0.191882895  5.3566370
#> 51  0.5 0.12489627 -0.71292714 -0.156142535  4.8623610
#> 71  0.7 0.06058091  1.05988571  0.008010569  3.6066649
#> 81  0.8 0.03540802 -0.06842219 -0.972522072  2.8487965
#> 91  0.9 0.01950207 -0.27984834  0.122160586  1.6897025
#> 101 1.0 0.00000000 -0.75580849  0.023558708 -0.9489437
#> 
#> Bayesian approximation
#> 
#>     FPR        FNR      T_p
#> 1   0.0 1.00000000 1.305437
#> 11  0.1 0.42627939 3.136244
#> 21  0.2 0.30470263 3.307470
#> 41  0.4 0.16141079 3.590653
#> 51  0.5 0.11881051 3.712487
#> 71  0.7 0.05947441 3.916642
#> 81  0.8 0.03734440 4.015426
#> 91  0.9 0.01977870 4.133968
#> 101 1.0 0.00000000 4.486299
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
