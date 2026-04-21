# fahb

The goal of `fahb` is to help with clinical trial **f**easibility
**a**ssessment via **h**ierarchical **B**ayesian recruitment models,
fitted to (internal or external) pilot date. It can be used at the
design stage to optimise the pilot and any pre-specified decision rules
to guide progression, and at the analysis stage to produce a
probabilistic prediction of the time until the main trial recruits to
target.

## Installation

Install the released version of `fahb` from CRAN:

``` r
install.packages("fahb")
```

Or you can install the development version of `fahb` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DTWilson/fahb")
```

## Example

Suppose we are planning an internal pilot for a trial which aims to
recruit `N = 320` participants from `m = 20` sites, and that we expect
recruitment to complete in three years. The pilot analysis will happen
at 12 months (so at time `t = 1/3` relative to the expected recruitment
time), and we would class the trial as infeasible if recruitment took
longer than `rel_thr = 1.2` times the expected recruitment time.

The variables `N, m, t` and `rel_thr` define the design variables. For
the model parameters, for the purpose of illustration we use the package
defaults (but see the vignette and the associated paper for more
discussion of these). To use `fahb` we first encode all design variables
and model parameters in a `fahb_problem` object, then run some
simulations via
[`forecast()`](https://dtwilson.github.io/fahb/reference/forecast.md)
before finding possible decision rules and the operating characteristics
they lead to:

``` r
library(fahb)

problem <- fahb_problem(N = 320, m = 20, t_int = 0.2, rel_thr = 1.2)

# Run the simulations
problem <- forecast(problem)

# Find some candidate decision rules are their OCs
design <- fahb_design(problem)
#> Searching for efficient progression criteria...
#> Approximating Bayesian operating characteristics...

print(design)
#> Standard progression criteria
#> 
#>     FPR         FNR        n_p         m_p        r_p
#> 1   0.0 0.826086957 26.4699097  4.62511758  8.7729347
#> 11  0.1 0.411781206 14.0982270  1.27756467  5.9841116
#> 21  0.2 0.283730715  8.7421711  0.44920095  6.3859320
#> 41  0.4 0.145301543  2.6732340 -0.44589371  5.6020928
#> 51  0.5 0.104908836  0.2255489  0.52603007  5.1955342
#> 71  0.7 0.039130435 -0.8229755 -0.14434948  3.9872926
#> 81  0.8 0.021458626 -0.7296959 -0.49522531  3.3632321
#> 91  0.9 0.009396914 -0.5483921  1.06629663  2.4837796
#> 101 1.0 0.000000000 -0.5295355  0.04794332 -0.6905117
#> 
#> Bayesian approximation
#> 
#>     FPR         FNR      T_p
#> 1   0.0 1.000000000 1.237585
#> 11  0.1 0.375175316 3.181149
#> 21  0.2 0.263253857 3.357522
#> 41  0.4 0.133660589 3.641103
#> 51  0.5 0.091023843 3.755226
#> 71  0.7 0.039971950 3.997307
#> 81  0.8 0.020476858 4.128723
#> 91  0.9 0.009256662 4.284346
#> 101 1.0 0.000000000 4.692425
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
plot(design)
```

![](reference/figures/README-unnamed-chunk-2-1.png)

Two types of decision rules are considered here. Firstly, we consider
rules which take the same form as the standard progression criteria
suggested by the NIHR. In the table output, `n_p` is a minimum number of
participants recruited, `m_p` is a minimum number of sites opened, and
`r_p` is a minimum rate of recruitment (participants per site-year), and
we progress to the main trial only if all of these thresholds are met.

The second type of decision rules is defined by `T_p`, the maximum
expected time until full recruitment conditional on the pilot data. We
progress to the main trial only if the actual posterior predictive
expectation is lower than this threshold.

Decision rules of both types are characterised by their false positive
(`FPR`) and false negative (`FNR`) rates. These are estimated via
simulation by comparing the decisions made with the underlying
feasibility, as determined by the threshold `rel_thr`. For example, we
can attain `FPR = 0.15` and `FNR = 0.34` if we proceed only if we
recruit at least $10.48$ participants from at least $1$ site, with an
overall rate of at least $6.57$ participants per site-year. We can get
slightly better operating characteristics if we use a Bayesian
progression rules instead. If we want to improve the pilot further we
can increase the design variable `t_int` so that the internal pilot will
have more data.
