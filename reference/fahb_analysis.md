# Build a `fahb` analysis object

Given a `fahb_problem` object calculate summary statistics which can
inform progression decisions. These include both standard progression
criteria statistics, and the expectation of the posterior predictive
distribution of the time until the trial recruits.

## Usage

``` r
fahb_analysis(n_pilot, t_pilot, problem, site_t = NULL, bayes_model = NULL)
```

## Arguments

- n_pilot:

  integer vector of numbers recruited at each open site.

- t_pilot:

  numeric vector of time (in years) each site has been open.

- problem:

  object of class `fahb_problem`.

- site_t:

  In the case of an external pilot, the time taken for all pilot sites
  to open.

- bayes_model:

  optional object of class `brmsfit` which will be used in the Bayesian
  analysis via `brms::update()` to avoid compiling a new model.

## Value

An object of class `fahb_analysis`.

## Examples

``` r
## Example illustrating a full analysis workflow
## (Not run on CRAN due to Bayesian model fitting)

# \donttest{
problem <- fahb_problem()
problem <- forecast(problem, n_sims = 500)

## Pilot trial data
n_pilot <- c(3, 5, 2)
t_pilot <- c(0.5, 0.6, 0.4)

analysis <- fahb_analysis(
  n_pilot = n_pilot,
  t_pilot = t_pilot,
  problem = problem
)
#> Compiling the model...
#> the number of chains is less than 1; sampling not done

print(analysis)
#> Standard progression criteria statistics:
#>       n_p       m_p       r_p 
#> 10.000000  3.000000  6.666667 
#> 
#> Expected posterior predictive time to recruit:
#> exp_pp_T 
#> 3.545051 
#> 
#> Posterior predictive distribution quantiles:
#>     0.5%     2.5%      20%      50%      80%    97.5%    99.5% 
#> 2.279105 2.522356 3.025034 3.470918 4.022959 4.964665 5.563473 
#> 
#> Posterior site opening rate hyperparamaters (Gamma):
#> shape  rate 
#> 33.00  3.35 
#> 
plot(analysis)
#> [[1]]

#> 
#> [[2]]

#> 
#> [[3]]

#> 
#> [[4]]

#> 
# }
```
