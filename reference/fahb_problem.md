# Build a `fahb_problem` object

Given a trial design and a set of model hyperparameters, build an object
of class `fahb_problem`.

## Usage

``` r
fahb_problem(
  N = 320,
  m = 20,
  t_int = 0.167,
  n_ext = NULL,
  m_ext = NULL,
  rel_thr = 1.2,
  so_hps = c(30, 2.85),
  mean_rr_hps = c(2, 0.329),
  sd_rr_hps = c(30, 100)
)
```

## Arguments

- N:

  target sample size.

- m:

  number of recruiting sites.

- t_int:

  timing of an internal pilot analysis, as a proportion of the expected
  time to recruit.

- n_ext:

  number of participants to recruit to an external pilot.

- m_ext:

  number of sites to open in an external pilot.

- rel_thr:

  threshold which discriminates feasible and infeasible trials, as a
  multiple of the expected time to recruit.

- so_hps:

  site opening rate hyperparameters (shape and rate for a Gamma prior).

- mean_rr_hps:

  mean site recruitment rate hyperparameters (mean and sd for a
  lognormal prior).

- sd_rr_hps:

  variance in site recruitment rates hyperparameters (shape and rate for
  a Gamma prior).

## Value

an object of class `fahb_problem`

## Examples

``` r
fahb_problem()
#> $N
#> [1] 320
#> 
#> $m
#> [1] 20
#> 
#> $t
#> [1] 0.5
#> 
#> $n_ext
#> NULL
#> 
#> $m_ext
#> NULL
#> 
#> $internal
#> [1] TRUE
#> 
#> $rel_thr
#> [1] 1.2
#> 
#> $exp_T
#> [1] 3.001289
#> 
#> $thr
#> [1] 3.601546
#> 
#> $so_hp_a
#> [1] 30
#> 
#> $so_hp_b
#> [1] 2.85
#> 
#> $mean_rr_hp_a
#> [1] 2
#> 
#> $mean_rr_hp_b
#> [1] 0.329
#> 
#> $sd_rr_hp_a
#> [1] 30
#> 
#> $sd_rr_hp_b
#> [1] 100
#> 
#> $sims
#> NULL
#> 
#> attr(,"class")
#> [1] "fahb_problem"
```
