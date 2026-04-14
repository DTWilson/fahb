test_that("default simulation works", {
  set.seed(81746)
  problem <- fahb_problem()
  problem$n_sims <- 10
  problem <- forecast(problem)
  expect_equal(round(problem$sims[8,1], 5), 7.49837)
})
