test_that("default interim analysis time is correct", {
  f <- fahb_problem()
  expect_equal(round(f$p_t,6), 0.501215)
})
