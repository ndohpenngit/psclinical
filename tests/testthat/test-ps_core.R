
library(testthat)
library(psclinical)

test_that("Continuous parallel sample size computes",{
  res <- ps_continuous_parallel(delta=2,sd=5,power=0.8)
  expect_true(res$n1>0)
})

test_that("Binary parallel sample size computes",{
  res <- ps_binary_parallel(p1=0.3,p2=0.2,power=0.8)
  expect_true(res$n1>0)
})

test_that("Survival extended computes",{
  res <- ps_survival(hr=0.7,power=0.8,accrual=2,followup=1)
  expect_true(res$events_required>0)
})

test_that("Simulation function returns 0-1",{
  pw <- sim_empirical_power(design="continuous", nsim=10, n=5, delta=1, sd=1)
  expect_true(pw>=0 & pw<=1)
})

