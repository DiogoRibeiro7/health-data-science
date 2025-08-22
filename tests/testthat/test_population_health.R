context("population health utilities")

set.seed(123)

test_that("case_control_match fits model", {
  df <- data.frame(y = c(1,0,1,0), x = c(1,0,0,1))
  fit <- case_control_match(df, "y", "x")
  expect_s3_class(fit, "glm")
})

test_that("simulate_sir conserves population", {
  times <- 0:5
  out <- simulate_sir(beta = 0.2, gamma = 0.1, S0 = 99, I0 = 1, R0 = 0, times = times)
  totals <- rowSums(out[, c("S", "I", "R")])
  expect_true(all(abs(totals - 100) < 1e-6))
})

test_that("syndromic_surveillance flags anomalies", {
  counts <- c(rep(5, 10), 50)
  flags <- syndromic_surveillance(counts, window = 3, sd_limit = 2)
  expect_true(tail(flags, 1))
})

test_that("cost_effectiveness returns numeric", {
  val <- cost_effectiveness(2000, 10, 1500, 8)
  expect_type(val, "double")
})
