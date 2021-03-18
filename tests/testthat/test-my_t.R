# within test-my_t.test.R
test_that("my_t.test calculates p_value correctly", {
  expect_equal(my_t.test(rnorm(10, 50, 1), 0, "greater")$p_value, 0)
})
test_that("my_t.test calculates df correctly", {
  expect_equal(my_t.test(rnorm(10, 50, 1), 0, "two.sided")$df, 9)
})
test_that("my_t.test reports alternative correctly", {
  expect_equal(my_t.test(rnorm(10, 50, 1), 0, "less")$alternative, "less")
})
test_that("my_t.test calculates test statistic correctly", {
  expect_equal(my_t.test(c(-1, 0, 0, 1, 3, 5), 0, "two.sided")$test_stat,
               1.4509525)
})
test_that("non-numeric input throws error", {
  expect_error(my_t.test("a string"))
  expect_error(my_pow(c(3,3,3), mu = "a string"))
})

