# within test-my_rf_cv.R
test_that("my_rf_cv calculates a reasonable measure of cv_err", {
  expect_true(my_rf_cv(5) >= 105000)
})
test_that("k_cv must be greater than one", {
  expect_error(my_rf_cv(1))
})
test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("a string"))
})
