# within test-my_knn_cv.R
test_that("my_knn_cv calculates a reasonable measure for cv_err", {
  expect_true(my_knn_cv(data.frame("bill_length_mm" = my_penguins$bill_length_mm,
                                    "bill_depth_mm" = my_penguins$bill_depth_mm,
                                    "flipper_length" = my_penguins$flipper_length_mm,
                                    "body_mass_g" = my_penguins$body_mass_g),
                         my_penguins$species, 1, 5)$cv_err >= 0.12)
  expect_true(my_knn_cv(data.frame("bill_length_mm" = my_penguins$bill_length_mm,
                                    "bill_depth_mm" = my_penguins$bill_depth_mm,
                                    "flipper_length" = my_penguins$flipper_length_mm,
                                    "body_mass_g" = my_penguins$body_mass_g),
                         my_penguins$species, 5, 5)$cv_err >= 0.19)
})
test_that("train must be a data frame", {
  expect_error(my_knn_cv(list("bill_length_mm" = my_penguins$bill_length_mm,
                              "bill_depth_mm" = c(5, 5, 5)),
                         my_penguins$species, 1, 5))
})
test_that("non-numeric input throws error", {
  expect_error(my_knn_cv("a string", my_penguins$species, 1, 5))
})

