# within test-my_lm.R
test_that("my_lm produces table", {
  expect_is(my_lm(my_penguins$species ~ my_penguins$body_mass_g, my_penguins),
            "table")
})
test_that("non-numeric input throws error", {
  expect_error(my_lm("a string"))
})
