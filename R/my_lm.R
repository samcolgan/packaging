#' Linear Regression Model
#'
#' This function performs a t-test on a numeric vector, yielding the test
#'   statistic, degrees of freedom, type of test, and p-value.
#'
#' @param x Numeric vector of data to be tested.
#' @param mu Numeric input indicating the mean under the null distribution.
#'   Defaults to 0.
#' @param alternative String input indicating if test is "\code{two.sided}",
#'   "\code{less}", or "\code{greater}". Defaults to "\code{two.sided}".
#' @keywords statistical inference
#'
#' @return A list with the following components:
#' @return \code{test_stat}: A test statistic
#' @return \code{df}: Degrees of freedom used in the test.
#' @return \code{alternative}: String indicating if test is "\code{two.sided}",
#'   "\code{less}", or "\code{greater}".
#' @return \code{p_value}: Probability of yielding a \code{test_stat} this
#' extreme or greater.
#'
#' @examples
#' data_1 <- rnorm(10, 3, 2)
#' my_t.test(data_1)
#'
#' data_2 <- c(0, 0, 0, 0, 50)
#' my_t.test(data_2, 1, "greater")
#'
#' @export
