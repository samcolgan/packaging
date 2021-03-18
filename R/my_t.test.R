#' Student's T-test
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
my_t.test <- function(x, mu = 0, alternative = "two.sided") {
  # Define standard error
  se <- sd(x) / sqrt(length(x))
  # Define test statistic
  test_stat <- (mean(x) - mu) / se
  # Degrees of freedom
  df <- length(x) - 1
  # Check if x is a number
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  # Check if mu is a number
  if (!is.numeric(mu)) {
    stop("mu must be numeric")
  }
  # If two-tailed test...
  if (str_detect(alternative, "two.sided")) {
    # Double upper-tail portion
    p_value <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
    # Limit to 3 significant digits
    p_value <- signif(p_value, 3)
    return(list("test_stat" = test_stat,
                "df" = df,
                "alternative" = alternative,
                "p_value" = p_value))
    # If lower-tailed test...
  } else if (str_detect(alternative, "less")) {
    # Take lower tail portion
    p_value <- pt(test_stat, df, lower.tail = TRUE)
    # Limit to 3 significant digits
    p_value <- signif(p_value, 3)
    return(list("test_stat" = test_stat,
                "df" = df,
                "alternative" = alternative,
                "p_value" = p_value))
    # If upper tailed test...
  } else if (str_detect(alternative, "greater")) {
    # Subtract off lower tail portion from total portion to find upper portion
    p_value <- 1 - pt(test_stat, df, lower.tail = TRUE)
    # Limit to 3 significant digits
    p_value <- signif(p_value, 3)
    return(list("test_stat" = test_stat,
                "df" = df,
                "alternative" = alternative,
                "p_value" = p_value))
    # If none, report error...
  } else {
    stop("alternative must be 'two.sided', 'less', or 'greater'.")
  }
}
