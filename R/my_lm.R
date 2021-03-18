#' Linear Regression Model
#'
#' This function produces a linear regression model, yielding a table with the
#'   estimate, standard error, t-value, and p-value for each coefficient.
#'
#' @param formula Formula class object relating to covariates
#' @param data Input data frame.
#' @keywords statistical inference
#' @keywords statistical prediction
#'
#' @return A table with the following column entries for each coefficient:
#' @return \code{estimate}: For each beta coefficient and the intercept.
#' @return \code{st_err}: Standard error on each coefficient.
#' @return \code{t_value}: A test statistic for each coefficient.
#' @return \code{p_value}: Probability of yielding a \code{t_value} this
#'   extreme or greater.
#'
#' @examples
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # Matrix of data
  m <- model.frame(formula, data)
  # Matrix of covariates
  X <- model.matrix(formula, data)
  # Response matrix
  Y <- model.response(m)
  # Initialize empty vector
  beta_hat <- c(rep(NA, ncol(X)))
  # Iterate to find beta hats
  for (i in ncol(X)) {
    # Vector of coefficient estimates
    beta_hat[i] <- solve((t(X) %*% X)) %*% t(X) %*% Y
  }
  # Degrees of freedom
  df <- nrow(data) - length(beta_hat)
  # Empty vector with length of beta hats
  sigma_squared_hat <- c(rep(NA, length(beta_hat)))
  for (i in length(beta_hat)) {
    # Fill with individual differences
    sigma_squared_hat[i] <- ((Y[i] - (X[i]*beta_hat[i]))^2) / df
  }
  # Overwrite with sum of entries
  sigma_squared_hat <- sum(sigma_squared_hat)
  # Empty vector with length of beta hats
  standard_error <- c(rep(NA, length(beta_hat)))
  # Standard errors
  for (i in length(beta_hat)) {
    standard_error[i] <- diag((sqrt(sigma_squared_hat * solve((t(X) %*% X)))))
  }
  # Empty vector with length of beta hats
  test_stat <- c(rep(NA, length(beta_hat)))
  # Test statistics
  for (i in length(beta_hat)) {
    test_stat[i] <- (mean(beta_hat[i]) - 0) / standard_error[i]
  }
  # Empty vector with length of beta hats
  p_value <- c(rep(NA, length(beta_hat)))
  # P_values
  for (i in length(beta_hat)) {
    p_value[i] <-  2 * pt(abs(test_stat[i]), df, lower.tail = FALSE) %>%
      signif(3)
  }
  # Results data frame (for column entries)
  results <- data.frame("Estimate" = beta_hat,
                        "Std. Error" = standard_error,
                        "t value" = test_stat,
                        "Pr(>|t|)" = p_value)
  # Form table
  return(table(results))
}
