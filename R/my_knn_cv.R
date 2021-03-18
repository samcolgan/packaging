#' K-Nearest Neighbors Cross-Validation Error
#'
#' This function conducts k-nearest neighbors on a data frame and reports the
#'   cross-validation misclassification test error.
#'
#' @param train Input data frame to be used for training and test data.
#' @param cl Column of true class values to be used for making and testing
#'   predictions. A string vector.
#' @param k_nn Integer representing number of neighbors used for predicting class
#'   of test data.
#' @param k_cv Integer representing number of folds used for calculating
#'   cross-validation error.
#' @keywords statistical prediction
#'
#' @return A list with the following components:
#' @return \code{class}: Vector of predicted class for each observation of
#'   \code{train} dataset.
#' @return \code{cv_err}: Integer for cross-validation misclassification error.
#'
#' @examples
#' # Conduct k-nearest neighbor on penguins dataset
#'
#' # Penguin training data with select covariates
#' penguin_train <- data.frame("bill_length_mm" = penguins$bill_length_mm,
#'                            "bill_depth_mm" = penguins$bill_depth_mm,
#'                            "flipper_length" = penguins$flipper_length_mm,
#'                            "body_mass_g" = penguins$body_mass_g)
#'
#' # Penguin class information
#' penguin_cl <- penguins$species
#'
#' # Test with k_nn = 1 nearest neighbor in penguins dataset
#' my_knn_cv(penguin_train, penguin_cl, 5)
#'
#' # Test with k_nn = 5 nearest neighbors in penguins dataset
#' my_knn_cv(penguin_train, penguin_cl, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn = 1, k_cv) {
  # Split data into k folds, randomly
  fold <- sample(rep(1:k_cv, length(cl) / k_cv))
  # In case length(fold) is too small because of extra decimal amount...
  # Find extra decimal amount and calculate number of missing folds
  decimal <- (length(cl) / k_cv) - floor(length(cl) / k_cv)
  missing_folds <- k_cv * decimal
  # Generate missing folds
  folds_extra <- sample(1:k_cv)
  folds_extra <- folds_extra[1:missing_folds]
  # Create full-length folds vector
  fold <- c(fold, folds_extra)
  # Data frame of training data, true class, and associated folds
  data <- data.frame("train" = train, "cls" = cl, "fold" = fold)
  # Remove any missing observations
  data <- na.omit(data)
  # New data frame without cls
  data_training <- subset(data, select = -cls )
  # Create empty list with k_cv objects to store class predictions
  pred_class <- vector(mode = "list", length = k_cv)
  # Create empty vector to store proportion of false predictions of true class
  mismatch <- c(rep(NA, k_cv))
  # For each fold k...
  for (i in 1:k_cv) {
    # Training data are those with value not equal to k
    data_train <- data_training %>% filter(fold != i)
    dataTrain_class <- data %>% filter(fold != i)
    # Test data are those with value k
    data_test <- data_training %>% filter(fold == i)
    true_class <- data %>% filter(fold == i)
    # Initialize empty vector for each fold
    pred_class[[i]] <- c(rep(NA, length(true_class$cls)))
    # Conduct k-nearest neighbors to predict class
    pred_class[[i]] <- knn(data_train, data_test, dataTrain_class$cls, k_nn)
    # Find proportion of false predictions
    mismatch[i] <- mean(pred_class[[i]] != true_class$cls)
  }
  # Vectorize class predictions
  class <- unlist(pred_class)
  # Misclassification rate is the average of misclassifications for each fold
  cv_err <- mean(mismatch)
  # Return class vector and CV error
  return(list("class" = class,
              "cv_err" = cv_err))
}
