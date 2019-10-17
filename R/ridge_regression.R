#' Ridge Regression
#'
#' @description a minimal implementation of ridge regression employing singular value decomposition.
#'
#' @param formula a formula of the form y ~ .
#' @param data a dataframe
#' @param lambda ridge penalty term; default = 0
#' @param contrasts an optional list of contrasts
#'
#' @details The bulk of the code is adapted from Professor Kane's book "A Computational Approach to Statistical Learning" as well as code examples from class. I added scaling and centering (modeled off of MASS:lm.ridge), reformatted the code to work in a function, edited output to pass tests, and added documentation.
#' @importFrom stats model.matrix
#' @return A list of beta coefficients.
#' @export
#'
ridge_regression <- function(formula, data, lambda = 0, contrasts = NULL) {

  # anticipate issue with folds
  rownames(data) <- NULL

  # get model matrix
  X <- model.matrix(formula, data, contrasts.arg = contrasts)

  # extract response variable and center
  Y <- data[[as.character(formula)[2]]][as.numeric(rownames(X))]


  # save mean for rescaling
  Xmean <- colMeans(X[, -1])
  Ymean <-mean(Y)

  # extract response variable(s), center, and standardize
  X <- X[, -1] - rep(Xmean, rep(nrow(X), ncol(X) - 1))
  Xscale <- drop(rep(1/nrow(X), nrow(X)) %*% X^2)^0.5
  X <- X/rep(Xscale, rep(nrow(X), ncol(X)))

  Y <- Y - Ymean

  # initialize beta matrix
  coef <- matrix(NA_real_, nrow = length(lambda), ncol = ncol(X))

  # svd to find coefficients
  svd <- svd(X)
  D <- diag(svd$d  / (svd$d^2 + lambda))
  coef <- svd$v %*% D %*% t(svd$u) %*% Y

  # change back to original scale:
  scaledcoef <- t(as.matrix(coef / Xscale))

  # add in intercept as Ymean - beta*Xmean
  intercept <- Ymean - scaledcoef %*% Xmean
  coef <- cbind(intercept, scaledcoef)

  # create output with unique class for predict method
  coef <- as.vector(coef)
  names(coef) <- c("Intercept", colnames(X))
  attributes(coef)$formula <- formula
  class(coef) <- c(class(coef), "ridge_regression")
  coef
}

#' Predict Method for Ridge Regression
#'
#' @param object ridge_regression object
#' @param ... `(dataframe)`
#'
#' @return An estimate of new Y given X.
#' @export
#'
predict.ridge_regression <- function(object, ...) {

  # extract dots
  dots <- list(...)
  x_frame <- dots[[1]]

  # check for bad arg
  if (!is.data.frame(x_frame)) {
    stop("The first argument should be a data.frame of values",
             "to predict")
  }

  # create new model matrix and predict
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}


