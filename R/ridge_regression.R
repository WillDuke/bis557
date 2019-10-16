#' Ridge Regression
#'
#' @description a minimal implementation of ridge regression employing singular value decomposition.
#'
#' @param formula a formula of the form y ~ .
#' @param data a dataframe
#' @param lambda ridge penalty term; default = 0
#'
#' @details The bulk of the code is adapted from Professor Kane's book "A Computational Approach to Statistical Learning" as well as code examples from class. I added scaling and centering (modeled off of MASS:lm.ridge), reformatted the code to work in a function, edited output to pass tests, and added documentation.
#'
#' @return A list of beta coefficients.
#' @export
#'
ridge_regression <- function(formula, data, lambda = 0, contrasts = NULL) {

  # anticipate issue with folds
  rownames(data) <- NULL

  # extract response variable and center
  Y <- data[[as.character(formula)[2]]][as.numeric(rownames(X))]
  Y <- Y - mean(Y)

  # extract response variable, center, and standardize
  X <- model.matrix(formula, data, contrasts.arg = contrasts)
  X <- X[, -1] - rep(colMeans(X[, -1]), rep(nrow(X), ncol(X) - 1))
  Xscale <- drop(rep(1/nrow(X), nrow(X)) %*% X^2)^0.5
  X <- X/rep(Xscale, rep(nrow(X), ncol(X)))

  # initialize beta matrix
  beta <- matrix(NA_real_, nrow = length(lambda), ncol = ncol(X))

  # svd to find coefficients
  svd <- svd(X)
  D <- diag(svd$d  / (svd$d^2 + lambda))
  beta <- as.vector(svd$v %*% D %*% t(svd$u) %*% Y)

  #name beta vector
  names(beta) <- colnames(X)

  #
  attributes(beta)$formula <- formula
  class(beta) <- c(class(beta), "ridge_regression")
  beta
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
  dots <- list(...)
  x_frame <- dots[[1]]
  if (!is.data.frame(x_frame)) {
    stop(red("The first argument should be a data.frame of values",
             "to predict"))
  }
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}


