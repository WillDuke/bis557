#' Ridge Regression
#'
#' @param formula a formula of the form y ~ .
#' @param data a dataframe
#' @param lambda ridge penalty term; default = 0
#'
#' @return A list of beta coefficients.
#' @export
#'
ridge_regression <- function(formula, data, lambda = 0) {
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  attributes(ret)$formula <- form
  class(ret) <- c(class(ret), "ridge_regression")
  ret
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

