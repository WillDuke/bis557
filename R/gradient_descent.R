#' @title Gradient Descent
#' @description Compute a linear regression via gradient descent. Passes input to linear_model if data frame is not full rank.
#'
#' @param formula A formula of form "A ~ B".
#' @param data A dataframe.
#' @param contrasts A list of contrasts; default is NULL.
#' @param factor The factor by which to adjust beta along gradient; default is 0.0001.
#' @param max_Iter Maximum number of iterations before the loop exits; default is 500,000.
#' @param threshold Size of difference between previous and new betas at which the loop exits; default is 0.001.
#' @return A list of coefficients from an lm model via gradient descent.
#' @examples
#' \dontrun{
#' data(iris)
#' grad_desc(Sepal.Length ~ ., iris)
#' }
#' @importFrom stats model.matrix setNames
#' @export

grad_desc <- function(formula, data, contrasts = NULL, factor = 0.0001, max_Iter = 5e5, threshold = 1e-12){
  #define model matrix, response variable, beta_k
  X <- model.matrix(formula, data, contrasts.arg = contrasts)
  Y <- as.matrix(subset(data, select = as.character(formula[[2]])), ncol = 1)
  beta_k <- matrix(1, ncol = 1, nrow = ncol(X))

  #define helper function check_r: finds sum of squared residuals
  check_r <- function(beta, Y, X) {
    drop(t(Y) %*% Y + t(beta) %*% t(X) %*% X %*% beta - 2 * t(Y) %*% X %*% beta)
  }

  #define helper function df: takes step toward beta
  df <- function(beta, Y, X){
    (2 * t(X) %*% X %*% beta - 2 * t(X) %*% Y)
  }

  ##divert if not full rank
  if(qr(X)$rank == dim(X)[2]){
    #define counter to set max number of loops
    counter = 1
    diff = 10

    #initiate while loop to check for improvement in residuals and stop after a given point
    while ((counter < max_Iter) & (diff > threshold)){
      r1 <- check_r(beta_k, Y, X) # check residuals
      beta_k <- beta_k - factor * df(beta_k, Y, X)  # improve beta_k
      r2 <- check_r(beta_k, Y, X) # check updated residuals
      diff <- r1 - r2
      counter = counter + 1
    }


    #print some useful info
    print(sprintf("Gradient descent completed after %i iterations", counter))
    print(sprintf("The final difference in the residuals was: %f,", diff))

    # convert to list that matches lm output
    gd <- list(coefficients = beta_k)
    list(coefficients = setNames(as.numeric(gd$coefficients), dimnames(gd$coefficients)[[1]]))
  }
  else{
    warning("Data frame has perfect collinearity. Passing to linear_model().")
    linear_model(formula, data, contrasts = NULL)
  }
}

