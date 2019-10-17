#' @title Linear Model
#' @description Compute a linear regression.
#'
#' @param formula A formula of form "A ~ B".
#' @param data A dataframe.
#' @param contrasts A list of contrasts.
#' @return A list of coefficients from an lm model. The output is computed using the QR decomposition with the coefficients of collinear variables set to NA.
#' @examples
#' \dontrun{
#' data(iris)
#' linear_model(Sepal.Length ~ ., iris, contrasts)
#' }
#' @importFrom stats model.matrix
#' @export

#define linear model function with contrasts and basic collinearity handling
linear_model <- function(formula, data, contrasts = NULL){

  #define model matrix and response variable
  X <- model.matrix(formula, data, contrasts.arg = contrasts)
  Y <- as.matrix(subset(data, select = as.character(formula[[2]])), ncol = 1)

  #solve with qr decomposition
  beta <- qr.solve(qr(X), Y)
  #convert 0s to NAs
  beta[beta==0] <- NA

  list(coefficients = beta)
}
