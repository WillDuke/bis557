#' CV for Ridge Regression
#'
#' @param data a data frame
#' @param formula a model formula of the form `y ~ ...`
#' @param folds the number of folds on which to cross validate
#' @param lambdas a vector of lambdas to check (default is `seq(0, 0.5, 0.01)`)
#'
#' @return
#'
#' @import parallel casl foreach
#' @export
#'
cv_ridge_regression <- function(data, formula, folds = 2, lambdas = seq(0, 0.5, 0.01)){
  folds <- vfold_cv(ridge_regression[[data]], folds)
  lambdas <- if(lambas == "detect"){
    lambas = {
  }


  registerDoParallel(detectCores(logical = FALSE))

  rmses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {

      casl_util_rmse(testing(folds$splits[[i]])$duration,
                     predict(ridge_regression(form, training(folds$splits[[i]]),
                                              lambda = lambda),
                             testing(folds$splits[[i]])))
    }
  }
  return(rmses)
}

