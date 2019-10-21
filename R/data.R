#' Sample dataset for testing regression functions.
#'
#' A dataset for testing tougher cases with the linear_model function in this package`.
#'
#' @format A data frame with 2 rows and 3 variables.
#' \describe{
#'   \item{y}{response}
#'   \item{x1}{predictor 1}
#'   \item{x2}{predictor 2}
#' }
#'
"lm_patho"


#' Sample dataset for testing cross validation.
#'
#' A dataset of modestly correlated variables from mvrnorm() and one response variable for testing cv_ridge_regression.
#'
#' @format A data frame with 200 rows and 4 variables.
#' \describe{
#'   \item{response}{response}
#'   \item{pred1}{predictor 1}
#'   \item{pred2}{predictor 2}
#'   \item{pred3}{predictor 3}
#'   \item{pred4}{predictor 4}
#' }
#'
"cv_test"
