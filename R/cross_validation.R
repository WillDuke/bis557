#' CV for Ridge Regression
#'
#' @description a minimal implementation to find optimal lambdas from a given set to use in ridge regression.
#'
#' @param data a data frame
#' @param formula a model formula of the form "y ~ ..."
#' @param folds the number of folds on which to cross validate
#' @param lambdas a vector of lambdas to check
#'
#' @return A tibble containing summary statistics of RMSEs by lambda.
#'
#' @import doParallel parallel casl foreach rsample dplyr ggplot2
#' @export
#'
#' @details The bulk of the code is adapted from Professor Kane's example in class, which I then generalized to work within a function with some additional modifications.
#'
cv_ridge_regression <- function(formula, data, folds = 5, lambdas = seq(0, 1, 0.03), contrasts = NULL){

  folds <- vfold_cv(data[sample.int(nrow(data), as.integer(0.5*nrow(data))),], folds)

  doParallel::registerDoParallel(parallel::detectCores(logical = FALSE))

  rmses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
      casl_util_rmse(
        testing(folds$splits[[i]])[[as.character(formula[2])]],
                     predict(ridge_regression(formula, training(folds$splits[[i]]),
                                              lambda = lambda, contrasts = contrasts),
                             testing(folds$splits[[i]]))
        )
    }
  }
  edf <- tibble(mean = apply(rmses, 1, mean),
                sd = apply(rmses, 1, sd),
                lambda = lambdas) %>%
    mutate(upper = mean + 2 * sd / nrow(.),
           lower = mean - 2 * sd / nrow(.))

  lambda_min <- edf$lambda[which.min(edf$mean)]
  find1se <- which((edf$mean > min(edf$mean) + var(edf$mean)/length(edf$mean)))
  lambda_1se <- edf$lambda[min(find1se[find1se > which.min(edf$mean)])]

  list(cv_table = edf, cv_plot = {
    ggplot2::ggplot(edf, aes(x = lambdas, y = mean, ymin = lower, ymax = upper)) +
      theme_minimal() +
      geom_point(aes(color = "red")) +
      geom_vline(xintercept = lambda_min, linetype="dotted") +
      geom_vline(xintercept = lambda_1se, linetype="dotted") +
      ylab("Root Mean Square Error") +
      xlab(expression(lambda)) +
      theme(legend.position = "none")
  },
  lambda_min = lambda_min,
  lambda_1se = lambda_1se)
}

