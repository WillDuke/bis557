#' CV for Ridge Regression
#'
#' @param data a data frame
#' @param formula a model formula of the form `y ~ ...`
#' @param folds the number of folds on which to cross validate (default is 2)
#' @param lambdas a vector of lambdas to check (default is `seq(0, 0.5, 0.01)`)
#'
#' @return A tibble containing summary statistics of RMSEs by lambda.
#'
#' @import doParallel casl foreach rsample dplyr
#' @export
#'
cv_ridge_regression <- function(formula, data, lambdas = seq(0, 0.5, 0.01)){

  folds <- vfold_cv(data[sample.int(nrow(data), as.integer(0.5*nrow(data))),], 10)

  registerDoParallel(detectCores(logical = FALSE))

  rmses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
      casl_util_rmse(
        testing(folds$splits[[i]]) %>% select(as.character(formula[2])) %>% as.matrix() %>% as.numeric(),
                     predict(ridge_regression(formula, training(folds$splits[[i]]),
                                              lambda = lambda),
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
  lambda_1se <- edf$lambda[min(which((edf$mean > min(edf$mean) + var(edf$mean)/length(edf$mean)) == TRUE))]

  list(cv_table = edf, cv_plot = {
    ggplot(edf, aes(x = lambdas, y = mean, ymin = lower, ymax = upper)) +
      geom_errorbar() +
      theme_minimal() +
      geom_point(aes(color = "red")) +
      geom_vline(xintercept = lambda_min) +
      geom_vline(xintercept = lambda_1se) +
      ylab("Root Mean Square Error") +
      xlab(expression(lambda))
  },
  lambda_min = lambda_min,
  lambda_1se = lambda_1se)
}
