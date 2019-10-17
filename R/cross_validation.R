#' CV for Ridge Regression
#'
#' @description a minimal implementation to find optimal lambdas from a given set to use in ridge regression.
#'
#' @param data a data frame
#' @param formula a model formula of the form "y ~ ..."
#' @param folds the number of folds on which to cross validate
#' @param lambdas a vector of lambdas to check
#' @param contrasts an optional list of contrasts
#'
#' @return A tibble containing summary statistics of RMSEs by lambda.
#' @importFrom stats sd model.matrix predict var
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
#' @importFrom rsample vfold_cv testing training
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom magrittr %>%
#' @import casl dplyr ggplot2
#' @export
#'
#' @details The rmse loop and edf table are adapted from Professor Kane's example in class, which I then generalized to work within a function with some additional modifications.
#'
cv_ridge_regression <- function(formula, data, folds = 5, lambdas = seq(0, 1, 0.03), contrasts = NULL){

  # satisfy R CMD check
  i <- lambda <- `.` <- lower <- upper <- NULL

  # create folds
  folds <- vfold_cv(data[sample.int(nrow(data), as.integer(0.7*nrow(data))),], folds)

  #select no. of cores for parallelizing search (use global if set, else # cores in CPU)
  if(!is.null(options("mc.cores")[[1]])){
          cores <- options("mc.cores")[[1]]
  } else {
    cores <- detectCores(logical = FALSE)
  }
  registerDoParallel(cores)

  #nested loop to find root mean squared error for each lambda across folds
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

  # create tibble of results
  edf <- tibble(mean = apply(rmses, 1, mean),
                sd = apply(rmses, 1, sd),
                lambda = lambdas) %>%
    mutate(upper = mean + 2 * sd / nrow(.),
           lower = mean - 2 * sd / nrow(.))

  # retrieve lambda min
  lambda_min <- edf$lambda[which.min(edf$mean)]

  #find closest lambda 1se to the right
  find1se <- which((edf$mean > min(edf$mean) + var(edf$mean)/length(edf$mean)))
  lambda_1se <- edf$lambda[min(find1se[find1se > which.min(edf$mean)])]

  #create a list of output with table, lambda and rmse plot, lambda_min, and lambda_1se
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

