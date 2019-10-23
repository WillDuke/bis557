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
#' @importFrom stats sd model.matrix predict var qnorm
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores
#' @importFrom rsample vfold_cv testing training
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom magrittr %>%
#' @import dplyr ggplot2
#' @export
#'
#' @details The mse loop and edf table are adapted from Professor Kane's example in class, which I then generalized to work within a function with some additional modifications.
#'
cv_ridge_regression <- function(formula, data, folds = 5, lambdas = exp(seq(-2, 4, 0.1)), contrasts = NULL){

  # satisfy R CMD check
  i <- lambda <- `.` <- lower <- upper <- NULL

  # create folds
  folds <- vfold_cv(data, v = folds)

  # select no. of cores for parallelizing search (use global if set, else # cores in CPU)
  if(!is.null(options("mc.cores")[[1]])){
          cores <- options("mc.cores")[[1]]
  } else {
    cores <- detectCores(logical = FALSE)
  }
  registerDoParallel(cores)

  # helper function to find MSEs
  mse <- function(y, y_hat){
    means <- mean((y - y_hat)^2)
    means
  }

  # nested loop to find root mean squared error for each lambda across folds
  mses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
      mse(
        testing(folds$splits[[i]])[[as.character(formula[2])]],
        predict(ridge_regression(formula, training(folds$splits[[i]]),
                                 lambda = lambda, contrasts = contrasts),
                testing(folds$splits[[i]]))
      )
    }
  }

  # create tibble of results
  edf <- tibble(mean = apply(mses, 1, mean),
                sd = apply(mses, 1, sd),
                lambda = lambdas) %>%
    mutate(upper = mean + qnorm(0.975) * sd / nrow(.),
           lower = mean - qnorm(0.975) * sd / nrow(.))

  # retrieve lambda min
  lambda_min <- edf$lambda[which.min(edf$mean)]

  # find closest lambda 1se to the right
  find1se <- which((edf$mean > min(edf$mean) + sqrt(var(edf$mean))/length(edf$mean)))
  nextindex <- find1se[find1se > which.min(edf$mean)]
  lambda_1se <- edf$lambda[ifelse(!is.null(nextindex),
                                  min(find1se[find1se > which.min(edf$mean)]),
                                  which.min(edf$mean))]

  # create a list of output with table, lambda and rmse plot, lambda_min, and lambda_1se
  list(cv_table = edf, cv_plot = {
    ggplot2::ggplot(edf, aes(x = lambdas, y = mean, ymin = lower, ymax = upper)) +
      theme_minimal() +
      geom_errorbar(alpha = 0.3, width = 0) +
      geom_point(aes(color = "red")) +
      geom_vline(xintercept = lambda_min, linetype="dotted") +
      geom_vline(xintercept = lambda_1se, linetype="dotted") +
      ylab("Mean Squared Error") +
      xlab(expression(lambda)) +
      theme(legend.position = "none")
  },
  lambda_min = lambda_min,
  lambda_1se = lambda_1se)
}

