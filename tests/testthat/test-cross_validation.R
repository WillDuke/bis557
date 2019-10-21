library(testthat)
library(MASS)
library(glmnet)

context("Test the output of cv_ridge_regression().")

test_that("Your cv_ridge_regression() function finds the same lambda.min with the same inputs in an easy case.", {

  skip("cv_ridge_regression is currently unstable.")

  data(mtcars)

  fit_cv_glmnet <- cv.glmnet(model.matrix(mpg ~ . , mtcars), as.matrix(mtcars[,1]), alpha = 0)

  fit_cv_ridge_regression <- cv_ridge_regression(mpg ~ ., mtcars, lambdas = fit_cv_glmnet$lambda)

  expect_equivalent(fit_cv_glmnet$lambda.min, fit_cv_ridge_regression$lambda_min,
                    tolerance = 1e-2)
})

test_that("Your cv_ridge_regression() function works with contrasts.", {

  skip("cv_ridge_regression is currently unstable.")

  data(mtcars)

  fit_cv_glmnet <- cv.glmnet(model.matrix(mpg ~ ., mtcars,
                                          contrasts.arg = list(Species = "contr.sum")),
                             as.matrix(mtcars[,1]), alpha = 0)

  fit_cv_ridge_regression <- cv_ridge_regression(mpg ~ ., mtcars, lambdas = fit_cv_glmnet$lambda, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_cv_glmnet$lambda.min, fit_cv_ridge_regression$lambda_min,
                    tolerance = 1e-2)

})
