library(testthat)

context("Test the output of grad_desc().")

test_that("Your grad_desc() function returns the same coefficients as lm.", {

  data(iris)

  fit_grad_desc <- grad_desc(Sepal.Length ~ ., iris)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_grad_desc$coefficients,
                    tolerance = 1e-3)
})

test_that("Your grad_desc() function works with contrasts.", {

  data(iris)

  fit_grad_desc <- grad_desc(Sepal.Length ~ ., iris,
                                   contrasts = list(Species = "contr.sum"))

  fit_lm <- lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(fit_lm$coefficients, fit_grad_desc$coefficients,
                    tolerance = 1e-3)
})
test_that("Your grad_desc() function works in a tougher case.", {

  data(lm_patho)

  fit_grad_desc <- grad_desc(y ~., lm_patho)

  fit_lm <- lm(y ~., lm_patho)

  expect_equivalent(fit_lm$coefficients, fit_grad_desc$coefficients,
                    tolerance = 1e-3)
})
