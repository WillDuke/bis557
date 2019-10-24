## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(mc.cores = 2)
library(casl)
library(bis557)

## ------------------------------------------------------------------------
data("iris")
iris$duplicate <- iris$Sepal.Width
ridge_regression(Sepal.Length ~ ., iris, lambda = 0)

## ------------------------------------------------------------------------
data("cv_test")
cv_ridge_regression(response ~ ., cv_test)

## ------------------------------------------------------------------------
n <- 1000; p <- 25
beta <- c(1, rep(0, p-1))
X  <- matrix(rnorm(n * p), ncol = p)
svals <- svd(X)$d
max(svals)/min(svals)

N <- 1e4; `12_errors` <- rep(0, N)
for (k in 1:N){
  y <- X %*% beta + rnorm(n)
  betahat <- casl_ols_svd(X, y)
  `12_errors`[k] <- sqrt(sum((betahat - beta)^2))
}
mean(`12_errors`)

## ------------------------------------------------------------------------
n <- 250; p <- 25
beta <- c(1, rep(0, p-1))
X  <- matrix(rnorm(n * p), ncol = p)
alpha <- 0.001
X[,1] <- X[,1] * alpha + X[,2] * (1 - alpha)
svals <- svd(X)$d
max(svals) / min(svals)

## ------------------------------------------------------------------------
N <- 1e4; `12_errors` <- rep(0, N)
for (k in 1:N){
  y <- X %*% beta + rnorm(n)
  betahat <- solve(crossprod(X), crossprod(X,y))
  `12_errors`[k] <- sqrt(sum((betahat - beta)^2))
}
mean(`12_errors`)

## ------------------------------------------------------------------------
lambda <- 0.3293489
svals <- svd(X)$d
(max(svals) +  lambda) / (min(svals) + lambda)

## ------------------------------------------------------------------------
N <- 1e4; `12_errors` <- rep(0, N)
for (k in 1:N){
  y <- X %*% beta + rnorm(n)
  betahat <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% y
  `12_errors`[k] <- sqrt(sum((betahat - beta)^2))
}
mean(`12_errors`)

