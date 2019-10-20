## code to prepare `cv_test` dataset goes here

usethis::use_data(cv_test, overwrite = TRUE)

library(tibble)

n <- 400; p <- 4
beta <- c(1,-1,0.5,0)
mu <- rep(0,p)
Sigma <- matrix(0.9, nrow = p, ncol = p)
diag(Sigma) <- 1
X <- MASS::mvrnorm(n, mu, Sigma)
y <- X %*% beta + rnorm(n, sd = 5)

cv_test <- as_tibble(cbind(y, X)) %>% `colnames<-`(c("response", paste0(rep("pred",4), 1:4)))


# library(clusterGeneration)

# beta <- c(3,2,4,1,0,0)
# cv_test <- as_tibble(mvrnorm(n = 1000, rep(0, 6), genPositiveDefMat(6)$Sigma), .name_repair = "minimal") %>%
#   `colnames<-`(c(paste0(rep("pred",6), 1:6))) %>%
#   mutate(response = as.matrix(.) %*% beta + rnorm(1000, sd = 3))
