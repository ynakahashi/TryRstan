################################################################################
##
## Detecting confounding factor
##
## written by Y.Nakahashi 
## 2017/12/11
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
# work_dir <- "/Users/ynakahashi/Desktop/ynakahashi_git/TryRstan"
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryRstan"
setwd(work_dir)

## load rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


################################################################################
## Simulate confounded data
################################################################################
N      <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y      <- rnorm(N, x_real)
dat    <- data.frame(y, x_real, x_spur)

## lm
summary(res.lm <- lm(y ~ ., data = dat))

## step
step(res.lm)



################################################################################
## Estimate via stan
################################################################################









