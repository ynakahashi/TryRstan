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
## Fit via stan
################################################################################
dat_stan <- list(
   N = nrow(dat),
   D = 2,
   X = dat[, 2:3],
   Y = dat[, 1]
)

fit_01 <- stan(file = './StanModel/Detect_Confound_Factor.stan', 
               data = dat_stan, 
               iter = 3000,
               chains = 4,
               seed = 1234)





## load sample data
data(iris)
dat <- iris

## Create Y
dat$Y   <- 0.8 * dat$Sepal.Length + 1.2 * dat$Petal.Length + rnorm(nrow(dat), 0, 1)
dat_Ana <- dat[, -c(5)]

## prepare data
dat_Iris <- list(N  = nrow(dat_Ana),
                 D  = 4,
                 X = dat_Ana[, 1:4],
                 Y = dat_Ana[, 5])


################################################################################
## Fit via stan
################################################################################
## fitting 
fit_01 <- stan(file = './StanModel/Sparse_Modeling.stan', 
               data = dat_Iris, 
               iter = 10000,
               chains = 4,
               seed = 1234)



