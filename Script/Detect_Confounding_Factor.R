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
set.seed(15)
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


################################################################################
## Check result
################################################################################
summary(fit_01)$summary[, c("mean", "2.5%", "50%", "97.5%", "Rhat")]
summary(lm(Y ~ Sepal.Length + Petal.Length, data = dat_Ana))

## Parameters
plot(fit_01)

## Trace plot
pars <- c("beta[1]", "beta[2]", "beta[3]", "beta[4]")
stan_trace(fit_01, pars = pars)

## Histogram
stan_hist(fit_01, pars = pars)

### Density
stan_dens(fit_01, separate_chains = T, pars = pars)

### Auto-correlation of the samples
stan_ac(fit_01, separate_chains = T, pars = pars)