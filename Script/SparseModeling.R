################################################################################
##
## Sparse modeling with Rstan
##
## written by Y.Nakahashi 
## 2017/12/11
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/ynakahashi/Desktop/ynakahashi_git/TryRstan"
setwd(work_dir)

## load rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


################################################################################
## Create data
################################################################################
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

res_01 <- rstan::extract(fit_01)



