################################################################################
##
## Modeling by X with deficit
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
dat$Y <- 
   5 + 
   3.5 * dat$Sepal.Length + 
   1.5 * dat$Sepal.Width +
   2.5 * dat$Petal.Length +
   4.5 * dat$Petal.Width +
   rnorm(nrow(dat), 0, 0.8)
dat_Ori <- dat[, -c(5)]
lm(Y ~ ., data = dat_Ori)

## Remove X
set.seed(123)
dat_Ana <- dat_Ori
tmp <- scale(dat_Ana$Sepal.Length)
# dat_Ana$Sepal.Length <- ifelse(runif(nrow(dat_Ana)) > 0.9, NA, dat_Ana$Sepal.Length)
dat_Ana$Sepal.Length <- ifelse(pnorm(tmp) > runif(nrow(dat_Ori), 0.6, 1),
                               NA, dat_Ana$Sepal.Length)
dat_Ord <- dat_Ana[order(dat_Ana$Sepal.Length), ]

lm(Y ~ ., data = dat_Ori)
lm(Y ~ ., data = dat_Ana)

## Prepare data
N_obs <- sum(!is.na(dat_Ord$Sepal.Length))
dat_Iris <- list(N_obs = N_obs,
                 N_mis = nrow(dat_Ord) - N_obs,
                 Y = dat_Ord$Y,
                 X1 = dat_Ord$Sepal.Length[1:N_obs],
                 X2 = dat_Ord[, 2],
                 X3 = dat_Ord[, 3],
                 X4 = dat_Ord[, 4]
                 )


################################################################################
## Fit via stan
################################################################################
## fitting 
fit_01 <- stan(file = './StanModel/X_with_Deficit.stan', 
               data = dat_Iris, 
               iter = 3000,
               chains = 4,
               seed = 1234)

################################################################################
## Check result
################################################################################
summary(fit_01)$summary[, c("mean", "2.5%", "50%", "97.5%", "Rhat")]
lm(Y ~ ., data = dat_Ori)
lm(Y ~ ., data = dat_Ana)

## Parameters
plot(fit_01)

## Trace plot
pars <- c("b1", "b2", "b3", "b4")
stan_trace(fit_01, pars = pars)

## Histogram
stan_hist(fit_01, pars = pars)

### Density
stan_dens(fit_01, separate_chains = T, pars = pars)

### Auto-correlation of the samples
stan_ac(fit_01, separate_chains = T, pars = pars)

res_01 <- rstan::extract(fit_01)



