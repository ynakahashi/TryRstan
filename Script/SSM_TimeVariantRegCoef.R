################################################################################
##
## Build State Space Model by Rstan.
## Simulate ROI data & estimate time-variant regression coefficient
##
## written by Y.Nakahashi 
## 2017/4/7
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryRstan"
# work_dir <- "/Users/ynakahashi/Desktop/ynakahashi_git/TryRstan"
setwd(work_dir)

## load rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## install & load sspir
# install.packages("/Users/nakahashi/Downloads/sspir_0.2.10.tar.gz",
#                  repos = NULL, type="source")
# library(sspir)

################################################################################
## Data Simulation
## Assume following model:
##   Model 1 : log(Y) ~ TV(intercept) + TV(TV) + error
################################################################################
## set seed
set.seed(123)

## number of data
num_week    <- 12
num_year    <- 4
num_region  <- 1
data_length <- num_week * num_year

## create local trend
mu_0  <- 4
var_mu <- 0.2

mu      <- matrix(numeric(length = data_length * num_region), 
                  nrow = data_length)
mu[1, ] <- rnorm(num_region, mean = mu_0, sd = sqrt(var_mu))

for (i in 1:num_region) {
   for(j in 2:data_length) {
      mu[j, i] <- rnorm(1, mean = mu[j-1, i], sd = sqrt(var_mu))
   }
}
## need to "gather" when num_region has more than 1


## create TV spending
tv_0 <- c(10, 20, 30, 30, 0, 0, 0, 20, 30, 40, 0, 30)
TV_Spending <- matrix(numeric(data_length * num_region), nrow = data_length)
scl_TV <- 5

for (i in 1:num_region) {
   for (j in 1:num_year) {
      for(k in 1:num_week) {
         if (j == 1) {
            TV_Spending[(j-1)*12+k, i] <- tv_0[k] + rgamma(1, scl_TV)
         } else {
            TV_Spending[(j-1)*12+k, i] <- TV_Spending[(j-2)*12+k, i] + 
               rgamma(1, scl_TV)
         }
      }
   }
}
## need to "gather" when num_region has more than 1

## create Time-variant regression coefficient of TV
beta_TV_0 <- sin(seq(1, 3, length=data_length))/100
var_TV    <- 0.000001
beta_TV      <- matrix(numeric(data_length * num_region), nrow = data_length)

for (i in 1:num_region) {
   for (j in 1:data_length) {
      beta_TV[j, i] <- beta_TV_0[j] + rnorm(1, 0, sd = sqrt(var_TV))
   }
}
# beta_TV_0   <- rep(0.001, data_length)
# var_TV_coef <- 0
# beta_TV <- beta_TV_0
# plot(beta_TV, type="b")
## need to "gather" when num_region has more than 1


## create data
var_error <- 0.000025
error <- matrix(rnorm(data_length * num_region, 0, sqrt(var_error)), nrow=data_length)
log_y <- mu + beta_TV * TV_Spending + error
Y     <- ceiling(exp(log_y))
log_y_real <- log(Y)

dat_Ana <- data.frame(
   "OrderNum" = Y,
   "LogOrder" = log_y_real,
   "TV_Spend" = TV_Spending
)


################################################################################
## Run stan
################################################################################
## model
dat_Ord <- list(N  = nrow(dat_Ana),
                Y  = dat_Ana$LogOrder,
                TV = dat_Ana$TV_Spend)

## fitting 
fit_01 <- stan(file = './StanModel/model_SSM_TVRC_Sim.stan', 
               data = dat_Ord, 
               iter = 10000,
               chains = 4,
               seed = 1234)

## extract results
res_01 <- rstan::extract(fit_01)
# mu
summary(fit_01)$summary

# coefficient
summary(fit_01)$summary[51, 1]
beta_TV_0[1]


## compare true & estimate parameters
plot(cbind(summary(fit_01)$summary[2:49, 1], mu))
plot(mu, type="b")
lines(summary(fit_01)$summary[2:49, 1], col=2) 

plot(cbind(summary(fit_01)$summary[52:99, 1], beta_TV))
plot(beta_TV, type="b")
lines(summary(fit_01)$summary[52:99, 1], col=2) 

plot(summary(fit_01)$summary[52:99, 1], type="b") 
lines(beta_TV, col=2) 

stan_trace(fit_01)
stan_hist(fit_01)
stan_dens(fit_01, separate_chains = T)
stan_ac(fit_01, separate_chains = T)






