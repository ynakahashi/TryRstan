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

## load libraries
library(dplyr)
library(tidyr)

## load rstan
library(rstan)
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

## install & load sspir
# install.packages("/Users/nakahashi/Downloads/sspir_0.2.10.tar.gz",
#                  repos = NULL, type="source")
# library(sspir)


################################################################################
## Data Simulation
## Assume following model:
##   Model 1 : log(Y_ti) ~ state_ti + beta_ti * X_ti + error_ti
################################################################################
####### simulation parameters settings
## set seed
set.seed(123)

## number of data
num_week    <- 12
num_year    <- 4
num_region  <- 5
data_length <- num_week * num_year

## distributions of
## state_t0
mu_state_t0  <- 3
var_state_t0 <- 0.1

## state_t
var_state <- 0.01

## white noise
var_error <- 0.000025

## regression coefficients of X
beta_X_0   <- sin(seq(1, 3, length=data_length))/100
var_beta_X <- 0.000001



##### simulate data
## X
X_0 <- ceiling(cos(5:17)*10+10)
X   <- matrix(numeric(data_length * num_region), nrow = data_length)
scl_TV <- 5

for (i in 1:num_region) {
   for (j in 1:num_year) {
      for(k in 1:num_week) {
         if (j == 1) {
            X[(j-1)*12+k, i] <- X_0[k] + rgamma(1, scl_TV)
         } else {
            X[(j-1)*12+k, i] <- X[(j-2)*12+k, i] + 
               rgamma(1, scl_TV)
         }
      }
   }
}
X_DF <- as.data.frame(X) %>% 
   gather() %>% 
   mutate("Area" = gsub("V", "", key)) %>%
   select(Area, value)


## state_t
state      <- matrix(numeric(length = data_length * num_region), 
                     nrow = data_length)
state[1, ] <- rnorm(num_region, mean = mu_state_t0, sd = sqrt(var_state_t0))

for (i in 1:num_region) {
   for(j in 2:data_length) {
      state[j, i] <- rnorm(1, mean = state[j-1, i], sd = sqrt(var_state))
   }
}
state_DF <- as.data.frame(state) %>% 
   gather() %>% 
   mutate("Area" = gsub("V", "", key)) %>%
   select(Area, value)


## beta of X
beta_X <- matrix(numeric(data_length * num_region), nrow = data_length)
for (i in 1:num_region) {
   beta_X[, i] <- beta_X_0 + rnorm(data_length, 0, sd = sqrt(var_beta_X))
}
beta_X_DF <- as.data.frame(beta_X) %>% 
   gather() %>% 
   mutate("Area" = gsub("V", "", key)) %>%
   select(Area, value)


## white noise
error <- matrix(rnorm(data_length * num_region, 0, sqrt(var_error)), 
                nrow=data_length)
error_DF <- as.data.frame(error) %>% 
   gather() %>% 
   mutate("Area" = gsub("V", "", key)) %>%
   select(Area, value)


## create data
log_y <- state_DF$value + beta_X_DF$value * X_DF$value + error_DF$value
Y     <- ceiling(exp(log_y))
log_y_real <- log(Y)

dat_Ana <- data.frame(
   "OrderNum" = Y,
   "LogOrder" = log_y_real,
   "Area"     = X_DF$Area,
   "X"        = X_DF$value
)


################################################################################
## Run stan
################################################################################
## model
dat_Ord <- list(N = nrow(dat_Ana),
                Y = dat_Ana$LogOrder,
                K = length(unique(dat_Ana$Area)),
                X = dat_Ana$X)

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