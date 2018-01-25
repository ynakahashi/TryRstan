## load rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


################################################################################
## Data Simulation
## Assume following model:
##   Model 1 : log(Y_At) ~ mu_At + beta_A * X_At + error_At
################################################################################
####### simulation parameters settings
## set seed
set.seed(123)

## number of data
num_month   <- 12
num_year    <- 4
num_region  <- 5
data_length <- num_month * num_year

## distributions of
## state_t0
state_t0     <- 3
var_state_t0 <- 1

## state_t
var_state <- 0.01

## white noise
var_error <- 1

## regression coefficients of X
mu_beta  <- 0.5
var_beta <- 0.01  
beta_ad  <- rnorm(num_region, mu_beta, sqrt(var_beta))
beta_ad_all <- rep(beta_ad, each = data_length)

## X
scale_x  <- 5
zero_per <- 0.2
X <- ifelse(runif(num_region * data_length) > zero_per, 1, 0) * 
   rpois(num_region * data_length, scale_x)



##### simulate data
## State
State <- matrix(0, nrow = num_region * data_length)
for (i in 1:num_region) {
   for (j in 1:data_length) {
      if (j == 1) {
         ## 1, 49, 97, 145, 193行目が各地域の先頭
         State[(i-1)*data_length+j] <- rnorm(1, state_t0, sqrt(var_state_t0))
      } else {
         State[(i-1)*data_length+j] <- 
            State[(i-1)*data_length+j-1] + rnorm(1, 0, sqrt(var_state))
      }
   }
}

## Y
error <- rnorm(num_region * data_length, 0, sqrt(var_error))
Y <- State + X * beta_ad_all + error

Area_ID <- 1:num_region
DF <- data.frame(
   "Area_ID" = rep(Area_ID, each = data_length), 
   "Y" = Y,
   "X" = X,
   "True_e" = error)

## lm
results <- list()
for (i in Area_ID) {
   results[[as.character(i)]] <- lm(Y ~ X, data = DF, subset = Area_ID == i)
}



################################################################################
## Run stan
################################################################################
## model
dat_Stan <- list(N       = data_length,
                 K       = num_region,
                 Y       = DF$Y,
                 X       = DF$X,
                 Area_ID = DF$Area_ID)

## fitting 
fit_01 <- stan(file = '/Users/nakahashi/Desktop/Git/TryRstan/StanModel/HBSSM_Sim.stan', 
               data = dat_Stan, 
               iter = 1000,
               chains = 4,
               seed = 123)


################################################################################
## extract results
################################################################################
library(ggplot2)

## sample
res_01 <- rstan::extract(fit_01)

## parameters
ests <- summary(fit_01)$summary
t0_rows    <- rownames(ests)[grep("state_t0", rownames(ests))]
state_rows <- rownames(ests)[grep("state\\[", rownames(ests))]
b0_rows    <- rownames(ests)[grep("beta_0", rownames(ests))]
beta_rows  <- rownames(ests)[grep("beta\\[", rownames(ests))]

## state
state_par <- 
   ests %>% 
   data.frame %>% 
   select(mean) %>% 
   mutate("Par" = rownames(ests)) %>% 
   filter(Par %in% state_rows) %>% 
   mutate("Area" = DF$Area_ID)

state_cmp <- data.frame(
   True = State,
   Est = state_par$mean,
   Area = as.factor(state_par$Area)
)

ggplot(state_cmp, aes(x = True, y = Est, colour = Area)) +
   geom_point() +
   coord_fixed()


## regression coefficient
beta_par <- 
   ests %>% 
   data.frame %>% 
   select(mean) %>% 
   mutate("Par" = rownames(ests)) %>% 
   filter(Par %in% beta_rows)

beta_cmp <- data.frame(
   True = beta_ad,
   Est = beta_par$mean
)

ggplot(beta_cmp, aes(x = True, y = Est)) +
   geom_point() +
   coord_fixed()


ests[rownames(ests) %in% b0_rows, "mean"]
print(mu_beta)
print(var_beta)

ests[rownames(ests) %in% t0_rows, "mean"]
print(state_t0)
print(var_state_t0)



## compare true & estimate parameters
plot(cbind(state_DF$value, est_state))
plot(state_DF$value, type="b")
lines(est_state, col=2) 

plot(cbind(beta_X_DF$value, est_beta))
plot(beta_X_DF$value, type="b")
lines(est_beta, col=2) 

y_hat <- est_state + dat_Ana$X * est_beta
plot(dat_Ana$LogOrder, y_hat)
plot(dat_Ana$LogOrder, type="b")
lines(y_hat, col=2)

## sampling check
stan_trace(fit_01, pars = b0_rows)
stan_hist(fit_01)
stan_dens(fit_01, separate_chains = T)
stan_ac(fit_01, separate_chains = T)
