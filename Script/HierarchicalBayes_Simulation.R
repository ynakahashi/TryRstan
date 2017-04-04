################################################################################
##
## Build Hierarchical Bayes Model by Rstan.
## Simulate ROI data & estimate non-linear model
##
## written by Y.Nakahashi 
## 2017/3/31
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
# work_dir <- "/Users/nakahashi/Desktop/GitTest/TryRstan"
work_dir <- "/Users/ynakahashi/Desktop/ynakahashi_git/TryRstan"
setwd(work_dir)

## load rstan
library(rstan)
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())


################################################################################
## Set parameters for simulation data
################################################################################
## seed
set.seed(123)

## number of data
num_week     <- 52
num_year     <- 4
year_name    <- c(1:num_year) + 2000
num_region   <- 10
region_name  <- 1:num_region

## seasonal effects
p <- 0.7 # just a threshold value for seasonal effect
season_eff <- sin(1:num_week) *  ifelse(runif(num_week) > p, 1, 0)
plot(season_eff, type="l")

## intercept
int_region <- runif(num_region) * rep(1:5, 2)
plot(int_region, type="l")

## regression coefficients
beta_TV_AS_0   <- c(0.006, 0.002) # hyper-parameter for TV ad-stock
beta_Digi_AS_0 <- c(0.010, 0.003) # hyper-parameter for Digital ad-stock
beta_TV_AS     <- rnorm(num_region, beta_TV_AS_0[1], beta_TV_AS_0[2])
beta_Digi_AS   <- rnorm(num_region, beta_Digi_AS_0[1], beta_Digi_AS_0[2])

## tactics
TV_para      <- c(20, 50, 0.7) # mean, sd, decay
Digi_para    <- c(5, 20, 0.5) # mean, sd, decay
lower_bond   <- 0

## residuals
var_error <- c(0, 0.2)


################################################################################
## Create simulation data
################################################################################
## create X
data_length  <- num_week * num_year * num_region
TV_Spend     <- ceiling(truncnorm::rtruncnorm(data_length, 
                                              a = lower_bond, 
                                              mean = TV_para[1], 
                                              sd = TV_para[2]))
TV_Matrix    <- data.frame(matrix(TV_Spend, nrow = num_week * num_year))
TV_AdStock   <- do.call("cbind", lapply(TV_Matrix, 
                                        function(x) {filter(x, TV_para[3], "recursive")}))
TV_ADS       <- tidyr::gather(data.frame(TV_AdStock))


Digi_Spend   <- ceiling(truncnorm::rtruncnorm(data_length, 
                                              a = lower_bond, 
                                              mean = Digi_para[1], 
                                              sd = Digi_para[2]))
Digi_Matrix  <- data.frame(matrix(Digi_Spend, nrow = num_week * num_year))
Digi_AdStock <- do.call("cbind", lapply(Digi_Matrix, 
                                        function(x) {filter(x, Digi_para[3], "recursive")}))
Digi_ADS     <- tidyr::gather(data.frame(Digi_AdStock))

X_Mat <- data.frame("TV_ADS" = TV_ADS$value, "Digi_ADS" = Digi_ADS$value)

## create Y
Intercept <- rep(int_region, each = num_week * num_year)
Seasonal  <- rep(season_eff, num_year * num_region)
TV_Eff    <- rep(beta_TV_AS, each = num_week * num_year) * X_Mat$TV_ADS
Digi_Eff  <- rep(beta_Digi_AS, each = num_week * num_year) * X_Mat$Digi_ADS
Error     <- rnorm(data_length, mean = var_error[1], sd = var_error[2])
log_Y     <- Intercept + Seasonal + TV_Eff + Digi_Eff + Error
plot(log_Y[1:208], type="l")


## create data
dat_Ana <- data.frame(
   "NUM_ORDERS"       = ceiling(exp(log_Y)),
   "Year"             = rep(rep(year_name, each = num_week), num_region),
   "Week_Num"         = stringr::str_pad(rep(1:num_week, num_year * num_region),
                                         2, side = "left", "0"),
   "Region"           = rep(region_name, each = num_week * num_year),
   "TV_Spending"      = TV_Spend,
   "Digital_Spending" = Digi_Spend
)



################################################################################
## Run stan
################################################################################
## load sample data
dat <- dat_Ana

## prepare data
dat_Ord <- list(N    = nrow(dat),
                Y    = log(dat$NUM_ORDERS),
                TV   = dat$TV_Spending,
                Digi = dat$Digital_Spending,
                K    = length(unique(dat$Region)),
                Reg  = dat$Region)

## fitting 
fit_01 <- stan(file = './StanModel/model_Simulation.stan', 
               data = dat_Ord, 
               iter = 1000,
               chains = 4,
               seed = 1234)
res_01 <- rstan::extract(fit_01)

## plot result
print(fit_01)
stan_trace(fit_01)
stan_hist(fit_01)
stan_dens(fit_01, separate_chains = T)
stan_ac(fit_01, separate_chains = T)


## fitting 02
fit_02 <- stan(file = './StanModel/model_Simulation_02.stan', 
               data = dat_Ord, 
               iter = 1000,
               chains = 4,
               seed = 1234)
