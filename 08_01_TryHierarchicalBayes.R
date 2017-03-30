################################################################################
##
## Try Hierarchical Bayes Model by Rstan
##
## written by Y.Nakahashi 
## 2017/3/30
##
################################################################################

################################################################################
## Environmental Settings
################################################################################
## set working directory
work_dir <- "/Users/nakahashi/Desktop/GitTest/TryRstan"
setwd(work_dir)

## load rstan
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

################################################################################
## Run stan
################################################################################
## load sample data
dat <- read.csv("data-sarary-2.txt", stringsAsFactors = F)

## plot
gp <- ggplot(dat, aes(X, Y, shape=as.factor(KID)))
gp <- gp + theme_bw(base_size = 18)
gp <- gp + geom_point(size = 2)
plot(gp)   



## prepare data
dat_sar <- list(N = nrow(dat),
                X = dat$X,
                Y = dat$Y,
                KID = dat$KID)

## fitting 8-1
fit_01 <- stan(file = 'model_8_1.stan', 
               data = dat_sar, 
               iter = 10000,
               chains = 4,
               seed = 1234)
res_01 <- rstan::extract(fit_01)

## fit lm for camparison
fit_11 <- lm(Y~X, data=dat)

## plot result
print(fit_01)
print(summary(fit_11))
print(summary(fit_11)$sigma)
stan_trace(fit_01)
stan_hist(fit_01)
stan_dens(fit_01, separate_chains = T)
stan_ac(fit_01, separate_chains = T)



## fitting 8-2
dat_sar <- list(N = nrow(dat),
                X = dat$X,
                Y = dat$Y,
                K = length(unique(dat$KID)),
                KID = dat$KID)

fit_02 <- stan(file = 'model_8_2.stan', 
               data = dat_sar, 
               iter = 10000,
               chains = 4,
               seed = 1234)
res_02 <- rstan::extract(fit_02)

print(fit_02)
stan_trace(fit_02)
stan_hist(fit_02)
stan_dens(fit_02, separate_chains = T)
stan_ac(fit_02, separate_chains = T)









