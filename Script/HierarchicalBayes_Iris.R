################################################################################
##
## Build Hierarchical Bayes Model by Rstan using Iris
##
## written by Y.Nakahashi 
## 2017/3/31
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
data(iris)
dat <- iris
Species_Class <- c("setosa" = 1, "versicolor" = 2, "virginica" = 3)
dat$Class <- Species_Class[dat$Species]

## plot
gp <- ggplot(dat, aes(Sepal.Length, Sepal.Width, colour=Species))
gp <- gp + theme_gray(base_size = 14)
gp <- gp + geom_point(size = 2)
plot(gp)   


## prepare data
dat_Iris <- list(N = nrow(dat),
                 X = dat$Sepal.Length,
                 Y = dat$Sepal.Width,
                 Z = dat$Petal.Length,
                 K = length(unique(dat$Class)),
                 Spe = dat$Class)

## fitting 
fit_01 <- stan(file = './StanModel/model_Iris.stan', 
               data = dat_Iris, 
               iter = 10000,
               chains = 4,
               seed = 1234)
res_01 <- rstan::extract(fit_01)

res_lm_01 <- lm(Sepal.Length~Sepal.Width, data=dat, Class == 1)
res_lm_02 <- lm(Sepal.Length~Sepal.Width, data=dat, Class == 2)
res_lm_03 <- lm(Sepal.Length~Sepal.Width, data=dat, Class == 3)

print(cbind(
   summary(res_lm_01)$coefficient[, 1],
   summary(res_lm_02)$coefficient[, 1],
   summary(res_lm_03)$coefficient[, 1]))


## plot result
print(fit_01)
print(summary(fit_11))
print(summary(fit_11)$sigma)
stan_trace(fit_01)
stan_hist(fit_01)
stan_dens(fit_01, separate_chains = T)
stan_ac(fit_01, separate_chains = T)

