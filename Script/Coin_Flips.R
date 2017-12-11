dat_Coin <- list(
   N = 42,
   W = 0
)

fit_01 <- stan(file = './StanModel/Coin_Flips.stan', 
               data = dat_Coin,
               iter = 3000,
               chains = 4,
               seed = 1234)


summary(fit_01)$summary[, c("mean", "2.5%", "50%", "97.5%", "Rhat")]

## Parameters
plot(fit_01)

## Trace plot
pars <- c("p")
stan_trace(fit_01, pars = pars)

## Histogram
stan_hist(fit_01, pars = pars)

### Density
stan_dens(fit_01, separate_chains = T, pars = pars)

### Auto-correlation of the samples
stan_ac(fit_01, separate_chains = T, pars = pars)

