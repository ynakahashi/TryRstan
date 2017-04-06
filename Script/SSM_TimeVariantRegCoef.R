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
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

## install & load sspir
install.packages("/Users/nakahashi/Downloads/sspir_0.2.10.tar.gz",
                 repos = NULL, type="source")





