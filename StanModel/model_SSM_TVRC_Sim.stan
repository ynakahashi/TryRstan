data {
   int N;
   vector[N] Y;
   vector[N] TV;
}

parameters {
   real mu_0;
   vector[N] mu;
   real<lower=0> var_mu;
   real beta_TV_0;
   vector[N] beta_TV;
   real<lower=0> var_TV_coef;
   // real beta_TV;
   real<lower=0> var_error;
}

model {
   ## local trend   
   mu[1] ~ normal(mu_0, var_mu/100);  
   for(i in 2:N) {
      mu[i] ~ normal(mu[i-1], var_mu/100);
   }
   
   ## Time-variant regression coefficient of TV
   beta_TV[1] ~ normal(beta_TV_0, var_TV_coef/100);
   for(i in 2:N) {
      beta_TV[i] ~ normal(beta_TV[i-1], var_TV_coef/100);
   }

   ## Order Num
   for(i in 1:N) {
      Y[i] ~ normal(mu[i] + beta_TV[i] * TV[i], var_error/100);
      // Y[i] ~ normal(mu[i] + beta_TV * TV[i], var_error/100);
   }
}
