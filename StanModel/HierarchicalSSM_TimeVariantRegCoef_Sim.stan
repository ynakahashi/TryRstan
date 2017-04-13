data {
   int N;
   int K;
   vector[N*K] Y;
   vector[N*K] X;
}

parameters {
   real state_t0;
   real beta_X_0;
   vector[N*K] state_t;
   vector[N*K] beta_X;
   real<lower=0> var_state_t0;
   real<lower=0> var_state;
   real<lower=0> var_beta_X;
   real<lower=0> var_error;
}

model {
   ## local trend
   for (k in 1:K) {
      state_t[1 + (k-1)*N] ~ normal(state_t0, var_state_t0/100);  
      for(i in 2:N) {
         state_t[i + (k-1)*N] ~ normal(state_t[i-1 + (k-1)*N], var_state/100);
      }  
   }
   
   ## Time-variant regression coefficient of X
   for (k in 1:K) {
      beta_X[1 + (k-1)*N] ~ normal(beta_X_0, var_beta_X/100);
      for(i in 2:N) {
         beta_X[i + (k-1)*N] ~ normal(beta_X[i-1 + (k-1)*N], var_beta_X/100);
      }
   }

   ## Order Num
   for(i in 1:(N*K)) {
      Y[i] ~ normal(state_t[i] + beta_X[i] * X[i], var_error/100);
   }
}
