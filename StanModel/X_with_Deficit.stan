data {
   int<lower=0> N_obs;
   int<lower=0> N_mis;
   real Y[N_obs + N_mis];
   real X1[N_obs];
   real X2[N_obs + N_mis];
   real X3[N_obs + N_mis];
   real X4[N_obs + N_mis];
}

parameters {
   real a;
   real b1;
   real b2;
   real b3;
   real b4;
   real<lower=0> sigma;
   real X_mis[N_mis];
}

model {
   for (i in 1:N_obs) {
      Y[i] ~ normal(a + b1 * X1[i] + b2 * X2[i] + b3 * X3[i] + b4 * X4[i],
      sigma);     
   }
   for (i in 1:N_mis) {
      Y[N_obs + i] ~ normal(a + b1 * X_mis[i] + b2 * X2[N_obs + i] + 
      b3 * X3[N_obs + i] + b4 * X4[N_obs + i],
      sigma);     
   }
   X_mis ~ normal(mean(X1), sd(X1));
}
