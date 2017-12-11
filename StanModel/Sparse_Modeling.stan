data {
   int<lower=0> N;
   int<lower=0> D;
   matrix[N, D] X;
   vector[N] Y;
}

parameters {
   real a;
   vector[D] beta;
   vector<lower=0>[D] lambda;
   real<lower=0> tau;
   real<lower=0> sigma;
}

model {
   a ~ uniform(0, 1);
   lambda ~ cauchy(0, 1);
   tau ~ cauchy(0, 1);
   
   for (d in 1:D) {
      beta[d] ~ normal(0, lambda[d] * tau);
   }

   Y ~ normal(a + X * beta, sigma);
}
