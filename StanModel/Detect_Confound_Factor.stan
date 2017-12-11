data {
   int<lower=0> N;
   int<lower=0> D;
   matrix[N, D] X;
   vector[N] Y;
}

parameters {
   real a;
   vector[D] beta;
   real<lower=0> sig;
}

model {
   for (d in 1:D) {
      beta[d] ~ normal(0, 1);
   }
   Y ~ normal(a + X * beta, sig);
}
