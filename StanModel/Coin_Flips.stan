data {
   int<lower=1> N;
   int<lower=0, upper=N> W;
}

parameters {
   real<lower=0, upper=1> p;
}

model {
   W ~ binomial(N, p);
}
