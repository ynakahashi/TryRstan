data {
   int N;
   int K;
   real Y[N];
   real TV[N];
   real Digi[N];
   int<lower=1, upper=K> Reg[N];
}

parameters {
   real a0;
   real b0;
   real c0;
   real a[K];
   real b[K];
   real c[K];
   real<lower=0> s_a;
   real<lower=0> s_b;
   real<lower=0> s_c;
   real<lower=0> s_Y;
}

/*
transformed parameters{
   real a[K];
   real b[K];
   for (k in 1:K) {
      a[k] = a0 + ak[k];
      b[k] = b0 + bk[k];
   }
}
*/

model {
   for (k in 1:K) {
      a[k] ~ normal(a0, s_a);
      b[k] ~ normal(b0, s_b);
      c[k] ~ normal(c0, s_c);
   }
   
   for (n in 1:N) 
      Y[n] ~ normal(a[Reg[n]] + b[Reg[n]]*TV[n] + c[Reg[n]]*Digi[n], s_Y);
}
