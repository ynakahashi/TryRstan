data {
   int N;
   int K;
   real Y[N];
   real TV[N];
   real Digi[N];
//   real TV_ADS[N];
//   real Digi_ADS[N];
   int<lower=1, upper=K> Reg[N];
}


transformed data {
   real TV_ADS[N];
   real Digi_ADS[N];
   real<lower=0, upper=1> dr_TV[K];
   real<lower=0, upper=1> dr_Digi[K];

   for (n in 2:N) {
      TV_ADS[n]   = TV[n] + TV[n-1] * dr_TV[Reg[n]];
      Digi_ADS[n] = Digi[n] + Digi[n-1] * dr_Digi[Reg[n]];
   }
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
   real<lower=0, upper=1> dr_TV0;
   real<lower=0, upper=1> dr_Digi0;
   real<lower=0> s_drTV;
   real<lower=0> s_drDigi;
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
      a[k]       ~ normal(a0, s_a);
      b[k]       ~ normal(b0, s_b);
      c[k]       ~ normal(c0, s_c);
      dr_TV[k]   ~ normal(dr_TV0, s_drTV);
      dr_Digi[k] ~ normal(dr_Digi0, s_drDigi);
   }

//   for (k in 1:K) {
//      dr_TV[k]   ~ normal(dr_TV0, s_drTV);
//      dr_Digi[k] ~ normal(dr_Digi0, s_drDigi);
//   }

   for (n in 1:N) 
      Y[n] ~ normal(a[Reg[n]] + b[Reg[n]] * TV_ADS[n] + c[Reg[n]]*Digi[n], s_Y);
}
