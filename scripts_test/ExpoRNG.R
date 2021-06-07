setwd("~/Desktop")

smod = "
data {
  int<lower = 0> n;
  vector[n] X;
}
parameters {
  real alpha;
}
transformed parameters{
  real lambda;
  lambda = exp(alpha);
}
model { 
  alpha ~ normal(0, 100);
  target += exponential_lpdf(X|lambda);
}
generated quantities {
  real X_pred;
  X_pred = exponential_rng(lambda);
}
"
write(smod, "test.stan")

n = 100
X = rexp(n, exp(-10))

library(rstan)
m1 = stan("test.stan", data = list(n = n, X = X),
          cores = 4)
m1
plot(m1)
mean(X)
fit.exp_vector
colnames(X_event)
