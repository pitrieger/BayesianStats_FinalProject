data {
  int<lower=0> n; // obs
  int<lower=1> p; // number covariates count
  int<lower=1> m; // number covariates zero
  matrix[n, p] X; // covariates count
  matrix[n, m] Z; // covariates zero
  int<lower=0> Y[n]; // days too early relative to regular termination
}
parameters {
  vector[p] beta; // coef count
  vector[m] gamma; // coef zero
}
transformed parameters{
  vector[n] lambda; // poisson param
  vector[n] theta; // binomial param
  lambda = exp(X*beta);
  theta = inv_logit(Z*gamma);
}
model {
  beta ~ normal(0, 2);
  gamma ~ normal(0, 2);
  for (i in 1:n) {
    if (Y[i] == 0)
      target += log_sum_exp(bernoulli_lpmf(1 | theta),
                            bernoulli_lpmf(0 | theta)
                              + poisson_lpmf(Y[i] | lambda));
    else
      target += bernoulli_lpmf(0 | theta)
                  + poisson_lpmf(Y[i] | lambda);
  }
}
