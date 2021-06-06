data {
  int<lower=0> n; // obs
  int<lower = 1, upper = n> n_nonzero;
  int<lower = 1, upper = n> n_zero;
  int<lower=1> p; // number covariates count
  int<lower=1> m; // number covariates zero
  int<lower=0> y_nonzero[n_nonzero];
  matrix[n_nonzero, p] X_nonzero; // covariates count
  matrix[n_zero, p] X_zero; // covariates count
  matrix[n_nonzero, m] Z_nonzero; // covariates zero
  matrix[n_zero, m] Z_zero; // covariates zero
}
parameters {
  vector[p] beta; // coef count
  vector[m] gamma; // coef zero
}
transformed parameters{
  vector[n_nonzero] lambda_nonzero; // poisson param
  vector[n_zero] lambda_zero; // poisson param
  vector[n_nonzero] theta_nonzero; // binomial param
  vector[n_zero] theta_zero; // binomial param
  lambda_nonzero = exp(X_nonzero*beta);
  lambda_zero = exp(X_zero*beta);
  theta_nonzero = inv_logit(Z_nonzero*gamma);
  theta_zero = inv_logit(Z_zero*gamma);
}
model {
  beta ~ normal(0, 5);
  gamma ~ normal(0, 5);
  target += log_sum_exp(bernoulli_lpmf(1 | theta_zero),
                                 bernoulli_lpmf(0 | theta_zero)+
                                 poisson_lpmf(0 | lambda_zero));
   target += bernoulli_lpmf(0 | theta_nonzero);
   target += poisson_lpmf(y_nonzero | lambda_nonzero);
}

