data {
    int<lower=0> n_event; // # uncensored obs                                     
    int<lower=0> n_censor; // # censored obs                                    
    int<lower=0> p; // # covariates excluding intercept                                        
    matrix[n_event,p] X_event; // uncensored obs design matrix                           
    matrix[n_censor,p] X_censor; // censored  -----"----------
    matrix[3,p] X_pred; // for posterior predictive distribution
    vector<lower=0>[n_event] T_event; // uncensored survival times                         
    vector<lower=0>[n_censor] T_censor; // lower bound of censored survival times = censor time                  
}
parameters {
  real mu;
  vector[p] beta;
  real<lower = 0> alpha;
}
transformed parameters {
  vector[n_event] sigma_event = exp(- (mu + X_event * beta)/ alpha);
  vector[n_censor] sigma_censor = exp(- (mu + X_censor * beta)/ alpha);
}

model {
  mu ~ normal(0, 100);
  beta ~ normal(0, 5);
  alpha ~ cauchy(0, 2.5);
  target += weibull_lpdf(T_event | alpha, sigma_event);
  target += weibull_lccdf(T_censor | alpha, sigma_censor);
}
generated quantities {
  vector[n_event + n_censor] T_hat;
  vector[3] T_pred;
  for(i in 1:n_event){
    T_hat[i] = weibull_rng(alpha, sigma_event[i]);
  }
  for(i in 1:n_censor){
    T_hat[i + n_event] = weibull_rng(alpha, sigma_censor[i]);
  }
  for(i in 1:3){
    T_pred[i] = weibull_rng(alpha, exp(- (mu + X_pred * beta)/ alpha)[i]); 
  }
}
