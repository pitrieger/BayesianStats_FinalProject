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
    real alpha;
    vector[p] beta;                                     
}
transformed parameters{
    vector[n_event] lambda_event = exp(alpha + X_event * beta);
    vector[n_censor] lambda_censor = exp(alpha + X_censor * beta);
}
model {
    alpha ~ normal(0, 100);
    beta ~ normal(0, 5);
    target += exponential_lpdf(T_event | lambda_event); 
    target += exponential_lccdf(T_censor | lambda_censor);  
}
generated quantities {
  vector[n_event + n_censor] T_hat;
  vector[3] T_pred;
  for(i in 1:n_event){
    T_hat[i] = exponential_rng(lambda_event[i]);
  }
  for(i in 1:n_censor){
    T_hat[i + n_event] = exponential_rng(lambda_censor[i]);
  }
  for(i in 1:3){
    T_pred[i] = exponential_rng(exp(alpha + X_pred * beta)[i]);
  }
  
}
