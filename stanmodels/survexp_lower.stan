data {
 int<lower=0> n_event; // # uncensored obs                                     
 int<lower=0> n_censor; // # censored obs                                    
 int<lower=0> p; // # covariates excluding intercept                                          
 matrix[n_event,p] X_event; // uncensored obs design matrix                           
 matrix[n_censor,p] X_censor; // censored  -----"----------                          
 vector<lower=0>[n_event] T_event; // uncensored survival times                         
 vector<lower=0>[n_censor] lower_censor; // lower bound of censored survival times = censor time                   
}
parameters {
    real alpha; //intercept
    vector[p] beta; // coefficients
    vector<lower = 1>[n_censor] T_censor_raw; // additional survival time after censor time
}
transformed parameters {
    vector[n_censor] T_censor = lower_censor + T_censor_raw; // total survival time of censored obs
}
model {
    vector[n_event] nu_event = exp(alpha + X_event * beta);
    vector[n_censor] nu_censor = exp(alpha + X_censor * beta);
    vector[(n_event + n_censor)] T = append_row(T_event, T_censor);
    vector[(n_event + n_censor)] nu = append_row(nu_event, nu_censor);
    alpha ~ normal(0, 100);
    beta ~ normal(0, 5);
    target += exponential_lpdf(T | nu); 
}