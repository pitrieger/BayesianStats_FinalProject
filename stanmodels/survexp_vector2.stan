data {
    int<lower=0> n_event; // # uncensored obs                                     
    int<lower=0> n_censor; // # censored obs                                    
    int<lower=0> p; // # covariates excluding intercept                                        
    matrix[n_event,p] X_event; // uncensored obs design matrix                           
    matrix[n_censor,p] X_censor; // censored  -----"----------                          
    vector<lower=0>[n_event] T_event; // uncensored survival times                         
    vector<lower=0>[n_censor] T_censor; // lower bound of censored survival times = censor time                  
}
parameters {
    real alpha;
    vector[p] beta;
    real mu_alpha;
    vector[p] mu_beta;
}
transformed parameters{
    vector[n_event] lambda_event = exp(alpha + X_event * beta);
    vector[n_censor] lambda_censor = exp(alpha + X_censor * beta);
}
model {
    mu_alpha ~ normal(0, 100);
    mu_beta ~ normal(0, 100);
    alpha ~ normal(mu_alpha, 5);
    beta ~ normal(mu_beta, 5);
    target += exponential_lpdf(T_event | lambda_event); 
    target += exponential_lccdf(T_censor | lambda_censor);  
}
generated quantities{
    vector[n_event] T_event_ppd;
    for(i in 1:n_event) {
        T_event_ppd[i] = exponential_rng(exp(alpha + X_event[i] * beta));
    }
}
