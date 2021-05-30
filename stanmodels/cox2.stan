

data {
 int<lower=1> N_uncensored;                                     
 int<lower=1> N_censored;                                        
 int<lower=0> NC;                                                
 matrix[N_censored,NC] X_censored;                               
 matrix[N_uncensored,NC] X_uncensored;
 vector<lower=0>[N_censored] times_censored;
 vector<lower=0>[N_uncensored] times_uncensored;                      
}
parameters {
    vector<lower = 0>[N_censored] T_raw;
    vector[NC] betas;                                     
    real intercept;                       
}
transformed parameters{
    vector[N_censored] T_censored = times_censored + T_raw + 1;
}
model {
    betas ~ normal(0,2);                                                            
    intercept ~ normal(-5,2);                                                     
    target += exponential_lpdf(times_uncensored | exp(intercept+X_uncensored*betas));
    target += exponential_lpdf(T_censored |exp(intercept+X_censored*betas));
}

