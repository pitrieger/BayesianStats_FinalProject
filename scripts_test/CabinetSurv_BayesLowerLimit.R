
library(survminer)
fem$pref2 = cut(fem$pref, c(0, 30, max(fem$pref)))

ggsurvplot(
  fit = survfit(Surv(duration, failure) ~ 1 + pref2, data = fem), 
  xlab = "Days", 
  ylab = "Overall survival probability")
hist(fem$pref)
share_women_executive + maxdur + ciep + 
  pref + minority + enpp + tt(pref) + tt(ciep)


cox2.stan = "

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
"

write(cox2.stan, "cox2.stan")

N <- nrow(fem)
X <- as.matrix(pull(fem, minority))
is_censored <- pull(fem,failure)==0
times <- pull(fem,duration)
msk_censored <- is_censored == 1
N_censored <- sum(msk_censored)

stan_data <- list(N_uncensored=N-N_censored, 
                  N_censored=N_censored, 
                  X_censored=as.matrix(X[msk_censored,]),
                  X_uncensored=as.matrix(X[!msk_censored,]),
                  times_censored=times[msk_censored],
                  times_uncensored = times[!msk_censored],
                  NC=ncol(X)
)

fit = stan("cox2.stan", data = stan_data, cores = 4)
summary(fit, pars = c("intercept", "betas"))

fit2 = stan("coxph.stan", data = stan_data, cores = 4)
summary(fit2, pars = c("intercept", "betas"))
