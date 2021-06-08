library(here)
library(readstata13)
library(tidyverse)
library(dplyr)
library(readxl)
library(rstan)
library(bayesplot)
library(shinystan)
library(foreach)
library(doParallel)
library(survival)
#options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(149)

# Data
  # https://www.tandfonline.com/doi/full/10.1080/13501763.2020.1773905
  fem = read.dta13(here('data/Krauss2020_WomenCabinet.dta'))

  # https://erdda.org/erd/data-archive/
  erd = read_xls(here('data/Andersson2014_ERD.xls'))
  erd = erd %>% mutate(v002e,
                       'start_date' = v004e,
                       'end_date' = v005e)

  # join by cabinet ID for starting date
  fem = left_join(fem, erd, by = 'v002e') %>% mutate(start_date = as.Date(start_date),
                                                     end_date = as.Date(end_date))

  # compute end dates for cabinets not included in ERD
  fem2 = data.frame()
  for(i in 1:length(unique(fem$country_name_short))){
    temp = fem[fem$country_name_short == unique(fem$country_name_short)[i],] # subset to country
    temp = temp %>% arrange(v002e) # bring in correct order
    for(j in 2:nrow(temp)){ 
      if(is.na(temp$start_date[j])){ # if no date, i.e. not included in ERD
        temp$start_date[j] = temp$end_date[j-1] # set start_date as end_date of previous cabinet
        temp$end_date[j] = temp$start_date[j] + temp$duration[j] # compute end_date as start_date + duration
      } else if(is.na(temp$end_date[j])){ # special ERD case where end date wasn't coded
        temp$end_date[j] = temp$start_date[j] + temp$duration[j] # compute end_date as start_date + duration
      }
    }
    fem2 = rbind(fem2, temp)
  }
rm(i, j, temp)

tt = function(x, t, ...) x * log(t)
fit.cph = coxph(Surv(duration, failure) ~ share_women_executive + maxdur + ciep + 
                  pref + minority + enpp, data = KK20)
cox.zph(fit.cph)

fit.cph = coxph(Surv(duration, failure) ~ share_women_executive + maxdur + ciep + 
                  pref + minority + enpp + decade, data = KK20)
cox.zph(fit.cph)

?cox.zph

fem$ciep = fem$ciep -2
fit.cph = coxph(Surv(duration, failure) ~ share_women_executive + maxdur + ciep + 
                  pref + minority + enpp, data = fem,
                tt = function(x, t, ...){x * log(t)})
summary(fit.cph)
plot(cox.zph(fit.cph))
fem$ciep = fem$ciep -2
fit.cph = coxph(Surv(duration, failure) ~ share_women_executive + maxdur + ciep + 
                  pref + as.factor(minority) + enpp, data = fem)

plot(cox.zph(fit.cph))
fit.cph = coxph(Surv(duration, failure) ~ share_women_executive + maxdur + ciep + 
                  pref + minority + enpp + I(pref*log(duration)) + I(ciep*log(duration)), data = fem)
summary(fit.cph)
cox.zph(fit.cph)
?summary.coxph
class(g)

?cox.zph

getAnywhere(coxph)
fem$duration


cox.zph(fit.cph)
summary(fit.cph)
install.packages("coxme")
library(coxme)
tt = function(x, t, ...) x * log(t)
fit.cme = coxme::coxme(Surv(duration, failure) ~ share_women_executive + maxdur + ciep + 
                         pref + minority + enpp + (1|country_name_short), data = fem)
summary(fit.cme)
summary(fem$maxdur)
sd(fem$pref)
?frailty
?frailty
clust
coxph()
cox.zph(fit.cph)
plot(cox.zph(fit.cph))
plot(fit.cme)
predict(fit.cme)
?predict.coxme
library(survminer)
ggcoxadjustedcurves(fit.cph)
ggcompetingrisks(fit.cph)
ggcoxfunctional(fit.cph)
cox.zph(fit.cph)
survfit(fit.cph)



fit.cph = coxph(Surv(duration, failure) ~ 1 + minority, x = T, model = T, data = fem)
summary(fit.cph)

ggplot(fem, aes(fill = as.factor(failure), x = as.factor(minority))) + 
  geom_bar(position = position_dodge())

fem %>% group_by(as.factor(failure), as.factor(minority)) %>%
  summarize(mu = mean(duration),
            sd = sd(duration))

cox.stan = "
data {
 int<lower=0> n_event;                                     
 int<lower=0> n_censor;                                        
 int<lower=0> p;                                                
 matrix[n_event,p] X_event;                               
 matrix[n_censor,p] X_censor;                           
 vector<lower=0>[n_event] T_event;                          
 vector<lower=0>[n_censor] T_censor;                      
}
parameters {
    real alpha;
    vector[p] beta;                                     
}
model {
    alpha ~ normal(-5,2);  
    beta ~ normal(0,2);                                                            
    target += exponential_lpdf(times_uncensored | exp(intercept+X_uncensored*betas)); 
    target += exponential_lccdf(times_censored | exp(intercept+X_censored*betas));  
}
generated quantities {
    vector[N_uncensored] times_uncensored_sampled;
    for(i in 1:N_uncensored) {
        times_uncensored_sampled[i] = exponential_rng(exp(intercept+X_uncensored[i,]*betas));
    }
}
"

x = tibble(c = c('a', NA, NA, 'b', '', NA),
           d = rnorm(6),
           g = c(rnorm(5), NA))

write(cox.stan, "coxph.stan")

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

fit2 = stan("coxph.stan", data = stan_data, cores = 4)
fit


plot(fit, pars = "betas")
exp(0.2423 + 2*0.1303)
exp(0.2423 -2*0.1303)
