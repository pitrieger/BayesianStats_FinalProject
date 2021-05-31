library(here)
library(readstata13)
library(tidyverse)
library(rstan)
library(survival)
library(readxl)
rstan_options(auto_write = TRUE)

### ### ### ###
###  Data  ####
### ### ### ###
KK20 = read.dta13(here('data/Krauss2020_WomenCabinet.dta'))

# https://erdda.org/erd/data-archive/
erd = read_xls(here('data/Andersson2014_ERD.xls'))
erd = erd %>% dplyr::select(v002e,
                            'start_date' = v004e,
                            'end_date' = v005e)

# join by cabinet ID for starting date
KK20 = left_join(KK20, erd, by = 'v002e') %>% mutate(start_date = as.Date(start_date),
                                                     end_date = as.Date(end_date))

# compute end dates for cabinets not included in ERD
KK20_2 = data.frame()
for(i in 1:length(unique(KK20$country_name_short))){
  temp = KK20[KK20$country_name_short == unique(KK20$country_name_short)[i],] # subset to country
  temp = temp %>% arrange(v002e) # bring in correct order
  for(j in 2:nrow(temp)){ 
    if(is.na(temp$start_date[j])){ # if no date, i.e. not included in ERD
      temp$start_date[j] = temp$end_date[j-1] # set start_date as end_date of previous cabinet
      temp$end_date[j] = temp$start_date[j] + temp$duration[j] # compute end_date as start_date + duration
    } else if(is.na(temp$end_date[j])){ # special ERD case where end date wasn't coded
      temp$end_date[j] = temp$start_date[j] + temp$duration[j] # compute end_date as start_date + duration
    }
  }
  KK20_2 = rbind(KK20_2, temp)
}
KK20 = KK20_2; rm(KK20_2, i, j, temp, erd)

# transform variables
KK20 = KK20 %>%
  mutate(year = format(start_date, "%Y") %>% as.numeric(),
         year_fac = as.factor(year),
         decade = as.factor(decade),
         ciep = as.factor(ciep),
         maxdur = scale(maxdur),
         share_women_executive = share_women_executive / 100)

### ### ### ### ### ### ### ### ###
### Exponential Survival model ####
### ### ### ### ### ### ### ### ###
# data in stan format
event = KK20$failure
T_event = KK20$duration[event == 1]
T_censor = KK20$duration[event == 0]
X = model.matrix(~ -1 + share_women_executive + pref + enpp + 
                   decade + ciep + minority + maxdur, 
                 data = KK20)
p = ncol(X)
X_event = matrix(X[event,], ncol = p)
X_censor = matrix(X[!event,], ncol = p)  
n_event = nrow(X_event)
n_censor = nrow(X_censor)

KK20.stan_vector = list(n_event = n_event, n_censor = n_censor, p = p,
                        X_event = X_event, X_censor = X_censor,
                        T_event = T_event, T_censor = T_censor)
#KK20.stan_lower = KK20.stan_vector 
#names(KK20.stan_lower)[names(KK20.stan_lower) == "T_censor"] = "lower_censor"

# run models
fit.exp_vector = stan(here("stanmodels", "survexp_vector.stan"),
                      data = KK20.stan_vector, cores = 4)
fit.exp_vector

plot(fit.exp_vector, pars = "T_event_ppd[284]")
X284 = extract(fit.exp_vector, pars = "T_event_ppd[284]")
plot(density(unlist(X284)))
abline(v = T_event[284])

plot(fit.exp_vector, pars = "T_event_ppd[200]")
X200 = extract(fit.exp_vector, pars = "T_event_ppd[200]")
plot(density(unlist(X200)))
abline(v = T_event[200])

plot(fit.exp_vector, pars = "T_event_ppd[284]")
X1 = extract(fit.exp_vector, pars = "T_event_ppd[284]")
plot(density(unlist(X284)))
abline(v = T_event[284])
T_event
hist(T_event)
plot(fit.exp_vector, pars = c('alpha', 'beta')) 
colnames(X)

#fit.exp_lower = stan(here("stanmodels", "survexp_lower.stan"),
#                      data = KK20.stan_lower, cores = 4)
#fit.exp_lower
colnames(X)
