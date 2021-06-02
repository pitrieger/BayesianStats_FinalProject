library(here)
library(readstata13)
library(tidyverse)
library(ggridges)
library(RColorBrewer)
library(rstan)
library(bayesplot)
library(survival)
library(readxl)
rstan_options(auto_write = TRUE)

### ### ### ###
###  Data  ####
### ### ### ###
KK20 = read.dta13(here('data/Krauss2020_WomenCabinet.dta'))

# https://erdda.org/erd/data-archive/
erd = read_xls(here('data/Andersson2014_ERD.xls'))
erd = erd %>% fill(v009e, .direction = "up") %>%
              dplyr::select(v002e,
                            'start_date' = v004e,
                            'end_date' = v005e,
                            'end_date_period' = v009e)

# join by cabinet ID for starting date
KK20 = left_join(KK20, erd, by = 'v002e') %>% mutate(start_date = as.Date(start_date),
                                                     end_date = as.Date(end_date), 
                                                     end_date_period = as.Date(end_date_period))

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
         maxdur_raw = maxdur,
         maxdur = scale(maxdur))

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

KK20.standat_exp_vector = list(n_event = n_event, n_censor = n_censor, p = p,
                        X_event = X_event, X_censor = X_censor,
                        T_event = T_event, T_censor = T_censor)
#KK20.stan_lower = KK20.stan_vector 
#names(KK20.stan_lower)[names(KK20.stan_lower) == "T_censor"] = "lower_censor"

# run models
fit.exp_vector = stan(here("stanmodels", "survexp_vector.stan"),
                      data = KK20.standat_exp_vector, cores = 4,
                      control = list(max_treedepth = 15))
fit.exp_vector

## Plot Coefficients
coef.samples = as.data.frame(extract(fit.exp_vector, pars = c("alpha", "beta")))
colnames(coef.samples) = c("Intercept", colnames(X))
coef.samples = coef.samples %>%
  select(!starts_with("decade")) %>%
  pivot_longer(1:8, names_to = "parameter")
coef.samples$parameter = factor(coef.samples$parameter,
                           levels = c("Intercept", "ciep4", "ciep5", "enpp", 
                                      "minority", "pref", "maxdur", 
                                      "share_women_executive"),
                           labels = c("Intercept", "CIEP: 4", "CIEP: 5", "ENPP",
                                      "Minority", "Ideological\ndivisiveness", "Max.\nduration", 
                                      "Share of Women\nin Cabinet"))

ggplot(coef.samples, aes(x = value, y = parameter)) + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") + 
  stat_density_ridges(quantile_fun = mean, quantile_lines = T,alpha = 0.5, fill = "orangered3") + 
  scale_x_continuous(limits = c(-20, 15)) + 
  labs(x = "Coefficient") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank())

## Plot convergence
plot(fit.exp_vector)
trace(fit.exp_vector)
coef.chains = traceplot(fit.exp_vector, pars = c("alpha", "beta"))$data
coef.chains$parameter = factor(coef.chains$parameter, 
                               levels = c("alpha", "beta[1]", "beta[2]", "beta[3]", 
                               "beta[4]", "beta[5]", "beta[6]", "beta[7]", "beta[8]", 
                               "beta[9]", "beta[10]", "beta[11]", "beta[12]", "beta[13]", 
                               "beta[14]", "beta[15]"),
                               labels = c("Intercept", "Share of Women\nin Cabinet", 
                                          "Ideological\ndivisiveness", "ENPP",
                                          "Decade 1940", "Decade 1950", "Decade 1960", "Decade 1970",
                                          "Decade 1980", "Decade 1990", "Decade 2000", "Decade 2010",
                                          "CIEP: 4", "CIEP: 5", "Minority", "Max.\nduration"))
ggplot(coef.chains, aes(x = iteration, y = value, color = chain)) + 
  geom_line(alpha = 0.4) + 
  facet_wrap(~parameter, scales = "free_y") + 
  scale_color_manual(values = brewer.pal(4, "Dark2")) +
  labs(x = "Iteration", y = "Coefficient", color = "Chain") + 
  theme_minimal() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")


## Posterior Predictive Distribution
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

### ### ###
### ZIP ###
### ### ### 
KK20$Y = (KK20$maxdur_raw - KK20$duration)
KK20$Y[KK20$failure == 0] = 0

#library(pscl)
#fit.pscl = zeroinfl(Y ~ share_women_executive + pref + enpp + 
#                      decade + ciep + minority, 
#                    data = KK20, dist = "poisson", link = "logit")
#summary(fit.pscl)

X = model.matrix(Y ~ share_women_executive + pref + enpp + 
                   decade + ciep + minority + maxdur, data = KK20)
Z = X
Y = KK20$Y

## Vectorized Stan
KK20.standat_ZIP = list(n = nrow(X), p = ncol(X), m = ncol(Z), 
                   y_nonzero = Y[Y!=0],
                   n_nonzero = sum(Y!=0),
                   n_zero = sum(Y == 0),
                   X_nonzero = X[Y!=0,],
                   X_zero = X[Y==0,],
                   Z_nonzero = Z[Y!=0,],
                   Z_zero = Z[Y==0,])

fit.ZIP = stan(here("stanmodels", "ZIP2.stan"), data = KK20.standat_ZIP,
                pars = c("beta", "gamma"),
                cores = 4)
fit.ZIP
# Why not working properly when adding maxdur? 
# even without, some Rhats not that great, especially for ZI part


# Plot Coefficients
coef.samples = as.data.frame(extract(fit.ZIP, pars = c("beta", "gamma")))
colnames(coef.samples) = c(paste0("beta_", colnames(X)), paste0("gamma_", colnames(Z)))
coef.samples = coef.samples %>%
  select(!matches("decade")) %>%
  pivot_longer(1:16, names_to = "parameter") %>%
  mutate(part = str_extract(parameter, "beta|gamma"),
         parameter = str_extract(parameter, "\\_.+") %>% substring(., 2))
coef.samples$parameter = factor(coef.samples$parameter,
                           levels = c("(Intercept)", "ciep4", "ciep5", "enpp", 
                                      "minority", "pref", "maxdur", 
                                      "share_women_executive"),
                           labels = c("Intercept", "CIEP: 4", "CIEP: 5", "ENPP",
                                      "Minority", "Ideological\ndivisiveness", "Max.\nduration", 
                                      "Share of Women\nin Cabinet"))

ggplot(coef.samples, aes(x = value, y = parameter)) + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  stat_density_ridges(quantile_fun = mean, quantile_lines = T,alpha = 0.5, fill = "orangered3") + 
  #scale_x_continuous(limits = c(-20, 15)) + 
  labs(x = "Coefficient") + 
  facet_wrap(~part, scales = "free_x") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank())
?facet_grid
