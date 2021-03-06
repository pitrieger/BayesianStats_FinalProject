library(here)
library(readstata13)
library(tidyverse)
library(ggridges)
library(RColorBrewer)
library(rstan)
library(bayesplot)
library(survival)
library(readxl)
library(stargazer)
rstan_options(auto_write = TRUE)
set.seed(505)


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

# data in stan format
event = KK20$failure
T_event = KK20$duration[event == 1]
T_censor = KK20$duration[event == 0]
X = model.matrix(~ share_women_executive + pref + enpp + 
                   decade + ciep + minority + maxdur, 
                 data = KK20)
X = X[,-1]
p = ncol(X)
X_event = X[event == 1,]
X_censor = X[event == 0,]  
n_event = nrow(X_event)
n_censor = nrow(X_censor)
X_pred = rbind(c(min(X[,1]), mean(X[,2]), mean(X[,3]), rep(0, 6), 1, 1, 0, 0, mean(X[,14])),
               c(mean(X[,1]), mean(X[,2]), mean(X[,3]), rep(0, 6), 1, 1, 0, 0, mean(X[,14])),
               c(max(X[,1]), mean(X[,2]), mean(X[,3]), rep(0, 6), 1, 1, 0, 0, mean(X[,14])))
KK20.standat = list(n_event = n_event, n_censor = n_censor, p = p,
                    X_event = X_event, X_censor = X_censor,
                    T_event = T_event, T_censor = T_censor,
                    X_pred = X_pred)

### ### ### ### ### ### 
### Descriptives ######
### ### ### ### ### ###
KK20$fail = ifelse(KK20$failure == 1, "Observed failure", "Censored")
ggplot(KK20, aes(x = duration)) +
  geom_histogram(color = "black", fill = "white", bins = 40) +
  facet_wrap(~fail, ncol = 1)+
  labs(x = "Duration") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 8))
KK20 %>% group_by(fail) %>% summarize(m = median(duration))
ggsave(here("results", "fig5_DVsummary.pdf"),
       width = 6, height = 4)

stargazer(as.data.frame(X))
round(100 * summary(KK20$decade)/nrow(KK20), 1)
round(100 * summary(KK20$ciep)/nrow(KK20), 1)

plot(density(extract(fit.weib_vector, pars = "alpha") %>% unlist()))
plot(seq(0, 15, l = 400), exp(dnorm(seq(0, 15, l = 400), 0, 2)))
l = exp(10 * rnorm(1000))
l = l[l<50]
hist(l, breaks = 500)
hist(abs(rcauchy(1000, 0, 100)))
hist(exp(10 * rnorm(1000)))

### ### ### ### ### ### ### ### ###
### Exponential Survival model ####
### ### ### ### ### ### ### ### ###
# run model
fit.exp_vector = stan(here("stanmodels", "survexp_vector.stan"),
                      data = KK20.standat, cores = 4,
                      control = list(max_treedepth = 10),
                      iter = 4000,
                      seed = 942)
fit.exp_vector

## Plot Coefficients
coef.samples = as.data.frame(extract(fit.exp_vector, pars = c("alpha", "beta")))
colnames(coef.samples) = c("Intercept", colnames(X))
coef.samples = coef.samples %>%
  pivot_longer(1:15, names_to = "parameter")
coef.samples$parameter = factor(coef.samples$parameter,
                           levels = rev(c("Intercept", "decade1950", "decade1960",
                                      "decade1970", "decade1980", "decade1990", "decade2000", "decade2010",
                                      "ciep4", "ciep5", "enpp", 
                                      "minority", "pref", "maxdur", 
                                      "share_women_executive")),
                           labels = rev(c("Intercept",  "Decade 1950", "Decade 1960", 
                                          "Decade 1970", "Decade 1980", "Decade 1990", "Decade 2000", "Decade 2010",
                                      "CIEP: 4", "CIEP: 5", "ENPP",
                                      "Minority", "Ideological\ndivisiveness", "Max.\nduration", 
                                      "Share of Women\nin Cabinet")))

ggplot(coef.samples, aes(x = value)) + 
  geom_density(fill = "orangered3", alpha = 0.7) + 
  facet_wrap(~parameter, scales = "free") + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") + 
  labs(x = "Coefficient") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 40, hjust = 1.1, vjust = 1.3))

ggsave(here("results", "fig1_exp_coefplot.pdf"),
       width = 6, height = 5)


## Plot convergence
coef.chains = traceplot(fit.exp_vector, pars = c("alpha", "beta"))$data
coef.chains$parameter = factor(coef.chains$parameter, 
                               levels = c("alpha", "beta[1]", "beta[2]", "beta[3]", 
                               "beta[4]", "beta[5]", "beta[6]", "beta[7]", "beta[8]", 
                               "beta[9]", "beta[10]", "beta[11]", "beta[12]", "beta[13]", 
                               "beta[14]"),
                               labels = c("Intercept", "Share of Women\nin Cabinet", 
                                          "Ideological\ndivisiveness", "ENPP",
                                          "Decade 1950", "Decade 1960", "Decade 1970",
                                          "Decade 1980", "Decade 1990", "Decade 2000", "Decade 2010",
                                          "CIEP: 4", "CIEP: 5", "Minority", "Max.\nduration"))
coef.chains$parameter = factor(coef.chains$parameter, levels = rev(c("Intercept", "Decade 1940", "Decade 1950", "Decade 1960", 
                                                                     "Decade 1970", "Decade 1980", "Decade 1990", "Decade 2000", "Decade 2010",
                                                                     "CIEP: 4", "CIEP: 5", "ENPP",
                                                                     "Minority", "Ideological\ndivisiveness", "Max.\nduration", 
                                                                     "Share of Women\nin Cabinet")))


ggplot(coef.chains, aes(x = iteration, y = value, color = chain)) + 
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) + 
  facet_wrap(~parameter, scales = "free_y") + 
  scale_color_manual(values = brewer.pal(4, "Dark2")) +
  labs(x = "Iteration", y = "Coefficient", color = "Chain") + 
  theme_minimal() + 
  guides(color = guide_legend(ncol = 2)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, vjust = 1.3, hjust = 1.1),
        legend.position = c(0.92, 0.04),
        legend.title.align = 0.5)

ggsave(here("results", "fig2_exp_convergence.pdf"),
       width = 6, height = 5)

## Posterior Predictive Distribution
T_pred = as.data.frame(extract(fit.exp_vector, pars = "T_pred"))
colnames(T_pred) = c("Minimum Share of Women in Cabinet", "Mean Share of Women in Cabinet", "Maximum Share of Women in Cabinet")
T_pred = T_pred %>% pivot_longer(cols = 1:3, names_to = "type",
                                 values_to = "T_pred")
ggplot(T_pred, aes(x = T_pred)) +
  geom_density(fill = "orangered3", alpha = 0.7) + 
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  scale_x_continuous(limits = c(0, 30000)) + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") + 
  labs(x = "Posterior Predicitive Distribution\nDuration of Cabinet") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 8))

ggsave(here("results", "fig3_exp_posteriorpredict.pdf"),
       width = 6, height = 4)

## Theoretical survplot
alpha = extract(fit.exp_vector, pars = c("alpha")) %>% unlist()
beta = extract(fit.exp_vector, pars = c("beta"))$beta
nu = exp(as.numeric(mean(alpha) + X_pred[1,2:14] %*% colMeans(beta[,2:14])) + as.matrix(X_pred[,1]) %*% t(as.matrix(beta[,1])))
nu = apply(nu, 1, function(x) c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.925)))
t_grid = 0:2000
surv.df = data.frame(t = t_grid,
                     type = rep(c("Minimal", "Mean", "Maximal"), each = length(t_grid)),
                     Survival = 1 - c(pexp(t_grid, nu[2, 1]), pexp(t_grid, nu[2, 2]), pexp(t_grid, nu[2, 3])),
                     Survival_lower = 1 - c(pexp(t_grid, nu[1, 1]), pexp(t_grid, nu[1, 2]), pexp(t_grid, nu[1, 3])),
                     Survival_upper = 1 - c(pexp(t_grid, nu[3, 1]), pexp(t_grid, nu[3, 2]), pexp(t_grid, nu[3, 3])))

ggplot(surv.df, aes(x = t, y = Survival, ymin = Survival_lower, ymax = Survival_upper, 
                    color = type, fill = type)) + 
  geom_ribbon(alpha=0.4, color = NA) + 
  geom_line() +
  labs(x = "Duration", y = "Survival",
       color = "Share of Women in Cabinet",
       fill = "Share of Women in Cabinet") + 
  scale_color_manual(values = brewer.pal(3, "Dark2")) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")) +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(title.position = 'top', title.hjust = 0.5))

ggsave(here("results", "fig4_exp_survplot.pdf"),
       width = 6, height = 4)

### ### ### ###
### Weibull ####
### ### ### ###
# fit model
fit.weib_vector = stan(here("stanmodels", "survweib_vector.stan"), 
                       data = KK20.standat,
                       cores = 4,
                       iter = 4000,
                       seed = 481)
fit.weib_vector

# coefficient plot
coef.samples = as.data.frame(extract(fit.weib_vector, pars = c("mu", "beta")))
colnames(coef.samples) = c("Intercept", colnames(X))
coef.samples = coef.samples %>%
  pivot_longer(1:15, names_to = "parameter")
coef.samples$parameter = factor(coef.samples$parameter,
                                levels = rev(c("Intercept", "decade1950", "decade1960",
                                               "decade1970", "decade1980", "decade1990", "decade2000", "decade2010",
                                               "ciep4", "ciep5", "enpp", 
                                               "minority", "pref", "maxdur", 
                                               "share_women_executive")),
                                labels = rev(c("Intercept",  "Decade 1950", "Decade 1960", 
                                               "Decade 1970", "Decade 1980", "Decade 1990", "Decade 2000", "Decade 2010",
                                               "CIEP: 4", "CIEP: 5", "ENPP",
                                               "Minority", "Ideological\ndivisiveness", "Max.\nduration", 
                                               "Share of Women\nin Cabinet")))

ggplot(coef.samples, aes(x = value)) + 
  geom_density(fill = "orangered3", alpha = 0.7) + 
  facet_wrap(~parameter, scales = "free") + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") + 
  labs(x = "Coefficient") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 40, hjust = 1.1, vjust = 1.3))

ggsave(here("results", "fig1_weib_coefplot.pdf"),
       width = 6, height = 5)


## Plot convergence
coef.chains = traceplot(fit.weib_vector, pars = c("mu", "beta", "alpha"))$data
coef.chains$parameter = factor(coef.chains$parameter, 
                               levels = c("alpha", "mu", "beta[1]", "beta[2]", "beta[3]", 
                                          "beta[4]", "beta[5]", "beta[6]", "beta[7]", "beta[8]", 
                                          "beta[9]", "beta[10]", "beta[11]", "beta[12]", "beta[13]", 
                                          "beta[14]"),
                               labels = c("alpha", "Intercept", "Share of Women\nin Cabinet", 
                                          "Ideological\ndivisiveness", "ENPP",
                                          "Decade 1950", "Decade 1960", "Decade 1970",
                                          "Decade 1980", "Decade 1990", "Decade 2000", "Decade 2010",
                                          "CIEP: 4", "CIEP: 5", "Minority", "Max.\nduration"))
coef.chains$parameter = factor(coef.chains$parameter, levels = rev(c("alpha", "Intercept", "Decade 1940", "Decade 1950", "Decade 1960", 
                                                                     "Decade 1970", "Decade 1980", "Decade 1990", "Decade 2000", "Decade 2010",
                                                                     "CIEP: 4", "CIEP: 5", "ENPP",
                                                                     "Minority", "Ideological\ndivisiveness", "Max.\nduration", 
                                                                     "Share of Women\nin Cabinet")))


ggplot(coef.chains, aes(x = iteration, y = value, color = chain)) + 
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) + 
  facet_wrap(~parameter, scales = "free_y") + 
  scale_color_manual(values = brewer.pal(4, "Dark2")) +
  labs(x = "Iteration", y = "Coefficient", color = "Chain") + 
  theme_minimal() + 
  guides(color = guide_legend(nrow = 1, title.position = "top"), vjust = 0.5) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, vjust = 1.3, hjust = 1.1),
        legend.position = "bottom",
        legend.title.align = 0.5)

ggsave(here("results", "fig2_weib_convergence.pdf"),
       width = 6, height = 5)

## Posterior Predictive Distribution
T_pred = as.data.frame(extract(fit.weib_vector, pars = "T_pred"))
colnames(T_pred) = c("Minimum Share of Women in Cabinet", "Mean Share of Women in Cabinet", "Maximum Share of Women in Cabinet")
T_pred = T_pred %>% pivot_longer(cols = 1:3, names_to = "type",
                                 values_to = "T_pred")
ggplot(T_pred, aes(x = T_pred)) +
  geom_density(fill = "orangered3", alpha = 0.7) + 
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  scale_x_continuous(limits = c(0, 30000)) + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") + 
  labs(x = "Posterior Predicitive Distribution\nDuration of Cabinet") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text = element_text(size = 8))

ggsave(here("results", "fig3_weib_posteriorpredict.pdf"),
       width = 6, height = 4)

## Theoretical survplot
alpha = extract(fit.weib_vector, pars = c("alpha")) %>% unlist()
mu = extract(fit.weib_vector, pars = c("mu")) %>% unlist()
beta = extract(fit.weib_vector, pars = c("beta"))$beta
sigma = exp(- (as.numeric(mean(mu) + X_pred[1,2:14] %*% colMeans(beta[,2:14])) + as.matrix(X_pred[,1]) %*% t(as.matrix(beta[,1])))/ mean(alpha));
sigma = apply(sigma, 1, function(x) c(quantile(x, prob = 0.025), mean(x), quantile(x, prob = 0.975)))
surv.df = data.frame(t = t_grid,
                     type = rep(c("Minimal", "Mean", "Maximal"), each = length(t_grid)),
                     Survival = 1 - c(pweibull(t_grid, mean(alpha), sigma[2, 1]), pweibull(t_grid, mean(alpha), sigma[2, 2]), pweibull(t_grid, mean(alpha), sigma[2, 3])),
                     Survival_lower = 1 - c(pweibull(t_grid, mean(alpha), sigma[1, 1]), pweibull(t_grid, mean(alpha), sigma[1, 2]), pweibull(t_grid, mean(alpha), sigma[1, 3])),
                     Survival_upper = 1 - c(pweibull(t_grid, mean(alpha), sigma[3, 1]), pweibull(t_grid, mean(alpha), sigma[3, 2]), pweibull(t_grid, mean(alpha), sigma[3, 3])))

ggplot(surv.df, aes(x = t, y = Survival, ymin = Survival_lower, ymax = Survival_upper, 
                    color = type, fill = type)) + 
  geom_ribbon(alpha=0.4, color = NA) + 
  geom_line() +
  labs(x = "Duration", y = "Survival",
       color = "Share of Women in Cabinet",
       fill = "Share of Women in Cabinet") + 
  scale_color_manual(values = brewer.pal(3, "Dark2")) +
  scale_fill_manual(values = brewer.pal(3, "Dark2")) +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(title.position = 'top', title.hjust = 0.5))

ggsave(here("results", "fig4_weib_survplot.pdf"),
       width = 6, height = 4)



apply(as.data.frame(extract(fit.exp_vector, pars = "T_pred")), 2, function(x) c(mean(x), median(x)))
apply(as.data.frame(extract(fit.weib_vector, pars = "T_pred")), 2, function(x) c(mean(x), median(x)))
