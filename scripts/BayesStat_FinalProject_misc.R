## misc / stuff that got cut

### ### ### ###
### EXPO   ####
### ### ### ### 
# Survival Function
 plot(fit.exp_vector, pars = "T_pred")
 T_pred = as.data.frame(extract(fit.exp_vector, pars = "T_pred"))
 
 ecdf1 = ecdf(T_pred$T_pred.1)
 ecdf2 = ecdf(T_pred$T_pred.2)
 ecdf3 = ecdf(T_pred$T_pred.3)
 t_grid = 0:2000
 surv.df = data.frame(t = t_grid,
                      Minimal = ecdf1(t_grid),
                      Mean = ecdf2(t_grid),
                      Maximal = ecdf3(t_grid))
 surv.df = surv.df %>% pivot_longer(cols = 2:4,
                                    names_to = "type",
                                    values_to = "cdf") %>%
   rowwise() %>%
   mutate(surv = 1 - cdf)
 ggplot(surv.df, aes(x = t, y = surv, color = type)) + 
   geom_line() +
   labs(x = "Duration", y = "Survival",
        color = "Share of Women in Cabinet") + 
   scale_color_manual(values = brewer.pal(3, "Dark2")) +
   theme_minimal() + 
   theme(legend.position = "bottom") + 
   guides(color = guide_legend(title.position = 'top', title.hjust = 0.5))

### ### ### ###
### WEIBULL####
### ### ### ### 
# Survival function from ecdf of T_pred
traceplot(fit.weib_vector)
plot(fit.weib_vector, pars = "T_pred")
T_pred = as.data.frame(extract(fit.weib_vector, pars = "T_pred"))

ecdf1 = ecdf(T_pred$T_pred.1)
ecdf2 = ecdf(T_pred$T_pred.2)
ecdf3 = ecdf(T_pred$T_pred.3)
t_grid = 0:2000
surv.df = data.frame(t = t_grid,
                     min = ecdf1(t_grid),
                     mean = ecdf2(t_grid),
                     max = ecdf3(t_grid))
surv.df = surv.df %>% pivot_longer(cols = 2:4,
                         names_to = "type",
                         values_to = "cdf") %>%
  rowwise() %>%
  mutate(surv = 1 - cdf)
ggplot(surv.df, aes(x = t, y = surv, color = type)) + 
  geom_line()

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
Z = model.matrix(Y ~ share_women_executive, data = KK20)
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
               cores = 4,
               #               control = list(max_treedepth = 15,
               #                             adapt_delta = 0.99),
               iter = 2000)
fit.ZIP
# Why not working properly when adding maxdur? 
# even without, some Rhats not that great, especially for ZI part
traceplot(fit.ZIP, pars = "beta")
traceplot(fit.ZIP, pars = "gamma")
fit.zinf = zeroinfl(Y ~ share_women_executive + pref + enpp + 
                      decade + ciep + minority + maxdur, data = KK20)
summary(fit.zinf)
-2.249 * 10^4
colnames(Z)
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

traceplot(fit.ZIP, pars = "gamma")
ggplot(coef.samples, aes(x = value, y = parameter)) + 
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  stat_density_ridges(quantile_fun = mean, quantile_lines = T,alpha = 0.5, fill = "orangered3") + 
  #scale_x_continuous(limits = c(-20, 15)) + 
  labs(x = "Coefficient") + 
  facet_wrap(~part + parameter, scales = "free") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank())

