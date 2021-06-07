## misc / stuff that got cut

### ### ### ###
### EXPO   ####
### ### ### ### 

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



