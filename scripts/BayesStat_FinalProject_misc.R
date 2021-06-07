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



