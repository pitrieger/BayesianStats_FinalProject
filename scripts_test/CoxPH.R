library(survival)
library(survminer)
data("lung")
head(lung)
plot(lung)

?gamlss
library(gamlss)


X = rweibull(10, 100)

n = 1000
X = rnorm(n)
Y = rWEI(n, exp(2 + 0.5 * X), sigma = 2)
hist(Y)
plot(Y ~ X)
fit = gamlss(Y ~ X, family = WEI())
summary(fit)
plot(resid(fit))
plot(fit)
summary(fem2$pref)

library(gamlss)
fit = gamlss(duration ~ share_women_executive + maxdur + ciep + pref + minority,
             ~ maxdur,
             family = WEI(), data = na.omit(fem2[,1:24]))
summary(fit)
plot(fit)
fitted(fit)
plot(fitted(fit) ~ na.omit(fem2[,1:24])$duration)

library(survival)

fit.cph = coxph(Surv(duration, failure) ~ share_women_executive + maxdur + ciep + pref + minority, data = fem2)
su
coxph()
cox.zph(fit.cph)
plot(cox.zph(fit.cph))

fit <- coxph(Surv(futime, fustat) ~ age + ecog.ps,  
             data=ovarian) 
temp <- cox.zph(fit) 
print(temp)                  # display the results 
plot(temp) 
