library(here)
library(readstata13)
library(tidyverse)
library(rstan)
library(pscl)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


## Data ====
KK20 = read.dta13(here('data/Krauss2020_WomenCabinet.dta'))
KK20$decade = as.factor(KK20$decade)
KK20$ciep = as.factor(KK20$ciep)

KK20$Y = (KK20$maxdur - KK20$duration)
KK20$Y[KK20$failure == 0] = 0

fit.pscl = zeroinfl(Y ~ share_women_executive + pref + enpp + 
                      decade + ciep + minority, 
                    data = KK20, dist = "poisson", link = "logit")
summary(fit.pscl)

X = model.matrix(Y ~ share_women_executive + pref + enpp + 
                   decade + ciep + minority, data = KK20)
Z = X
Y = KK20$Y

## Vectorized Stan
zip.standat = list(n = nrow(X), p = ncol(X), m = ncol(Z), 
                   y_nonzero = Y[Y!=0],
                   n_nonzero = sum(Y!=0),
                   n_zero = sum(Y == 0),
                   X_nonzero = X[Y!=0,],
                   X_zero = X[Y==0,],
                   Z_nonzero = Z[Y!=0,],
                   Z_zero = Z[Y==0,])

fit.stan = stan(here("stanmodels", "ZIP2.stan"), data = zip.standat,
                pars = c("beta", "gamma"))
fit.stan
pairs(fit.stan)

coef(fit.pscl)
