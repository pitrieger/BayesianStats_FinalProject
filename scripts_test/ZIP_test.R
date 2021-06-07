# Proof of concept stan ZIP regression
library(here)
library(tidyverse)
library(rstan)
library(pscl)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
logit_inv = function(x) 1 / (1+exp(-x))

# data
n = 1000
p = 1
X = cbind(1, matrix(rnorm(n*p), ncol = p))
Z = cbind(1, matrix(rnorm(n*p), ncol = p))
beta = c(4, 0.5)
nu = as.numeric(exp(X%*%beta))
gamma = c(1.5, -0.9)
pi = as.numeric(logit_inv(Z%*%gamma))
zeros = sapply(pi, function(p) rbinom(1,1,p))
Y = zeros*rpois(n, nu)
hist(Y)
df = data.frame(x = X[,2],
                z = Z[,2],
                y = Y)


## Maximum likelihood
fit.zinf = zeroinfl(y ~ x | z, data = df)
summary(fit.zinf)

## Vectorized Stan
zip.standat = list(n = n, p = 2, m = 2, 
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


