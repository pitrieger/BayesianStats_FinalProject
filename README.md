# BayesianStats_FinalProject

This repo includes the replication files of the final project for the course Applied Bayesian Statistics. In the paper, I replicate the analysis of a paper by [Krauss & Kroeber (2020)](https://www.tandfonline.com/doi/full/10.1080/13501763.2020.1773905) who show that government stability is higher in cabinets with a higher share of female ministers. I transfer their survival model to parametric survival models. More specifically, I implement exponential and Weibull survival models using `rstan`. The report of the analysis is included in the folder "writing/".

The full analysis can be replicated in the `R`-script "scripts/BayesStat_FinalProject_main.R". Stan code is contained in the folder "stanmodels/" which also includes additional models, some of which can be tested via the scripts in the folder "scriptstest/". The latter folder also contains code that was used to verify that the used models work as intended by testing the models with simulated data. However, the scripts in this folder are mostly working scripts and have mostly not been cleaned, updated or commented.

The code for the survival models was partly inspired by 
* [Eren M. Elçi's blog post](https://ermeel86.github.io/case_studies/surv_stan_example.html)
* [Jacki Buros Novik's package](https://github.com/jburos/biostan)
