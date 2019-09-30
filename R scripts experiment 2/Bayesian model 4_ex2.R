#################
####Bayesian models

##Zero_one_inflated_beta family models with log odds output

##to interpret the coeficients from the log odds use this forumla, it converts log odds back to the value on the real value of RV
1/(1+exp(-alpha))


##defining priors
prior_z <- c(set_prior("normal(0,.5)", class = "b"),
             set_prior("normal(0,1)", class = "sd"))

##illustration of the priors selected
curve(dnorm(x, 0,  .5) , from=-5, to=5)
curve(dnorm(x, 0,  1) , from=-5, to=5)


##fit the model with zero one inflated beta family, defined priors and values devided by 100 to fit between 0 and 1 (required for beta family)

##null model
fit2n <- brm((value/100) ~ 1 + (1|id) + (1|item), 
             family = "zero_one_inflated_beta",
             sample_prior = TRUE,
             save_all_pars = TRUE,
             prior = set_prior("normal(0,.5)", class = "Intercept"),
             cores = 2,
             data = anchoring)

fit2n_l <- loo(fit2n)


##intercept model
fit2z <- brm((value/100) ~ 0 + Intercept + quantifier + (1|id) + (1|item), 
             family = "zero_one_inflated_beta",
             sample_prior = TRUE,
             save_all_pars = TRUE,
             prior = prior_z,
             cores = 2,
             data = anchoring)

fit2z_l <- loo(fit2z)

##posterior probability checks
pp_check(fit2z)


##simpel loo comparions of the two models
loo_compare(fit2n_l, fit2z_l)


##Intercept and uncorrelated slope model
fit2y <- brm((value/100) ~ 0 + Intercept + quantifier + (quantifier||id) + (quantifier||item), 
             family = "zero_one_inflated_beta",
             sample_prior = TRUE,
             save_all_pars = TRUE,
             prior = prior_z,
             iter = 4000,
             cores = 2,
             data = anchoring)

fit2y_l<- loo(fit2y)

##posterior probability checks
pp_check(fit2y)


##simple loo comparions of the three models
loo_compare(fit2y, fit2z_l, fit2n_l)

model_weights(fit2y, fit2z, fit2n, weights = "loo2")


##BayesFactor test between quantifierEveryone and quantifierNo 
hypothesis(fit2y, "1/(1+exp(-(Intercept + quantifierEveryone))) = 1/(1+exp(-(Intercept)))")

##BayesFactor test between quantifierSome and quantifierNo 
hypothesis(fit2y, "1/(1+exp(-(Intercept + quantifierSome))) = 1/(1+exp(-(Intercept)))")

##marginal effects plot for the model
plot(marginal_effects(fit2z, probs = c(.05,.95), plot = F)) [[1]] + theme_bw() +
  xlab("Quantifiers") + ylab("Assigned probability") 

