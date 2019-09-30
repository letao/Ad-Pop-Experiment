######################
####Bayesian model3


##prior distribution for model 3. In this case we have a categorical model, so we choose a prior distribution on a gaussian prob. distribution scale 
curve(dnorm(x, 0, 1), from = -3, to = 3)

##null model
bm4.n <- brm(argument ~ 1 + (1|id) + (1|item), 
             family = categorical(link = logit),
             prior = c(set_prior("normal(0,1)", class = "Intercept")),
             sample_prior = TRUE,
             save_all_pars = TRUE,
             iter = 4000,
             cores = 2,
             data = prod_tsk)

bm4n_l <- loo(bm4.n)


##Intercept model, with specified seperate intercepts ( 0 + Intercept) for each category of agreement strength 
bm4 <- brm(argument ~ 0 + Intercept + strength + (1|id) + (1|item), 
           family = categorical(link = logit),
           prior = c(set_prior("normal(0,1)", class = "b")),
           sample_prior = TRUE,
           save_all_pars = TRUE,
           cores = 2,
           iter = 4000,
           data = prod_tsk)

bm4_l <- loo(bm4)

##model comparison between the null model and the intercept model (bm4)
loo_compare(bm4n_l, bm4_l)

model_weights(bm4, bm4.2, weights = "loo2")



##BayesFactor tests for Some and Everyone variables inside the model
hypothesis(bm4x, "exp(muSome_strengthsomewhat) = exp(muSome_Intercept)")

hypothesis(bm4x, "exp(muEveryone_strengthsomewhat) = exp(muEveryone_Intercept)")


##Intercept and uncorrelated slipe model 
bm4.2 <- brm(argument ~ 0 + Intercept + strength + (strength||id) + (strength||item), 
             family = categorical(link = logit),
             prior = c(set_prior("normal(0,1)", class = "b")),
             sample_prior = TRUE,
             save_all_pars = TRUE,
             cores = 2,
             iter = 4000,
             data = prod_tsk)

bm4.2_l <- loo(bm4.2)

##model comparison between bm4 and bm4.2 and bm4.n
loo_compare(bm4_l, bm4.2_l, bm4n_l)

model_weights(bm4, bm4.2, bm4.n, weights = "loo2")

##Plot of the intercept model
plot(marginal_effects(bm4x, probs = c(.05,.95), plot = F, categorical = TRUE)) [[1]] + 
  theme_bw() +
  xlab("Strength of agreement") + ylab("probability of replies") + 
  labs(colour = "Quantifier", fill = "Quantifier")










