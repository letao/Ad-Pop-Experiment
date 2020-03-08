############################
####Bayesian models 1 and 2



##testing the bayesian models

##data for the participant (id) model
prod_count_id <- prod_tsk %>% 
  count(argument, strength, id)

##data for the item model
prod_count_item <- prod_tsk %>% 
  count(argument, strength, item)


##Prior distribution for the participant model
curve(dnorm(x, 2, 1), from = -5, to = 5) 

curve(dnorm(x, -1, 1), from = -5, to = 5) 

##null model for participants
bm2n <- brm(n ~ 1 + (1|id), 
            family = negbinomial(link = log),
            prior = c(set_prior("normal(2,1)", class = "Intercept")),
            sample_prior = TRUE,
            save_all_pars = TRUE,
            iter = 4000,
            cores = 2,
            data = prod_count_id)

bm2n_l <- loo(bm2n)


## intercept model for participants
bm2 <- brm(n ~ 0 + intercept + argument + (1|id), 
           family = negbinomial(link = log),
           prior = c(set_prior("normal(2,1)", class = "b", coef = "intercept"), 
                     set_prior("normal(-1,1)", class = "b", coef = "argumentEveryone"),
                     set_prior("normal(-1,1)", class = "b", coef = "argumentSome")),
           sample_prior = TRUE,
           save_all_pars = TRUE,
           iter = 4000,
           cores = 2,
           data = prod_count_id)

bm2_l <- loo(bm2)

##comparison of null model and bm2 
loo_compare(bm2n_l,bm2_l)

model_weights(bm2, bm2.2, weights = "loo2")


##slope without correlated intercpet model for participants
bm2.2 <- brm(n ~ 0 + intercept + argument + (argument||id), 
             family = negbinomial(link = log),
             prior = c(set_prior("normal(2,1)", class = "b", coef = "intercept"), 
                       set_prior("normal(-1,1)", class = "b", coef = "argumentEveryone"),
                       set_prior("normal(-1,1)", class = "b", coef = "argumentSome")),
             sample_prior = TRUE,
             save_all_pars = TRUE,
             cores = 2,
             data = prod_count_id)

bm2.2_l <- loo(bm2.2)

##comparison of model bm2, bm2.2 and bm2n
loo_compare(bm2_l, bm2.2_l, bm2n_l)

model_weights(bm2n, bm2.2, bm2n, weights = "loo2")


##plot of the model
cc <- plot(marginal_effects(bm2, plot = F)) [[1]] + theme_bw() +
  xlab("Quantifiers") + ylab("Average count of replies") + 
  ggtitle("Participant model")


##illustration of the piror distribution for the item model N(3,1)
curve(dlnorm(x, 3, 1), from = 0, to = 40) 



##null model for items
bm3n <- brm(n ~ 1 + (1|item), 
            family = negbinomial(link = log),
            prior = c(set_prior("normal(3,1)", class = "Intercept")),
            sample_prior = TRUE,
            save_all_pars = TRUE,
            iter = 4000,
            cores = 2,
            data = prod_count_item)

bm3n_l <- loo(bm3n)

##intercept model for items
bm3 <- brm(n ~ 0 + intercept + argument + (1|item), 
           family = negbinomial(link = log),
           prior = c(set_prior("normal(3,1)", class = "b", coef = "intercept"), 
                     set_prior("normal(-1,1)", class = "b", coef = "argumentEveryone"),
                     set_prior("normal(-1,1)", class = "b", coef = "argumentSome")),
           sample_prior = TRUE,
           save_all_pars = TRUE,
           iter = 4000,
           cores = 2,
           data = prod_count_item)

bm3_l <- loo(bm3)

##model comparison between null and participant model
loo_compare(bm3n_l, bm3_l)

model_weights(bm3n, bm3, weights = "loo2")


##intercept and uncorrelated slope model for items
bm3.2 <- brm(n ~ 0 + intercept + argument + (argument||item), 
             family = poisson(link = log),
             prior = c(set_prior("normal(3,1)", class = "b", coef = "intercept"), 
                       set_prior("normal(-1,1)", class = "b", coef = "argumentEveryone"),
                       set_prior("normal(-1,1)", class = "b", coef = "argumentSome")),
             sample_prior = TRUE,
             save_all_pars = TRUE,
             iter = 4000,
             cores = 2,
             data = prod_count_item)

bm3.2_l <- loo(bm3.2)

##Comparison between bm3, bm3.2 and bm3.n models
loo_compare(bm3_l, bm3.2_l, bm3n_l)

model_weights(bm3, bm3.2, bm3n, weights = "loo2")


##plot of model b3
cc2 <- plot(marginal_effects(bm3, plot = F)) [[1]] + theme_bw() +
  xlab("Quantifiers") + ylab("Average count of replies") +
  ggtitle("Item model")

##plot of both items and participant models
gridExtra::grid.arrange(cc,cc2, nrow = 1)

