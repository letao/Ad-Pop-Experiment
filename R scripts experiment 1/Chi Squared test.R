####################
####Chi Squared test



##table for the chi square analysis
M <- as.table(cbind(c(1669, 897, 330)))


dimnames(M) <- list(quantifier = c("No", "Some", "Everyone"),
                    responses = c("count"))



##chi square test and summary
chi.counts <- chisq.test(M)
chi.counts


chi.counts$observed   # observed counts (same as M)
chi.counts$expected   # expected counts under the null
chi.counts$residuals  # Pearson residuals
chi.counts$stdres     # standardized residuals

