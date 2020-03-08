#################
####Data clean-up and descriptives


##read the csv file
my.data <- read_csv(file.choose())

##Change the factor levels to set dummy coding with X as base level
anchoring <- my.data %>% mutate(quantifier = factor(quantifier, 
                                                    levels = c("No", "Some", "Everyone")),
                                quantifier = recode(quantifier,
                                                    "No" = "X")
                               )

##function to calculate confidence intervals with a gaussian normal distribution
ci <- function(x,y) {
  qnorm(0.975)*sd(x)/sqrt(length(unique(y)))
}

##summary of data
anchoring_sum <- anchoring %>%
  
  ##grouping element by quantifier
  group_by(
    quantifier
  ) %>%
  
  ##calculating mean sd and ci for each quantifier
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    ci = ci(value, id)
  )

##plot of means from summary
anchoring_plot <- ggplot(anchoring_sum, aes(x=quantifier, y=mean)) +
  geom_bar(stat = "identity", aes(fill = quantifier)) +
  coord_cartesian(ylim = c(30,90)) + scale_y_continuous(breaks=seq(30, 90, by = 10)) +
  geom_errorbar(aes(ymin= mean - ci, ymax = mean + ci), colour = "black", width = 0.2) +
  labs(fill = "Quantifier") +
  ylab("Quantifier") +
  ggtitle("Average values per Quantifier") +
  theme_bw() +
  theme(legend.position = "none")

##plot of histograms faceted by quantifier levels
h_plot <- ggplot(anchoring) +
  geom_histogram(aes(x=value, fill = quantifier), bins = 8) +
  geom_vline(xintercept = 50, size = .5, colour = "black",
             linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  labs(fill = "Quantifier") +
  ggtitle("Histogram of replies per Quantifier") +
  theme_bw() +
  theme(legend.position = "none")+
  facet_wrap(~quantifier)

##combination of both plots
gridExtra::grid.arrange(anchoring_plot,h_plot, nrow = 2)