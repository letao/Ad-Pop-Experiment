#################
####Data clean-up and descriptive

##select data
prod <- read.csv("Arg_Production_task2.csv")

##investigate the data
str(prod)


prod_data <- prod %>% 
  
  ##reorder the levels of factors for analysis
  mutate(
    argument = factor(argument, 
                      levels = c("3", "2", "1")),
    statement = factor(statement,
                       levels = c("1", "2", "3", "4"))
  ) %>%
  
  ##rename the variables
  mutate(
    argument = recode(argument,
                      "3" = "No", 
                      "2" = "Some", 
                      "1" = "Everyone"),
    statement = recode(statement,
                       "1" = "Strong.Agree", 
                       "2" = "Somewhat.Agree", 
                       "3" = "Somewhat.Disagree", 
                       "4" = "Strong.Disagree")
  )


##leave only the complete cases
prod_data <- prod_data[complete.cases(prod_data),]


prod_tsk <- prod_data %>%
  
  ##subset the data to only include the critical items, no fillers
  subset(
    item.type == "critical"
  ) %<>% 
  
  ##converting variables to factors
  mutate_at(
    c("item.type","strength", "agreement"), funs(factor(.))
  )

str(prod_tsk)



##create the counts for each argument choosen 
prod_count <- prod_tsk %>%
  
  ##count data
  count(argument
  ) %>%
  
  ##percentage data using the n which is taken from count argument
  mutate(
    pctg = ((n/sum(n))*100)
  )

##plot of counts
ggplot(prod_count, aes(x = argument, y = n)) + 
  geom_bar(stat = "identity", aes(fill = argument))  +  
  coord_cartesian(ylim = c(0, 1900)) +
  ylab("Count of total replies") +
  xlab("Quantifier chosen") +
  ggtitle("Count of arguments chosen") +
  theme_bw()

##plot of percentages
ggplot(prod_count, aes(x = argument, y = pctg)) + 
  geom_bar(stat = "identity", aes(fill = argument))  +  
  coord_cartesian(ylim = c(0, 100)) +
  ylab("% of total replies") +
  xlab("Quantifier") +
  labs(fill = "Quantifier") +
  theme_bw()


##data and plots for arguments devided by strength
prod_count_2 <- prod_tsk %>%
  
  ##calculate counts
  count(argument,strength
  ) %>% 
  
  ##calcualte percentages 
  mutate(pctg = ((n/sum(n))*100)
  )


ggplot(prod_count_2, aes(x = argument, y = pctg)) + 
  geom_bar(stat = "identity", aes(fill = argument))  +  
  coord_cartesian(ylim = c(0, 40)) +
  ylab("% of total replies") +
  xlab("Quantifier") +
  facet_wrap(~strength) +
  labs(fill = "Quantifier") +
  theme_bw()


##data and plots for arguments devided by agreement
prod_count_3 <- prod_tsk %>% 
  
  ##calculate counts
  count(argument,agreement
  ) %>% 
  
  ##calculate percentages
  mutate(pctg = ((n/sum(n))*100)
  )


ggplot(prod_count_3, aes(x = argument, y = pctg)) + 
  geom_bar(stat = "identity", aes(fill = argument))  +  
  coord_cartesian(ylim = c(0, 50)) +
  ylab("% of total replies") +
  xlab("Quantifier") +
  facet_wrap(~agreement) + 
  theme_bw()

