# Test for significant preference difference among training topics for each position separately
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2017-01-13

rm(list = ls())
load(file = "output/results-processed.RData")
source(file = "functions/iterate.olr.R")

################################################################################
# SETUP
# Prepare data for tests; ultimately want a data frame with three columns:
# topic name, preference score, and position of respondent

# Pull out training topic columns
topic.columns <- grep(pattern = "topic.", x = colnames(results))
topics <- results[, topic.columns]

# Add position vector
topics$position <- results$position

# Remove "topic." prefix from column names
colnames(topics) <- gsub(pattern = "topic.", replacement = "", x = colnames(topics))

# install.packages("tidyr")
library("tidyr")
topics.wide <- topics %>% gather(key = topic, 
                                 value = preference, 
                                 c(1:ncol(topics)),
                                 -position)
topics.wide$topic <- factor(x = topics.wide$topic)
topics.wide$preference <- ordered(x = topics.wide$preference, levels = c(1, 2, 3, 4, 5))

################################################################################
# Iterate over all levels of position and run (1) KW test and if significant, 
# (2) ordinal logistic regression over all topics

kw.crit.val <- 0.05 # determines whether or not to run olr...need to Bonferroni this?

for (p in 1:length(levels(topics.wide$position))) {
  # Subset data
  current.level <- levels(topics.wide$position)[p]
  pos.data <- topics.wide[topics.wide$position == current.level, ]
  # KW Test
  kw.topic <- kruskal.test(x = pos.data$preference, g = pos.data$topic)

  # Need to check p-value... should just be kw.topic@p.value
  if (kw.topic$p.value < kw.crit.val) {
    message(paste0("Running ordinal logistic regression on ", 
                   current.level, 
                   " (p = ", 
                   round(x = kw.topic$p.value, digits = 4), 
                   ")"))
    # olr
    topic.olr <- iterate.olr(response = "preference", predictor = "topic", data = pos.data)
    
    # Print results only if at least one was significant
    if (any(topic.olr$p.values < topic.olr$adj.p, na.rm = TRUE)) {
      print(topic.olr)
    } else {
      message("No significant results in ordered logistic regression for TOPIC")
    }
    
  } else {
    message(paste0("KW test not significant for ", 
                   current.level, 
                   "(p = ", 
                   round(x = kw.topic$p.value, digits = 4), 
                   ")"))
  }
}

# KW test not significant for Faculty(p = 0.7617)
# KW test not significant for Staff(p = 0.3131)
# KW test not significant for Student(p = 0.3155)