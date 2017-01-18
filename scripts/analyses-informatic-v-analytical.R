# Test for differences in interest between analyses and information topics
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2017-01-18

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

# Apply category of analytical or informatic
analytical <- c("transcriptome", "pipelines", "alignment", "phylo", "scripting")
topics.wide$topic.type <- "informatic"
topics.wide$topic.type[topics.wide$topic %in% analytical] <- "analytical"
topics.wide$topic.type <- factor(topics.wide$topic.type)

topics.wide$preference <- ordered(x = topics.wide$preference, levels = c(1, 2, 3, 4, 5))

################################################################################
# Test for differences among TOPIC TYPES

# Kruskal Wallis omnibus test for an affect of topic on preference
kw.topic.type <- kruskal.test(x = topics.wide$preference, g = topics.wide$topic.type)
# Kruskal-Wallis rank sum test
# 
# data:  topics.wide$preference and topics.wide$topic.type
# Kruskal-Wallis chi-squared = 0.89028, df = 1, p-value = 0.3454
