# Distribution of preferences for scripting training
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-04-18

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
# install.packages("plyr")
library("plyr")

# Find out counts for each of the five levels
topics <- results[, c(4:15)]
counts <- data.frame(row.names = c("Not interested", "2", "3", "4", "Very interested"))
for (curr.topic in colnames(topics)) {
  topic.counts <- count(df = topics, vars = curr.topic)
  counts[, curr.topic] <- topic.counts$freq
}

# How many answered either "not interested" or "very interested"
not.plus.very <- counts[1, ] + counts[5, ]
rownames(not.plus.very)[1] <- "Not or Very"
# Highest: scripting with 43

# How many answered in the middle three levels
intermediates <- counts[2, ] + counts[3, ] + counts[4, ]
rownames(intermediates)[1] <- "2, 3, or 4"
# Lowest: scripting with 29