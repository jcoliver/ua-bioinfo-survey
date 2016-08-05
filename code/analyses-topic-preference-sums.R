# Sum of preference scores (Likert scale)
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-06-01

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
topic.columns <- grep(pattern = "topic.", x = colnames(results))
results$topic.sum <- apply(X = results[, topic.columns], MARGIN = 1, FUN = sum)
hist(results$topic.sum, xlab = "Sum of topic preference scores", main = "")
# Tri-modal