# Sum of preference scores (Likert scale)
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-06-01

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
results$topic.sum <- apply(X = results[, c(4:15)], MARGIN = 1, FUN = sum)
hist(results$topic.sum, xlab = "Sum of topic preference scores", main = "")
# Tri-modal