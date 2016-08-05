# Test for significant preference difference among training topics
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-06-01

rm(list = ls())
load(file = "output/results-processed.RData")
topics <- results[,c(4:15)]
library("tidyr")
topics.long <- topics %>% gather(key = topic, value = preference, c(1:ncol(topics)))
topics.long$topic <- factor(x = topics.long$topic)
topics.long$preference <- ordered(x = topics.long$preference, levels = c(1, 2, 3, 4, 5))

################################################################################
# Kruskal Wallis omnibus test for an affect of topic on preference
# see http://www.ats.ucla.edu/stat/r/whatstat/whatstat.htm#kw
kw <- kruskal.test(x = topics.long$preference, g = topics.long$topic)
# Kruskal-Wallis rank sum test
# 
# data:  topics.long$preference and topics.long$topic
# Kruskal-Wallis chi-squared = 15.952, df = 11, p-value = 0.1429

################################################################################
# Ordinal logistic regression, full set of comparisons, using each topic as 
# reference
topic.levels <- levels(topics.long$topic)
p.matrix <- matrix(NA, length(topic.levels), length(topic.levels))
rownames(p.matrix) <- colnames(p.matrix) <- topic.levels
for (i in 1:length(topic.levels)) {
  topics.long$topic <- relevel(x = topics.long$topic, ref = topic.levels[i])
  olr <- polr(formula = preference ~ topic, data = topics.long, Hess = TRUE)
  olr.coeff <- coef(summary(olr))
  olr.p <- pnorm(abs(olr.coeff[, "t value"]), lower.tail = FALSE) * 2
  olr.coeff <- cbind(olr.coeff, "p value" = round(olr.p, 6))
  p.matrix[levels(topics.long$topic)[-1], i] <- olr.coeff[1:(length(topic.levels) - 1), 4]
}
# Bonferroni correction would be a/m (a = 0.05, m is number of comparisons, or 
# n(n-1)/2)
adj.p <- 0.05/((length(topic.levels) * (length(topic.levels) - 1)) / 2)
# 0.00076