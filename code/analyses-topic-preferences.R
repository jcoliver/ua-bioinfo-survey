# Test for significant preference difference among training topics
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-06-01

rm(list = ls())
load(file = "output/results-processed.RData")

# Pull out training topic columns
topic.columns <- grep(pattern = "topic.", x = colnames(results))
topics <- results[, topic.columns]

# Remove "topic." prefix from column names
colnames(topics) <- gsub(pattern = "topic.", replacement = "", x = colnames(topics))

# install.packages("tidyr")
library("tidyr")
topics.wide <- topics %>% gather(key = topic, 
                                 value = preference, 
                                 c(1:ncol(topics)))
topics.wide$topic <- factor(x = topics.wide$topic)
topics.wide$preference <- ordered(x = topics.wide$preference, levels = c(1, 2, 3, 4, 5))

################################################################################
# Kruskal Wallis omnibus test for an affect of topic on preference
# see http://www.ats.ucla.edu/stat/r/whatstat/whatstat.htm#kw
kw <- kruskal.test(x = topics.wide$preference, g = topics.wide$topic)
# Kruskal-Wallis rank sum test
# 
# data:  topics.wide$preference and topics.wide$topic
# Kruskal-Wallis chi-squared = 15.952, df = 11, p-value = 0.1429

################################################################################
# Ordinal logistic regression, full set of comparisons, using each topic as 
# reference
library("MASS")
topic.levels <- levels(topics.wide$topic)
p.matrix <- matrix(NA, length(topic.levels), length(topic.levels))
rownames(p.matrix) <- colnames(p.matrix) <- topic.levels
for (i in 1:length(topic.levels)) {
  topics.wide$topic <- relevel(x = topics.wide$topic, ref = topic.levels[i])
  olr <- polr(formula = preference ~ topic, data = topics.wide, Hess = TRUE)
  olr.coeff <- coef(summary(olr))
  olr.p <- pnorm(abs(olr.coeff[, "t value"]), lower.tail = FALSE) * 2
  olr.coeff <- cbind(olr.coeff, "p value" = round(olr.p, 6))
  p.matrix[levels(topics.wide$topic)[-1], i] <- olr.coeff[1:(length(topic.levels) - 1), 4]
}
# Bonferroni correction would be a/m (a = 0.05, m is number of comparisons, or 
# n(n-1)/2)
adj.p <- 0.05/((length(topic.levels) * (length(topic.levels) - 1)) / 2)
# 0.00076