# Test for significant preference difference among training topics
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-06-01

rm(list = ls())
load(file = "output/results-processed.RData")

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
# Test for differences among TOPICS

# Kruskal Wallis omnibus test for an affect of topic on preference
# see http://www.ats.ucla.edu/stat/r/whatstat/whatstat.htm#kw
kw.topic <- kruskal.test(x = topics.wide$preference, g = topics.wide$topic)
# Kruskal-Wallis rank sum test
# 
# data:  topics.wide$preference and topics.wide$topic
# Kruskal-Wallis chi-squared = 15.952, df = 11, p-value = 0.1429

################################################################################
# Ordinal logistic regression, full set of comparisons, using each topic as 
# reference
library("MASS")
topic.levels <- levels(topics.wide$topic)
topic.values.matrix <- matrix(NA, length(x = topic.levels), length(x = topic.levels))
rownames(topic.values.matrix) <- colnames(topic.values.matrix) <- topic.levels
topic.olr.results <- list("p.values" = topic.values.matrix, "t.values" = topic.values.matrix)

for (i in 1:length(x = topic.levels)) {
  topics.wide$topic <- relevel(x = topics.wide$topic, ref = topic.levels[i])
  olr <- polr(formula = preference ~ topic, data = topics.wide, Hess = TRUE)
  olr.coeff <- coef(summary(olr))
  olr.p <- pnorm(abs(olr.coeff[, "t value"]), lower.tail = FALSE) * 2
  olr.coeff <- cbind(olr.coeff, "p.value" = olr.p)
  topic.olr.results[["p.values"]][levels(topics.wide$topic)[-1], i] <- olr.coeff[1:(length(x = topic.levels) - 1), "p.value"]
  topic.olr.results[["t.values"]][levels(topics.wide$topic)[-1], i] <- olr.coeff[1:(length(x = topic.levels) - 1), "t value"]
}
# Bonferroni correction would be a/m (a = 0.05, m is number of comparisons, or 
# n(n-1)/2)
topic.adj.p <- 0.05/((length(x = topic.levels) * (length(x = topic.levels) - 1)) / 2)
# 0.00076

# Print p-values matrix only if there are any significant values
if (any(topic.olr.results[["p.values"]] < topic.adj.p, na.rm = TRUE)) {
  print(topic.olr.results)
}

################################################################################
# Test for differences among POSITIONS

# Kruskal Wallis omnibus test for an affect of position on preference
kw.pos <- kruskal.test(x = topics.wide$preference, g = topics.wide$position)
# Kruskal-Wallis rank sum test
# 
# data:  topics.wide$preference and topics.wide$position
# Kruskal-Wallis chi-squared = 58.621, df = 2, p-value = 1.865e-13

################################################################################
# Ordinal logistic regression, full set of comparisons, using each topic as 
# reference
pos.levels <- levels(topics.wide$position)
pos.values.matrix <- matrix(NA, length(x = pos.levels), length(x = pos.levels))
rownames(pos.values.matrix) <- colnames(pos.values.matrix) <- pos.levels
pos.olr.results <- list("p.values" = pos.values.matrix, "t.values" = pos.values.matrix)

for (i in 1:length(x = pos.levels)) {
  topics.wide$position <- relevel(x = topics.wide$position, ref = pos.levels[i])
  olr <- polr(formula = preference ~ position, data = topics.wide, Hess = TRUE)
  olr.coeff <- coef(summary(olr))
  olr.p <- pnorm(abs(olr.coeff[, "t value"]), lower.tail = FALSE) * 2
  olr.coeff <- cbind(olr.coeff, "p.value" = olr.p)
  pos.olr.results[["p.values"]][levels(topics.wide$position)[-1], i] <- olr.coeff[1:(length(x = pos.levels) - 1), "p.value"]
  pos.olr.results[["t.values"]][levels(topics.wide$position)[-1], i] <- olr.coeff[1:(length(x = pos.levels) - 1), "t value"]
}

pos.adj.p <- 0.05/((length(x = pos.levels) * (length(x = pos.levels) - 1)) / 2)
# 0.0167

# Print p and t matrices only if there is at least one significant result
if (any(pos.olr.results[["p.values"]] < pos.adj.p, na.rm = TRUE)) {
  print(pos.olr.results)
}
