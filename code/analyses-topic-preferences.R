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
# A function to run ordinal logistic regression for each level in a factor
iterate.olr <- function(response, predictor, data) {
  library("MASS")
  orig.levels <- levels(data[, predictor])
  values.matrix <- matrix(NA, length(orig.levels), length(orig.levels))
  rownames(values.matrix) <- colnames(values.matrix) <- orig.levels
  olr.results <- list("p.values" = values.matrix, "t.values" = values.matrix)
  
  for (i in 1:length(orig.levels)) {
    data[, predictor] <- relevel(x = data[, predictor], ref = orig.levels[i])
    olr <- polr(formula = data[, response] ~ data[, predictor], Hess = TRUE)
    olr.coeff <- coef(summary(olr))
    olr.p <- pnorm(abs(olr.coeff[, "t value"]), lower.tail = FALSE) * 2
    olr.coeff <- cbind(olr.coeff, "p.value" = olr.p)
    olr.results[["p.values"]][levels(data[, predictor])[-1], i] <- olr.coeff[1:length(x = orig.levels) - 1, "p.value"]
    olr.results[["t.values"]][levels(data[, predictor])[-1], i] <- olr.coeff[1:length(x = orig.levels) - 1, "t value"]
  }

  adj.p <- 0.05/((length(orig.levels) * (length(orig.levels) - 1)) / 2)
  olr.results$adj.p <- adj.p
  return(olr.results)  
}

################################################################################
# Test for differences among TOPICS

# Kruskal Wallis omnibus test for an affect of topic on preference
# see http://www.ats.ucla.edu/stat/r/whatstat/whatstat.htm#kw
kw.topic <- kruskal.test(x = topics.wide$preference, g = topics.wide$topic)
# Kruskal-Wallis rank sum test
# 
# data:  topics.wide$preference and topics.wide$topic
# Kruskal-Wallis chi-squared = 15.952, df = 11, p-value = 0.1429

########################################
# Ordinal logistic regression, full set of comparisons, using each topic as 
# reference

topic.olr <- iterate.olr(response = "preference", predictor = "topic", data = topics.wide)

# Print results only if at least one was significant
if (any(topic.olr$p.values < topic.olr$adj.p, na.rm = TRUE)) {
  print(topic.olr)
} else {
  print("No significant results in ordered logistic regression for TOPIC")
}

################################################################################
# Test for differences among POSITIONS

# Kruskal Wallis omnibus test for an affect of position on preference
kw.pos <- kruskal.test(x = topics.wide$preference, g = topics.wide$position)
# Kruskal-Wallis rank sum test
# 
# data:  topics.wide$preference and topics.wide$position
# Kruskal-Wallis chi-squared = 58.621, df = 2, p-value = 1.865e-13

########################################
# Ordinal logistic regression, full set of comparisons, using each position as 
# reference

pos.olr <- iterate.olr(response = "preference", predictor = "position", data = topics.wide)

# Print results only if at least one was significant
if (any(pos.olr$p.values < pos.olr$adj.p, na.rm = TRUE)) {
  print(pos.olr)
} else {
  print("No significant results in ordered logistic regression")
}

# Calculate mean scores & SE (s/(n^1/2))for each position
positions <- levels(topics.wide$position)
pos.info <- data.frame("position" = positions, "mean" = NA, "se" = NA)
for (p in 1:length(positions)) {
  pos.prefs <- as.numeric(topics.wide$preference[topics.wide$position == positions[p]])
  pos.prefs <- na.omit(pos.prefs)
  pos.info$mean[p] <- mean(pos.prefs)
  pos.info$se[p] <- sd(pos.prefs) / sqrt(length(pos.prefs))
}

# position     mean         se
# 1  Faculty 2.616162 0.07799989
# 2    Staff 3.672222 0.09514292
# 3  Student 2.996032 0.09214004
