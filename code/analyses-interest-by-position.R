# Test for significant difference in interest by position
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2017-01-11

rm(list = ls())
load(file = "output/results-processed.RData")

# Pull out training topic columns
position.columns <- grep(pattern = "topic.", x = colnames(results))
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
# Proportional odds logistic regression, testing for an effect of topic on 
# preference, including position as a co-variate
# See http://www.ats.ucla.edu/stat/r/dae/ologit.htm

# install.packages("MASS")
library("MASS")
olr <- polr(preference ~ topic + position, data = topics.wide, Hess = TRUE)
olr.summary <- summary(olr)
olr.summary

# Add p-values based on t distribution
coeff.table <- coef(olr.summary)
p <- pnorm(abs(coeff.table[, "t value"]), lower.tail = FALSE) * 2
coeff.table <- cbind(coeff.table, "p.value" = p)
coeff.table

################################################################################
# 2-way ANOVA

topics.for.anova <- topics.wide
topics.for.anova$preference <- as.numeric(topics.for.anova$preference)
pref.anova <- aov(formula = preference ~ topic + position, data = topics.for.anova)
summary(pref.anova)

################################################################################
# GLMM
#install.packages("lme4")
library("lme4")

topics.for.glm <- topics.wide
topics.for.glm$preference <- as.numeric(topics.for.glm$preference)
topics.lm <- lmer(formula = preference ~ topic + (1|position), data = topics.for.glm)
topics.lm.summary <- summary(topics.lm)

################################################################################
# Kruskal Wallis omnibus test for an effect of topic on preference
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