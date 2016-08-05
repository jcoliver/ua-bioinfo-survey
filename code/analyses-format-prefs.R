# Test for significant preference in training format
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-03-21

rm(list = ls())
load(file = "output/results-processed.RData")

# Pull out training format columns
format.columns <- grep(pattern = "training.", x = colnames(results))

# Don't want training.na
training.na <- grep(pattern = "training.na", x = colnames(results))
format.columns <- format.columns[which(format.columns != training.na)]
formats <- results[, format.columns]

# Remove the "training." prefix from column names for easier interpretation 
# of output
colnames(formats) <- gsub("training.", "", colnames(formats))

# install.packages("tidyr")
library("tidyr")
format.wide <- gather(data = formats,
                      key = format.name, 
                      value = pref)

# Drop NA
format.wide <- format.wide[!is.na(format.wide$pref), ]
format.wide$format.name <- factor(format.wide$format.name)

################################################################################
# Kruskal Wallis omnibus test for an affect of format on preference
# see http://www.ats.ucla.edu/stat/r/whatstat/whatstat.htm#kw
format.kw <- kruskal.test(x = format.wide$pref, g = format.wide$format.name)

# Kruskal-Wallis rank sum test
# 
# data:  format.wide$pref and as.factor(format.wide$format.name)
# Kruskal-Wallis chi-squared = 32.901, df = 5, p-value = 3.938e-06

################################################################################
# Logistic regression with workshop as reference
# see http://www.r-bloggers.com/logistic-regression-and-categorical-covariates/
# Make "workshops" the first level, and thus used as reference in glm
format.wide$format.name <- relevel(x = format.wide$format.name, ref = "workshops")
logit.workshops.ref <- glm(pref ~ format.name, data = format.wide, family = "binomial")
summary(logit.workshops.ref)
# Call:
#   glm(formula = pref ~ format.name, family = "binomial", data = format.wide)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6237  -1.0713  -0.7892   1.2874   1.6237  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             1.0068     0.2681   3.756 0.000173 ***
#   format.namegroup       -0.9786     0.3581  -2.733 0.006274 ** 
#   format.nameindividual  -1.3192     0.3600  -3.665 0.000248 ***
#   format.namelecture     -1.2617     0.3593  -3.511 0.000446 ***
#   format.nameonline      -1.2617     0.3593  -3.511 0.000446 ***
#   format.namewebinar     -2.0136     0.3791  -5.311 1.09e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 588.72  on 425  degrees of freedom
# Residual deviance: 554.66  on 420  degrees of freedom
# AIC: 566.66
# 
# Number of Fisher Scoring iterations: 4

# Write to a file
sink(file = "output/summary-logit-format-prefs-workshops.txt")
summary(logit.workshops.ref)
sink()

