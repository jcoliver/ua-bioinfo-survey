# General assessment of interest in bioinformatics
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-04-25

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
# How many had at least one "Very interested" response (= 5)? How many had at 
# least one 4 or 5?

# Just interested in training topic interests
topic.columns <- grep(pattern = "topic.", x = colnames(results))
topics <- results[, topic.columns]

total.respondents <- nrow(topics)
# 72

# Figure out how many 5s (MARGIN = 1 for applying function to rows)
topics$very.interested <- apply(X = topics[,], MARGIN = 1, FUN = function(x) any(x == 5))
at.least.one.very <- sum(topics$very.interested)
# 44
very.prop <- at.least.one.very/total.respondents
# 0.611

# Figure out how many 4 or 5s (MARGIN = 1 for applying function to rows)
topics$interested <- apply(X = topics[,], MARGIN = 1, FUN = function(x) any(x %in% c(4, 5)))
at.least.one.interested <- sum(topics$interested)
# 57
interested.prop <- at.least.one.interested/total.respondents
# 0.792


################################################################################
# Analysis through time, see if late responses showed less interest
# Relationship is n.s. (p = 0.686) and in opposite direction as predicted
# See end for details

# Prepare start times
starts <- as.character(results$start)
starts.format <- strptime(starts, "%Y-%m-%d %H:%M:%S")
topics$start.times <- starts.format

# Identify the topics columns
not.topics <- which(colnames(topics) %in% c("very.interested", "start.times", "mean"))
num.topics <- ncol(topics) - length(not.topics)
topics$mean <- apply(X = topics[, -not.topics], MARGIN = 1, FUN = function(x) sum(x) / num.topics) 

# The model
time.glm <- glm(topics$mean ~ as.double(topics$start.times))
summary(time.glm)
# Call:
#   glm(formula = topics$mean ~ as.double(topics$start.times))
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.92178  -0.63125   0.06102   0.76098   1.98431  
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                     -8.171e+01  2.080e+02  -0.393    0.696
# as.double(topics$start.times)    5.831e-08  1.435e-07   0.406    0.686
# 
# (Dispersion parameter for gaussian family taken to be 1.302835)
# 
# Null deviance: 91.413  on 71  degrees of freedom
# Residual deviance: 91.198  on 70  degrees of freedom
# AIC: 227.35
# 
# Number of Fisher Scoring iterations: 2
