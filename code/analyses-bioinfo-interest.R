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
training <- results[,4:15]

total.respondents <- nrow(training)
# 72

# Figure out how many 5s (MARGIN = 1 for applying function to rows)
training$very.interested <- apply(X = training[,], MARGIN = 1, FUN = function(x) any(x == 5))
at.least.one.very <- sum(training$very.interested)
# 44
very.prop <- at.least.one.very/total.respondents
# 0.611

# Figure out how many 4 or 5s (MARGIN = 1 for applying function to rows)
training$interested <- apply(X = training[,], MARGIN = 1, FUN = function(x) any(x %in% c(4, 5)))
at.least.one.interested <- sum(training$interested)
# 57
interested.prop <- at.least.one.interested/total.respondents
# 0.792


################################################################################
# Analysis through time, see if late responses showed less interest
# Relationship is n.s. (p = 0.698) and in opposite direction as predicted
# See end for details

# Prepare start times
starts <- as.character(results$start)
starts.format <- strptime(starts, "%Y-%m-%d %H:%M:%S")
training$start.times <- starts.format

# Identify the training columns
not.training <- which(colnames(training) %in% c("very.interested", "start.times", "mean"))
num.training <- ncol(training) - length(not.training)
training$mean <- apply(X = training[, -not.training], MARGIN = 1, FUN = function(x) sum(x) / num.training) 

# The model
time.glm <- glm(training$mean ~ as.double(training$start.times))
time.glm
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                     -8.329e+01  2.213e+02  -0.376    0.708
# as.double(training$start.times)  5.951e-08  1.528e-07   0.390    0.698

