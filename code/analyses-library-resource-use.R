# Test for differential use of library resources
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-01-27

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
# Kruskal Wallis test
# install.packages("tidyr")
library("tidyr")
resource.cols <- which(substr(x = colnames(results), 1, 5) == "freq.")
resource.freq <- results[, resource.cols]
colnames(resource.freq) <- gsub(pattern = "freq.", replacement = "", x = colnames(resource.freq))
resource.freq$position <- results$position
resource.long <- resource.freq %>% gather(key = resource, value = freq, -position)
resource.long$resource <- factor(x = resource.long$resource)

kw <- kruskal.test(x = resource.long$freq, g = resource.long$resource)
# Kruskal-Wallis rank sum test
# 
# data:  resource.long$freq and resource.long$resource
# Kruskal-Wallis chi-squared = 61.479, df = 2, p-value = 4.468e-14