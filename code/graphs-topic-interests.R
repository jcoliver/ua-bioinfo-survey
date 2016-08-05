# Graphing topic interest levels
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-02-26

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
# Stacked bar chart for topic interest levels
topic.columns <- grep(pattern = "topic.", x = colnames(results))
training <- results[, topic.columns]

# Remove "topic." prefix
colnames(training) <- gsub(pattern = "topic.", replacement = "", x = colnames(training))

# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("dplyr")
library("ggplot2")
library("tidyr")
library("dplyr")
# Convert to wide format for easy summarizing
# training.topic  score 
# entrez          5
# entrez          2
# ...
# blast           3
# blast           4
training.wide <- gather(data = training,
                        key = training.topic,
                        value = score)

# Re-level training topic per ordering
load(file = "output/topics-ordered-decr.RData") # loads topics.decr.order
training.wide$training.topic <- factor(training.wide$training.topic, levels <- rev(topics.decr.order))

# Re-order the score factor to set order of plotting
training.wide$score <- factor(training.wide$score, levels = c(1, 2, 3, 4, 5))

pdf(file = "output/figure-topic-interests-bar.pdf", useDingbats = FALSE)
ggplot(data = training.wide, 
       aes(x = training.wide$training.topic,
           fill = as.factor(training.wide$score))) + # coerce int to factor
  geom_bar(width = 0.9, color = "white", size = 3) + 
  scale_fill_grey(name = "Interest Level",
                  labels = c("Not Interested", "", "", "", "Very Interested"),
                  start = 0.9, end = 0.2) +
  ylab(label = "Responses") +
  xlab(label = "Topic") + 
  coord_flip() +
  guides(fill = "none") + # Removes the legend
  theme_bw() + 
  theme(text = element_text(family = "Times"))
dev.off()