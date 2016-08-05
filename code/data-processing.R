# Preliminary data processing of raw results
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-01-27

# Read in the file and do some preliminary processing
results <- read.delim(file = "data/survey-results-processed.txt")

# Convert position to a factor
results$position[results$position == 1] <- "Faculty" # Faculty
results$position[results$position == 2] <- "Staff"   # Staff
results$position[results$position == 3] <- "Staff"   # Post-doc
results$position[results$position == 4] <- "Student" # Graduate student
results$position[results$position == 5] <- "Faculty" # Visiting scholar, but none in data
results$position[results$position == 6] <- "Staff"   # appointed professional

results$position <- as.factor(results$position)

################################################################################
# Processing resource use frequency data
#
# Take vector of integers (use) and character vector (use.values) and return 
# copy of use vectors as factors using use.values for levels
useToFactor <- function(use, use.values) {
  if(!is.integer(use)) {
    stop("use must be a vector of integers")
  }
  if(!is.character(use.values)) {
    stop("use.values must be a character vector")
  }
  use.chars <- use.values[use]
  use.chars <- factor(use.chars, levels = use.values)
}

frequency.cats <- c("Never", "Yearly", "Monthly", "Weekly", "Daily")
results$freq.online <- useToFactor(use = results$freq.online, use.values = frequency.cats)
results$freq.physical <- useToFactor(use = results$freq.physical, use.values = frequency.cats)
results$freq.personnel <- useToFactor(use = results$freq.personnel, use.values = frequency.cats)

save(results, file = "output/results-processed.RData")

################################################################################
# Ordering topics based on scores
#
# Sorted by how many 5s then by reverse order of how many 1s (actual processing 
# happens in reverse order)
# install.packages("plyr")
library("plyr")

topic.columns <- which(substr(colnames(results), 1, 6) == "topic.")
topic.names <- colnames(results[, topic.columns])
empty.counts <- matrix(data = NA, nrow = length(topic.names), ncol = 5)
topic.counts <- data.frame(empty.counts, row.names = topic.names)
colnames(topic.counts) <- c("pref.1", "pref.2", "pref.3", "pref.4", "pref.5")

# Count number of 5s, 4s, 3s, etc. for each topic
for (curr.topic in topic.names) {
  curr.topic.counts <- plyr::count(df = results, vars = curr.topic)
  topic.counts[curr.topic,] <- curr.topic.counts$freq
}

# Reverse order by 1s, then order by 5s
topic.counts <- topic.counts[order(topic.counts$pref.1, decreasing = FALSE),]
topic.counts <- topic.counts[order(topic.counts$pref.5, decreasing = TRUE),]

topics.decr.order <- factor(rownames(topic.counts))
save(topics.decr.order, file = "output/topics-ordered-decr.RData")

