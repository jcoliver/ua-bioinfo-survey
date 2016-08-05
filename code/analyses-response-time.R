# Analyzing survey completion times
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-04-18

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
# Mean completion time
# Times are factors, so change to seconds since epoch began
starts <- as.character(results$start)
starts.format <- strptime(starts, "%Y-%m-%d %H:%M:%S")
starts.epoch <- as.integer(as.POSIXct(starts.format))

ends <- as.character(results$end)
ends.format <- strptime(ends, "%Y-%m-%d %H:%M:%S")
ends.epoch <- as.integer(as.POSIXct(ends.format))

# Calculate time to completion
total.seconds <- ends.epoch - starts.epoch
total.count <- length(total.seconds)

# Some weren't completed, so omit anything over an hour
total.seconds <- total.seconds[total.seconds <= 60 * 60]
under.hour.count <- length(total.seconds) # 69
num.too.long <- total.count - under.hour.count # 3 excluded
mean.seconds <- mean(total.seconds) # # 265.96
mean.minutes <- mean.seconds / 60 # 4.43
