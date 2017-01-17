# Responses through time, in relation to e-mail reminders
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-04-25

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
# Look at when respondents started survey, in relation to the most recent e-mail 
# reminder

# Prepare start times
starts <- as.character(results$start)
starts.format <- strptime(starts, "%Y-%m-%d %H:%M:%S")

# Setup reminder POSIXct objects
reminders <- c("2015-11-17 09:23:00", "2015-12-01 09:35:00", "2015-12-14 11:55:00")
reminders <- strptime(reminders, "%Y-%m-%d %H:%M:%S")

# Establish data frame; necessary to setup nearest.reminder as POSIXct / POSIXt 
# class now - will have issues later if we use NA
through.time <- data.frame(start.time = starts.format[order(starts.format)], 
                           nearest.reminder = reminders[1])

# For each start time, find closest reminder (in past)
for (response in 1:nrow(through.time)) {
  which.reminder <- 1 # By default, nearest is very first reminder
  if (through.time$start.time[response] > reminders[2]) { # started survey after second reminder
    if (through.time$start.time[response] < reminders[3]) { # started survey before third reminder
      which.reminder <- 2
    } else { # started survey after third reminder
      which.reminder <- 3
    }
  }
  if (which.reminder > 1) { # Only need to reassign if not 1 (because it was set as default when declaring through.time)
    through.time$nearest.reminder[response] <- reminders[which.reminder]
  }
}

# Calculate lag between most recent e-mail and start of response
through.time$latency <- difftime(time1 = through.time$start.time,
                                 time2 = through.time$nearest.reminder,
                                 units = "hours")

# Sanity check
if (length(which(through.time$latency < 0)) > 0) {
  stop("ERROR: Negative value identified in latency calculations.")
}

# Figure out how many occurred within window time of e-mail
total.responses <- nrow(through.time)
cat("Total responses: ", total.responses, "\n", sep = "")

# For a 12 hour window
window <- 12
within.12 <- which(through.time$latency <= window)
cat("Within ", window, " hr window(s): ", length(within.12), "\n", sep = "")
# 64
cat("Proportion within ", window, " hr window(s): ", round(x = length(within.12)/total.responses, digits = 3), "\n", sep = "")
# 0.889

# For a 24 hour window
window <- 24
within.24 <- which(through.time$latency <= window)
cat("Within ", window, " hr window(s): ", length(within.24), "\n", sep = "")
# 66
cat("Proportion within ", window, " hr window(s): ", round(x = length(within.24)/total.responses, digits = 3), "\n", sep = "")
# 0.917