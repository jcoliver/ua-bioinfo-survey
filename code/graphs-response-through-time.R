# Graphing responses through time
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-04-18

rm(list = ls())
load(file = "output/results-processed.RData")

################################################################################
# Plot survey starting times

# Prepare start times & counts
starts <- as.character(results$start)
starts.format <- strptime(starts, "%Y-%m-%d %H:%M:%S")

through.time <- data.frame(start.time = starts.format[order(starts.format)], count = NA)
through.time$count <- 1:length(through.time$count)

# Create objects for rectangles showing 12hr windows after e-mails
reminder.window <- 12 # hours of window to look at
reminders <- c("2015-11-17 09:23:00", "2015-12-01 09:35:00", "2015-12-14 11:55:00")
reminders.time <- strptime(reminders, "%Y-%m-%d %H:%M:%S")
reminders.df <- data.frame(start = reminders.time, end = NA)
reminders.df$end <- reminders.df$start + reminder.window * 60 * 60

# install.packages("ggplot2")
library("ggplot2")
pdf(file = "output/figure-response-through-time.pdf", useDingbats = FALSE)
ggplot() +
  geom_rect(data = reminders.df, 
            aes(xmin = start, xmax = end,
                ymin = -Inf, ymax = Inf),
            alpha = 0.4) +
  geom_line(data = through.time, aes(start.time, count)) +
  ylab(label = "# Completed Surveys") +
  xlab(label = "Date") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        text = element_text(family = "Times")) +
  ggtitle(label = paste0("Window: ", reminder.window, " hours"))
dev.off()
