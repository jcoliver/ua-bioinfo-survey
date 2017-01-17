# Basic demographics of respondents
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-02-26

rm(list = ls())
load(file = "output/results-processed.RData")

# Number complete
completed <- nrow(results)
# 72

################################################################################
# Position information
pos.table <- table(results$position)
ids <- names(pos.table)
counts <- as.numeric(pos.table)
ids <- c(ids, "Other position")
counts <- c(counts, completed - sum(counts))

table.data <- data.frame(category = "Position", id = ids, count = counts)

################################################################################
# College information
college.table <- table(results$college)
ids <- names(college.table)
counts <- as.numeric(college.table)
ids[1] <- "Other / not listed"
new.data <- data.frame(category = "College", id = ids, count = counts)

# Want to distribute dual appointments
sci.and.med <- which(new.data$id == "Science & Medicine")
sci.med.count <- new.data$count[sci.and.med]
new.data$count[which(new.data$id == "Science")] <- new.data$count[which(new.data$id == "Science")] + sci.med.count
new.data$count[which(new.data$id == "Medicine")] <- new.data$count[which(new.data$id == "Medicine")] + sci.med.count
new.data <- new.data[-sci.and.med, ]

table.data <- rbind(table.data, new.data)

################################################################################
# Time at UA
time.bins <- cut(results$ua.time, breaks = c(0, 5, 10, 20, 40), labels = c("0-5", "5-10", "10-20", ">20"), include.lowest = TRUE)
time.bins <- na.omit(time.bins)
time.table <- table(time.bins)
ids <- names(time.table)
counts <- as.numeric(time.table)
ids <- c(ids, "Not given")
counts <- c(counts, completed - length(time.bins))
time.data <- data.frame(category = "Time", id = ids, count = counts)

table.data <- rbind(table.data, time.data)

table.data$percent <- 100 * round(table.data$count / completed, 3)

write.table(x = table.data, file = "output/table-demographics.txt", quote = FALSE, sep = "\t", row.names = FALSE)
# category                                        id count percent
# 1  Position                                   Faculty    33    45.8
# 2  Position                                     Staff    15    20.8
# 3  Position                                   Student    21    29.2
# 4  Position                            Other position     3     4.2
# 5   College                        Other / not listed     7     9.7
# 6   College Arizona Biological and Biomedical Program     1     1.4
# 7   College                            BIO5 Institute     1     1.4
# 8   College                                  Medicine    44    61.1
# 9   College                                   Nursing     2     2.8
# 10  College                                  Pharmacy     7     9.7
# 11  College                             Public Health     6     8.3
# 12  College                                   Science     5     6.9
# 13     Time                                       0-5    31    43.1
# 14     Time                                      5-10    13    18.1
# 15     Time                                     10-20    13    18.1
# 16     Time                                       >20     6     8.3
# 17     Time                                 Not given     9    12.5
# > 