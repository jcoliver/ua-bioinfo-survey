# Graphing training format preferences
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-02-01

rm(list = ls())
load(file = "output/results-processed.RData")
################################################################################
# Bar chart, faceting by position

# Pull out training format columns
format.columns <- grep(pattern = "training.", x = colnames(results))
# Don't want training.na
training.na <- grep(pattern = "training.na", x = colnames(results))
format.columns <- format.columns[which(format.columns != training.na)]
formats <- results[, format.columns]
# Remove "training." prefix from column names
colnames(formats) <- gsub(pattern = "training.", replacement = "", x = colnames(formats))
# Add position column
formats$position <- results$position

# Want to get totals, so we can order the format factor levels appropriately
format.sums <- colSums(x = formats[, -which(colnames(formats) == "position")], na.rm = TRUE)
format.order <- order(format.sums, decreasing = TRUE)
format.levels.order <- names(format.sums)[format.order]

# install.packages("tidyr")
library("tidyr")
format.wide <- gather(data = formats,
                      key = format.name, 
                      value = pref,
                      -position)

# Only want ones that actually marked a preference
format.wide <- format.wide[format.wide$pref == 1, ]
format.wide <- na.omit(format.wide)

# Re-level format.name so they appear in desired order
format.levels <- c("workshops", "group", "individual", "lecture", "online", "webinar")
format.wide$format.name <- factor(format.wide$format.name, levels = format.levels)

format.colors <- c("#AB0520", "#0C234B", "#558618", "#F19E1F", "#8CD9E3", "#B75527")

# install.packages("ggplot2")
library("ggplot2")
pdf(file = "output/figure-format-preferences-bar.pdf", useDingbats = FALSE, width = 7, height = 3.5)
  ggplot(data = format.wide,
         aes(x = format.name, fill = format.name)) +
    geom_bar(position = "dodge", color = "black", lwd = 0.2) + 
    facet_grid(. ~ position) + 
    scale_fill_manual(name = "Formats", values = format.colors) + 
    ylab(label = "Count") +
    xlab(label = NULL) + # label = "Format"
    theme_bw() + 
    theme(axis.ticks = element_blank(), 
          axis.text.x = element_blank(),
          text = element_text(family = "Times"))
dev.off()