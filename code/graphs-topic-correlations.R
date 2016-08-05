# Graphing correlation among training topics
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2016-01-28

rm(list = ls())
load(file = "output/results-processed.RData")
load(file = "output/topics-ordered-decr.RData")

################################################################################
# install.packages("ggplot2")
# install.packages("reshape2")
library("ggplot2")
library("reshape2")

# Omit public access (as.character otherwise will use level integer of factor)
topics.incr.order <- as.character(rev(topics.decr.order[topics.decr.order != "pub.access"]))

# Pull out the topics columns
colnames(results) <- gsub(pattern = "topic.", replacement = "", x = colnames(results))
topics <- results[, topics.incr.order]

################################################################################
# Do correlation test for each pair of topics; store rho and p
cor.mat.p <- matrix(data = NA, nrow = ncol(x = topics), ncol = ncol(x = topics))
rownames(x = cor.mat.p) <- colnames(topics)
colnames(x = cor.mat.p) <- colnames(topics)
cor.mat.rho <- cor.mat.p
for (i in 1:(ncol(x = topics) - 1)) {
  for (j in (i + 1):ncol(x = topics)) {
    test <- cor.test(x = topics[, i], y = topics[, j],
                     alternative = "greater", # in two-tailed, there are no negative r values
                     method = "spearman")
    cor.mat.p[i, j] <- cor.mat.p[j, i] <- test$p.value
    cor.mat.rho[i, j] <- cor.mat.rho[j, i] <- test$estimate
    rm(test)
  }
}
rm(i, j)

################################################################################
# Graph the P-values and rho
pdf(file = "output/figure-topic-correlations.pdf", useDingbats = FALSE)
# P-values
cor.p.melt <- melt(cor.mat.p)
cor.p.melt <- na.omit(cor.p.melt)
cor.p.melt$value <- -log10(cor.p.melt$value)

# Bonferroni correction would be a/m (a = 0.05, m is number of comparisons, 
# or n(n-1)/2 = 55)
# a/m = 0.05/55 = 0.00091
# -log10(a/m) = 3.041393
# All are significant at this level, as maximum p value is
# 0.00022 [or -log10(0.00022) = 3.658 (higher than minimum)

p.heatmap <- ggplot(data = cor.p.melt, aes(x = Var1, y = Var2, fill = value)) + 
  ggtitle(label = expression("-log"[10]*"(P-values)")) +
  geom_tile() + 
  scale_fill_gradient(low = "#EEEEEE", high = "#111111") + 
  xlab(label = NULL) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "Times"))
print(p.heatmap)

# Spearman rho values
cor.rho.melt <- melt(cor.mat.rho)
cor.rho.melt <- na.omit(cor.rho.melt)

rho.heatmap <- ggplot(data = cor.rho.melt, aes(x = Var1, y = Var2, fill = value)) + 
  ggtitle(label = "Spearman correlation coefficient") +
  geom_tile() + 
  scale_fill_gradient(low = "#EEEEEE", high = "#111111") + 
  xlab(label = NULL) +
  ylab(label = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "Times"))
print(rho.heatmap)

dev.off()
