library(ggplot2)

insurance <- read.csv(file.choose(),header = TRUE)

psych::describe(insurance)


numeric_cols <- sapply(insurance, is.numeric)


insurance.scaled <- scale(insurance[, numeric_cols])

##hierarchical clustering
insurance.hclust <- hclust(dist(insurance.scaled))

#dendrogram and cut into clusters
plot(insurance.hclust)
insurance.clusters <- cutree(insurance.hclust, k=3)

plot(insurance.hclust, hang = -1, main= "Cutted Dendrogram", sub= "", xlab= "", ylab= "")
rect.hclust(insurance.hclust, k=3, border = "red")

insurance_df <- data.frame(insurance.scaled, Cluster= as.factor(insurance.clusters))
insurance_df$ClusterLabel <- factor(insurance_df$Cluster,
                                    levels = c(1, 2, 3),
                                    labels = c("Average Risk Clients", 
                                               "Well Off, Moderate Risk Clients", 
                                               "High-Risk, Low-Income Clients"))

ggplot(data = insurance_df, mapping = aes(x = income, y = claim_amount, color = ClusterLabel)) +
  geom_point(size = 1.5) +
  labs(title = "Clusters of Insurance Customers based on Income and the Cost of Claims",
       x = "Income",
       y = "Claim Amount",
       caption = "Source: Insurance Dataset",
       color = "Cluster") +
  scale_color_manual(values = c("black", "blue", "firebrick1")) + 
  scale_shape_manual(values = c(4, 18, 17)) + 
  scale_x_continuous(name = "Income", breaks = seq(-4, 20, by = 1)) +
  scale_y_continuous(name = "Claim Amount", breaks = seq(-4, 12, by = 0.5)) +
  theme(
    axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    aspect.ratio = 1/1,
    panel.background = element_blank(),
    
  )

insurance.means <- aggregate(insurance.scaled, by = list(insurance.clusters), FUN=mean)
print (insurance.means)

insurance$cluster <- insurance.clusters
head(insurance)




aggregate(cbind(income, claim_amount) ~ ClusterLabel, data = insurance_df, mean)



chi_claim <- xtabs(~ claim_amount + Cluster, data = insurance_df)
chi_claim
summary(chi_claim)

chi_income <- xtabs(~ income + Cluster, data = insurance_df)
chi_income
summary(chi_income)







