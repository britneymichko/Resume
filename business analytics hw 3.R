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

















## old one

ggplot(data = insurance.df, mapping= aes(x = income, y = claim_amount, color = ClusterLabel)) +
  geom_point(size= 1.5) +
  labs(title = "Clusters of Insurance Customers",
       x = "Income (scaled)",
       y = "Claim Amount (scaled)",
       caption = "Source : Insurance Dataset",
       color = "Cluster") +
  scale_color_manual(values = c("black", "blue", "firebrick1")) + 
  scale_shape_manual(values = c(4, 18, 17)) + 
  scale_x_continuous(name = "Income (scaled)", breaks = seq(-2.5, 2.5, by = 1)) +
  scale_y_continuous(name = "Claim Amount (scaled)", breaks = seq(-2.5, 2.5, by = 0.5)) +
  theme(
    axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),  # 👈 better control
    aspect.ratio = 1/1,
    panel.background = element_blank()
  )

insurance.means <- aggregate(insurance.scaled, by = list(insurance_clusters1), FUN=mean)
print (insurance.means)

insurance$cluster <- insurance.clusters
head(insurance)


chi_claim <- xtabs(~ claim_amount + Cluster, data = insurance_df)
chi_claim
summary(chi_claim)

chi_income <- xtabs(~ income + Cluster, data = insurance_df)
chi_income
summary(chi_income)



#K-means clustering

insurance <- insurance[, c("townsize", "ed_cat")]

insurance_scaled <- scale(insurance_data)

set.seed(123)
insurance_km <- kmeans(insurance_scaled, centers = 3, nstart = 25)


insurance$kcluster <- insurance_km$cluster


insurance_df <- data.frame(
  townsize = insurance_scaled[, 1],
  ed_cat = insurance_scaled[, 2],
  Cluster = as.factor(insurance_km$cluster)
)

ggplot(data = insurance_df, aes(x = townsize, y = ed_cat, 
                                color = Cluster, shape = Cluster)) +
  geom_point(size = 2) +
  labs(
    x = "Townsize (scaled)",
    y = "Education Category (scaled)",
    title = "K-means Clustering of Insurance Data (k = 3)",
    caption = "Source: Insurance Dataset"
  ) +
  scale_color_manual(values = c("black", "blue", "firebrick1")) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    aspect.ratio = 1
  )

insurance_km$centers

head(insurance)
chi_townsize <- xtabs (~ townsize + Cluster, data = insurance_df)
chi_townsize
summary(chi_townsize)

head(insurance)
chi_ed_cat <- xtabs (~ ed_cat + Cluster, data = insurance_df)
chi_ed_cat
summary(chi_ed_cat)



scale_shape_manual(values = c(4, 18, 17)) 
