#############################################################
# Machine Learning in Finance                               #
# DBS, University of Deusto                                 #
# ADE + FAB                                                 #
#Assignment 4                                               #
#############################################################
# Set working directory to this script's folder (RStudio)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(script_path)) setwd(dirname(script_path))
}
dir.create("figures", showWarnings = FALSE)

#install.packages(c("ggplot2","factoextra","ggrepel","dplyr","tidyr","psych","purrr","clue","clValid","tibble","cowplot","cluster","fpc"))

# ggplot2: for data visualization
library(ggplot2)
# factoextra: helper functions for clustering visualization
library(factoextra)
# ggrepel: better text label positioning in ggplot2
library(ggrepel)
# dplyr: data manipulation
library(dplyr)
# tidyr: data reshaping and tidying
library(tidyr)
library(psych)
library(purrr)
library(clue)
library(clValid)
library(tibble)
library(cowplot)
library(cluster) 
library(fpc) 


# Correct formatting for importing dataset
companies_financials <- read.csv('companies_financials.csv',row.names = 1, sep = ",", dec = ".")
summary(companies_financials)
table(companies_financials$Sector)
table(companies_financials$Industry_Group)
numeric_financials <- companies_financials[, sapply(companies_financials, is.numeric)]
summary(numeric_financials)
names(numeric_financials)
#variable descripction


# 1.  Price_USD: Current share price in U.S. dollars
# 2.  Quarterly_Return_Pct: (Current_Price - Past_Price) / Past_Price * 100
# 3.  Price_Volatility: Standard deviation of price over a period
# 4.  Return_on_Equity: Net_Profit / Shareholders_Equity
# 5.  Liquidity_CashRatio: Cash / Current_Liabilities
# 6.  Cash_Flow_USD: Net_Cash_From_Operations - CapEx ± Other_Flows
# 7.  Net_Profit_USD: Revenue - Expenses - Taxes - Interest
# 8.  EPS_USD: Net_Profit / Shares_Outstanding
# 9.  Shares_Outstanding_M: Number of shares issued (in millions)
#10. PE_Ratio: Price_Per_Share / EPS
#11. PB_Ratio: Price_Per_Share / Book_Value_Per_Share
#12. Market_Cap_Millions: Price_Per_Share * Shares_Outstanding_M


corPlot(companies_financials[-1:-3],xlas=2) #no correlations
############################################################
#Data diccionary
#Companies financials <- full database
#numeric financials <- numeric variables -shares & price
#data <- numeric financials standarized
#hclustward <- ward with data
#numeric_financials_f1 <- numeric financials - 7 outliers
#data_f1 <- data - 7 outliers
#for clust ward, if nothing is first, .1 is second(definitive), and .3 is third
#numeric financials2 <- numeric financials f1 - clusters 5 & 6
#clustersN <- n remains the number of clusters
#full financials <- numerical finacials_f1 + sector + industry
#significant financials <- numerical finacials_f1 - not significant variables


############################################################
# Data preparation                                        ##
############################################################


#we modify some variables
numeric_financials$MarketCapM <- numeric_financials$Price_USD*numeric_financials$Shares_Outstanding_M
numeric_financials <- numeric_financials[-c(1,9)]
summary(numeric_financials)
#we modify this ratio so the logarithm does not give us any trouble
numeric_financials$Liquidity_CashRatio <- ifelse(numeric_financials$Liquidity_CashRatio==0,1,numeric_financials$Liquidity_CashRatio)
# Graphic before logarithm
ggplot(data = numeric_financials, aes(x = Return_on_Equity, y = Liquidity_CashRatio, label = rownames(numeric_financials))) +
  geom_point(color="blue") + geom_text_repel(size = 4)


#we visualize distributions
colSums(is.na(numeric_financials))
numeric_financials %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density() +
  theme(strip.text = element_text(size = 5))
#some of them need a logarithmic transformation
numeric_financials[ , c("Liquidity_CashRatio", "MarketCapM", "PE_Ratio","Price_Volatility","Return_on_Equity")] <- log(numeric_financials[ , c("Liquidity_CashRatio", "MarketCapM", "PE_Ratio","Price_Volatility","Return_on_Equity")])
numeric_financials %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density() +
  theme(strip.text = element_text(size = 5))
#it looks better
numeric_financials_st <- as.data.frame(scale(numeric_financials)) 
#we check for outliers defined as observations with values higher than 4 times the std
summary(numeric_financials_st)
numeric_financials_st$sum <- rowSums(abs(numeric_financials_st) > 4)
table(numeric_financials_st$sum)
colSums(abs(numeric_financials_st) > 4)
outliers <- numeric_financials_st[numeric_financials_st$sum > 1,]
outliers
#only 2 so no worries


# graphic after logarithm
ggplot(data = numeric_financials, aes(x = Return_on_Equity, y = Liquidity_CashRatio, label = rownames(numeric_financials))) +
  geom_point(color="blue") + geom_text_repel(size = 4)




# In order to perform clustering we only include numerical variables


# When clustering, scaling is necessary
data <- as.data.frame(scale(numeric_financials))
summary(data)


############################################################
# Hierarchical clustering (ward)                          ##
############################################################


# Set the random seed for reproducibility
set.seed(123)


# Compute and plot the within-cluster sum of squares (wss) for k = 1 to k = 10
fviz_nbclust(data, hcut, method = "wss",) +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


#library(purrr)
#m <- c("single", "complete","average", "ward")
#names(m) <- c(  "single", "complete","average", "ward")


# function to compute coefficient
#ac <- function(x) {
# agnes(data, method = x)$ac
#}


#map_dbl(m, ac)
#agnes(data, method = "ward")


#fviz_nbclust(data, FUN = hcut, method = "wss")
#fviz_nbclust(data, FUN = hcut, method = "silhouette")
#library(cluster)
#gap_stat <- clusGap(data, FUN = hcut, nstart = 25, K.max = 10, B = 50)
#fviz_gap_stat(gap_stat)
# Compute clValid
clmethods <- c("hierarchical","kmeans")
first_result <- clValid(data, nClust =2:8,clMethods = clmethods, validation = "internal")
# Summary
summary(first_result)
#really bad results for kmeans


#we do hierarchical clustering with 3:6 clusters
dist_financials <- dist(data)
hclust_ward <- hclust(dist_financials, method = "ward.D2")
cut_ward3  <- cutree(hclust_ward, k = 3)
cut_ward4 <- cutree(hclust_ward, k = 4)
cut_ward5 <- cutree(hclust_ward, k = 5)
cut_ward6 <- cutree(hclust_ward, k = 6)


#we check for the distribution of the clusters
table(cut_ward3)
table(cut_ward4)
table(cut_ward5)
table(cut_ward6)
#clearly 7 outliers


fviz_cluster(list(data = data, cluster = cut_ward3),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()


fviz_cluster(list(data = data, cluster = cut_ward4),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(list(data = data, cluster = cut_ward5),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(list(data = data, cluster = cut_ward6),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
#clearer in the graphic


cut_avg_ward <- cutree(hclust_ward, k = 6)
numeric_financials$clu_ward_4 <- factor(cut_avg_ward)
table(numeric_financials$clu_ward_4)
rownames(outliers)
rownames(numeric_financials[numeric_financials$clu_ward_4==5,]) #we have there the 2 outliers, APA and DVN
rownames(numeric_financials[numeric_financials$clu_ward_4==6,])
clust5 <- numeric_financials[numeric_financials$clu_ward_4==5,-c(11,12)]
summary(clust5)
clust5_names <- row.names(clust5)
companies_financials[clust5_names,]$Sector #casually 6/7 are from the energy sector


#we get rid of them
numeric_financials_f1 <- numeric_financials[!numeric_financials$clu_ward_4 %in% c(5), ]
variables <- colnames(clust5)
png("figures/boxplots_cluster5_vs_rest.png", width = 1600, height = 1200, res = 150)
par(mfrow = c(3, 4), mar = c(3, 4, 2, 1))
variables <- colnames(clust5)
invisible(lapply(variables, function(x) {
  boxplot(numeric_financials_f1[[x]], clust5[[x]], horizontal = TRUE,
          xlab = "", main = paste(x, "Comparison"), col = "lightgreen")
}))
dev.off()
numeric_financials <- numeric_financials[-c(11,12)]
numeric_financials_f1 <-numeric_financials_f1[-c(11,12)]
summary(numeric_financials_f1)
#we repeat again the process to check outliers
numeric_financials_st <- as.data.frame(scale(numeric_financials_f1)) 
summary(numeric_financials_st)
numeric_financials_st$sum <- rowSums(abs(numeric_financials_st) > 4)
table(numeric_financials_st$sum)
colSums(abs(numeric_financials_st) > 4)
summary(numeric_financials_st)
no_outliers <- numeric_financials_st[numeric_financials_st$sum < 2,]
summary(no_outliers)
outliers2 <- numeric_financials_st[numeric_financials_st$sum > 1,]
count(outliers2)
rownames(outliers)
rownames(outliers2) #different outliers as we have removed the previous ones
outliers2
data_f1 <- as.data.frame(scale(numeric_financials_f1))


clmethods <- c("hierarchical","kmeans")
definitive_result <- clValid(data_f1, nClust =4,clMethods = clmethods, validation = "internal")
# Summary
summary(first_result)
summary(definitive_result) #some metric better, some worse in hierarchical, better kmeans


#we do cluster again
fviz_nbclust(data_f1, hcut, method = "wss",) +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


dist_financials_f1 <- dist(data_f1)
hclust_ward_f1 <- hclust(dist_financials_f1, method = "ward.D2")
cut_ward3.1 <- cutree(hclust_ward_f1, k = 3)
cut_ward4.1 <- cutree(hclust_ward_f1, k = 4)
cut_ward5.1 <- cutree(hclust_ward_f1, k = 5)
cut_ward6.1 <- cutree(hclust_ward_f1, k = 6)




fviz_cluster(list(data = data_f1, cluster = cut_ward3.1),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(list(data = data_f1, cluster = cut_ward4.1),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(list(data = data_f1, cluster = cut_ward5.1),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(list(data = data_f1, cluster = cut_ward6.1),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
table(cut_ward3.1)
table(cut_ward4.1)
table(cut_ward5.1)
table(cut_ward6.1)
#nice distribution with 4 clusters


# We compare it with the complete linkage method
hclust_comp <- hclust(dist_financials_f1, method = "complete")
# Cluster memberships
cut_avg_comp <- cutree(hclust_comp, k = 4)
numeric_financials_f1$clu_comp_4 <- factor(cut_avg_comp)
summary(numeric_financials_f1$clu_comp_4)


cut_avg_ward <- cutree(hclust_ward_f1, k = 4)
numeric_financials_f1$clu_ward_4 <- factor(cut_avg_ward)
table(numeric_financials_f1$clu_ward_4)
# Cluster membership comparison
table(numeric_financials_f1$clu_ward_4, numeric_financials_f1$clu_comp_4)
#clearly worse the complete method with one group taking most of them


#however we try getting rid of the 2 smallest clusters, to check if it improves


numeric_financials_f1<- numeric_financials_f1[-11]
cut_avg_ward <- cutree(hclust_ward_f1, k = 6)
numeric_financials_f1$clu_ward_4 <- factor(cut_avg_ward)
table(numeric_financials_f1$clu_ward_4)
#now we get rid of the 8 extrange individuals to check whether the model improves
numeric_financials2 <- numeric_financials_f1[!numeric_financials_f1$clu_ward_4 %in% c(5, 6), ]
numeric_financials2 <- numeric_financials2[-11]


#outliers
numeric_financials_st2 <- as.data.frame(scale(numeric_financials2)) 
summary(numeric_financials_st2)
numeric_financials_st2$sum <- rowSums(abs(numeric_financials_st2) > 4)
table(numeric_financials_st2$sum) #no problematic outliers
data2 <- as.data.frame(scale(numeric_financials2))
clmethods <- c("hierarchical","kmeans")
result_removing_too_many_outliers <- clValid(data2, nClust =4,clMethods = clmethods, validation = "internal")
# Summary
summary(result_removing_too_many_outliers) #kmeans worse, less data, hierarchical better in connectivity, worse in the rest (looking at 4 clusters)
summary(definitive_result)
summary(first_result)


fviz_nbclust(data2, hcut, method = "wss",) +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


#we perfrom ward
dist_financials.2 <- dist(data2)
hclust_ward.2 <- hclust(dist_financials.2, method = "ward.D2")
cut_ward4.2 <- cutree(hclust_ward.2, k = 4)
cut_ward5.2 <- cutree(hclust_ward.2, k = 5)
cut_ward6.2 <- cutree(hclust_ward.2, k = 6)
table(cut_ward4.2) #really concentrated
table(cut_ward5.2) #smaller groups
table(cut_ward6.2) #bad distributions




fviz_cluster(list(data = data2, cluster = cut_ward4.2),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(list(data = data2, cluster = cut_ward5.2),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(list(data = data2, cluster = cut_ward6.2),
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Ward)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
table(cut_ward4.1)
table(cut_ward4.2) #way better before
table(cut_ward5.1)
table(cut_ward5.2)#better the new 5 clusters
summary(result_removing_too_many_outliers)
summary(definitive_result)
#however, the best remain the 4 previous clusters, with kmeans being more similar




############################################################
# Non hierarchical clustering  (k-means)                  ##
############################################################




#with that being said, we perform Kmeans clustering
clusters6 <- kmeans(x = data_f1, centers = 6, nstart = 50)
clusters5 <- kmeans(x = data_f1, centers = 5, nstart = 50)
clusters4 <- kmeans(x = data_f1, centers = 4, nstart = 50)
clusters3 <- kmeans(x = data_f1, centers = 3, nstart = 50)
# Display the cluster assignments for each observation
clusters4$cluster


# Display the sizes of each cluster
clusters3$size
clusters4$size
clusters5$size
clusters6$size
table(cut_ward4.1)


# Display the number of iterations performed in the clustering algorithm
clusters4$iter


# Display the cluster centers
clusters4$centers


# Display the within-cluster sum of squares for each cluster
clusters4$withinss


# Display the between-cluster sum of squares
clusters4$betweenss


# Display the total sum of squares
clusters4$totss


# Display the total within-cluster sum of squares
clusters4$tot.withinss
# Visualize the clusters using the fviz_cluster function from the factoextra package
fviz_cluster(clusters3,data = data_f1,
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (k-means)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
p1 <-  fviz_cluster(clusters4,data = data_f1,
                    ellipse.type = "convex", repel = FALSE,
                    show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (k-means)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
p1
fviz_cluster(clusters5,data = data_f1,
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (k-means)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
fviz_cluster(clusters6,data = data_f1,
             ellipse.type = "convex", repel = FALSE,
             show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (k-means)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()








# now we compare cluster memberships
hclust_ward_f1 <- hclust(dist_financials_f1, method = "ward.D2")
cut_avg_ward <- cutree(hclust_ward_f1, k = 4)
numeric_financials_f1$clu_ward_4 <- factor(cut_avg_ward)
summary(numeric_financials_f1$clu_ward_4)
numeric_financials_f1$Kmeans <- factor(clusters4$cluster)
numeric_financials_f1
table(numeric_financials_f1$Kmeans)
table(numeric_financials_f1$clu_ward_4)




#as kmeans is random, we have to ensure that they match correctly the groups
conf_mat <- table(numeric_financials_f1$Kmeans, numeric_financials_f1$clu_ward_4)
assignment <- solve_LSAP(conf_mat, maximum = TRUE)
numeric_financials_f1$Kmeans <- assignment[numeric_financials_f1$Kmeans]


df <- as.data.frame.table(table(numeric_financials_f1$clu_ward_4, numeric_financials_f1$Kmeans))
colnames(df) <- c("Hierarchical", "Kmeans", "Count")
table(numeric_financials_f1$Kmeans)
table(numeric_financials_f1$clu_ward_4)
kmeans_result <- clValid(data_f1, nClust = 4,clMethods = clmethods, validation = "internal")
summary(kmeans_result) #just to remind it, the good one is hierarchical




############################################################
# Clustering method comparison                            ##
############################################################


# Heatmap
ggplot(df, aes(x = Kmeans, y = Hierarchical, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 4) +
  scale_fill_gradient(low = "lightgreen", high = "red") +
  labs(title = "Heatmap: Hierarchical Clustering vs K-means Clustering",
       x = "K-means cluster", y = " Hierarchical") +
  theme_minimal()


#we do it with percentages as groups aren't completly equal
prop_tab <- prop.table(table(numeric_financials_f1$clu_ward_4, numeric_financials_f1$Kmeans), 2) * 100
df2 <- as.data.frame(as.table(round(prop_tab, 1)))
colnames(df2) <- c("Hierarchical", "Kmeans", "Percent")




ggplot(df2, aes(x = Kmeans, y = Hierarchical, fill = Percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(Percent, "%")), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "% of coincidence for column",
       x = "K-means", y = "Hierarchical") +
  theme_minimal()
#the biggest differences are in 1,2


test_differences <- numeric_financials_f1[numeric_financials_f1$Kmeans %in% c(1, 2), ]
test_differences <- test_differences[test_differences$clu_ward_4%in% c(1, 2),]
test_differences$group <-  ifelse(
  test_differences$Kmeans == 1 & test_differences$clu_ward_4 == 1, 1,
  ifelse(test_differences$Kmeans == 2 & test_differences$clu_ward_4 == 2, 2, 3)
)
#group3 is 2 kmeans, 1 ward
#we isolate them and check for differences
test_differences %>%
  group_by(group) %>%
  summarise(across(where(is.numeric), mean))
table(test_differences$group)
png("figures/boxplots_group_differences.png", width = 1600, height = 1200, res = 150)
par(mfrow = c(3, 4), mar = c(3, 4, 2, 1))
variables <- colnames(test_differences[,-11:-13])
invisible(lapply(variables, function(x) {
  boxplot(test_differences[[x]] ~ test_differences$group,
          horizontal = TRUE,
          xlab = "", main = paste(x, "by group"),
          col = "lightgreen")
}))
dev.off()


anova(aov(Cash_Flow_USD~group, data= test_differences))
anova_pvalues <- sapply(variables, function(var) {
  formula <- as.formula(paste(var, "~ clu_ward_4"))
  model <- aov(formula, data = test_differences)
  summary(model)[[1]][["Pr(>F)"]][1]
})
anova_pvalues < 0.05 #there are differences, but we want to check especifically 1 and 3
sort(anova_pvalues)


test_differences1 <- test_differences[test_differences$group %in% c(1,3),]
anova(aov(Cash_Flow_USD~group, data= test_differences1))
anova_pvalues <- sapply(variables, function(var) {
  formula <- as.formula(paste(var, "~ group"))
  model <- aov(formula, data = test_differences1)
  summary(model)[[1]][["Pr(>F)"]][1]
})
anova_pvalues <-as.data.frame(anova_pvalues)
anova_pvalues < 0.05 #yes there are differences, in 4 variables


test_differences2 <- test_differences[test_differences$group %in% c(2,3),]
anova(aov(Cash_Flow_USD~group, data= test_differences2))
anova_pvalues2 <- sapply(variables, function(var) {
  formula <- as.formula(paste(var, "~ group"))
  model <- aov(formula, data = test_differences2)
  summary(model)[[1]][["Pr(>F)"]][1]
})
anova_pvalues2<-as.data.frame(anova_pvalues2)
anova_pvalues2 < 0.05 #yes there are differences, in 4 variables
dfdifferences <- cbind(anova_pvalues,anova_pvalues2)
dfdifferences
dfdifferences$anova_pvalues <- dfdifferences$anova_pvalues<0.05
dfdifferences$anova_pvalues2 <- dfdifferences$anova_pvalues2 <0.05
dfdifferences
#we check whether there are significant differences among the groups, and we conclude that the group 3 is
# more similar to group one, than to group2
A <-as.data.frame(round(sort(prop.table(table(companies_financials[test_differences$group == 1, ]$Sector)),decreasing=TRUE),2))
B <- as.data.frame(round(sort(prop.table(table(companies_financials[test_differences$group == 2, ]$Sector)),decreasing=TRUE),2))
C <- as.data.frame(round(sort(prop.table(table(companies_financials[test_differences$group == 3, ]$Sector)),decreasing=TRUE),2))
colnames(A) <- c("Sector", "Group1")
colnames(B) <- c("Sector", "Group2")
colnames(C) <- c("Sector", "Group3")
merged <- full_join(A, B, by = "Sector") %>%
  full_join(C, by = "Sector")
merged






#now we check for the standarized profiles of the clusters
numeric_financials_3 <- as.data.frame(scale(numeric_financials_f1[, -c(11:12)]))
numeric_financials_3[c(names(numeric_financials_f1)[11:12])] <- numeric_financials_f1[, 11:12]
numeric_financials_3 <- numeric_financials_3[names(numeric_financials_f1)]


# Calculate the mean values for each variable within each cluster
t1 <- numeric_financials_3[, -12] %>%
  group_by(clu_ward_4) %>%
  summarise_all("mean")


# Reshape the data for plotting
cluster_pro_w <- t1 %>%
  pivot_longer(!clu_ward_4, names_to = "Variable", values_to = "z_values")


# Plot the standardized profiles for each cluster
ggplot(cluster_pro_w, aes(x = Variable, y = z_values, group = clu_ward_4)) +
  geom_line(aes(color = clu_ward_4), linetype = "F1", size = 1) +
  geom_point(aes(color = clu_ward_4), size = 5) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "#993333", size = 8, angle = 45)) +
  labs(x = "Financial metrics",
       y = "Standardized values",
       colour = "Cluster - Ward method",
       title = "Cluster: standardized profiles")




############################################################
# Cluster validation (logistic regression and anova)       ##
############################################################




cluster1 <- companies_financials[rownames(numeric_financials_f1[numeric_financials_f1$clu_ward_4==1,]),]
cluster2 <- companies_financials[rownames(numeric_financials_f1[numeric_financials_f1$clu_ward_4==2,]),]
cluster3 <- companies_financials[rownames(numeric_financials_f1[numeric_financials_f1$clu_ward_4==3,]),]
cluster4 <- companies_financials[rownames(numeric_financials_f1[numeric_financials_f1$clu_ward_4==4,]),]


databases <- list(
  base1 = cluster1,
  base2 = cluster2,
  base3 = cluster3,
  base4 = cluster4
)
get_sector_prop <- function(data5, name) {
  prop <- round(prop.table(table(data5$Sector)) * 100,0)
  df <- as.data.frame(prop)
  colnames(df) <- c("Sector", name)
  return(df)
}
#we check for sector % in groups
table_list <- imap(databases, get_sector_prop)
final_table <- reduce(table_list, full_join, by = "Sector")
final_table[is.na(final_table)] <- 0
final_table <- final_table %>% arrange(desc(base1))
print(final_table) #we can see some clear groups as energy


get_industry_prop <- function(data5, name) {
  prop <- round(prop.table(table(data5$Industry_Group)) * 100,0)
  df <- as.data.frame(prop)
  colnames(df) <- c("Industry", name)
  return(df)
}
#now the same with sector, but as they are more categories it gets worse
table_list <- imap(databases, get_industry_prop)
final_table <- reduce(table_list, full_join, by = "Industry")
final_table[is.na(final_table)] <- 0
final_table <- final_table %>% arrange(desc(base1))
print(final_table)


numeric_financials_f1$cluster1 <- ifelse(numeric_financials_f1$clu_ward_4==1,1,0)
numeric_financials_f1$cluster2 <- ifelse(numeric_financials_f1$clu_ward_4==2,1,0)
numeric_financials_f1$cluster3 <- ifelse(numeric_financials_f1$clu_ward_4==3,1,0)
numeric_financials_f1$cluster4 <- ifelse(numeric_financials_f1$clu_ward_4==4,1,0)


#we perform logistic models to check for clustering asignmnet variables
m1 <- glm(cluster1 ~ Quarterly_Return_Pct + Price_Volatility + Return_on_Equity + Liquidity_CashRatio + Cash_Flow_USD +
            Net_Profit_USD + EPS_USD + PE_Ratio + PB_Ratio + MarketCapM,
          data = numeric_financials_f1, family = binomial(link = "logit"))
m2 <- glm(cluster2 ~ Quarterly_Return_Pct + Price_Volatility + Return_on_Equity + Liquidity_CashRatio + Cash_Flow_USD +
            Net_Profit_USD + EPS_USD + PE_Ratio + PB_Ratio + MarketCapM,
          data = numeric_financials_f1, family = binomial(link = "logit"))
m3 <- glm(cluster3 ~ Quarterly_Return_Pct + Price_Volatility + Return_on_Equity + Liquidity_CashRatio + Cash_Flow_USD +
            Net_Profit_USD + EPS_USD + PE_Ratio + PB_Ratio + MarketCapM,
          data = numeric_financials_f1, family = binomial(link = "logit"))
m4 <- glm(cluster4 ~ Quarterly_Return_Pct + Price_Volatility + Return_on_Equity + Liquidity_CashRatio + Cash_Flow_USD +
            Net_Profit_USD + EPS_USD + PE_Ratio + PB_Ratio + MarketCapM,
          data = numeric_financials_f1, family = binomial(link = "logit"))


# Display the summary of the logistic regression model
summary(m1)
summary(m2)
summary(m3)
summary(m4)
table(numeric_financials_f1$cluster4)


# Logistic model including both numerical and categorical variables
full_financials <- numeric_financials_f1
full_financials$Sector <- companies_financials[rownames(full_financials), "Sector"]
full_financials$Industry_Group <- companies_financials[rownames(full_financials), "Industry_Group"]




str(full_financials)
full_financials[-c(11:18)] %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density() +
  theme(strip.text = element_text(size = 5))
table(full_financials$clu_ward_4)
m6.1 <- glm(cluster2 ~ Quarterly_Return_Pct  + Price_Volatility  + Return_on_Equity  + Liquidity_CashRatio  + Cash_Flow_USD  +
              Net_Profit_USD  + EPS_USD  + PE_Ratio  + PB_Ratio  + MarketCapM + Industry_Group,
            data = full_financials, family = binomial(link = "logit"))
m1.1 <- glm(cluster1 ~ Quarterly_Return_Pct  + Price_Volatility  + Return_on_Equity  + Liquidity_CashRatio  + Cash_Flow_USD  +
              Net_Profit_USD  + EPS_USD  + PE_Ratio  + PB_Ratio  + MarketCapM + Sector,
            data = full_financials, family = binomial(link = "logit"))
m2.1 <- glm(cluster2 ~ Quarterly_Return_Pct  + Price_Volatility  + Return_on_Equity  + Liquidity_CashRatio  + Cash_Flow_USD  +
              Net_Profit_USD  + EPS_USD  + PE_Ratio  + PB_Ratio  + MarketCapM + Sector,
            data = full_financials, family = binomial(link = "logit"))
m3.1 <- glm(cluster3 ~ Quarterly_Return_Pct  + Price_Volatility  + Return_on_Equity  + Liquidity_CashRatio  + Cash_Flow_USD  +
              Net_Profit_USD  + EPS_USD  + PE_Ratio  + PB_Ratio  + MarketCapM + Sector,
            data = full_financials, family = binomial(link = "logit"))
m4.1 <- glm(cluster4 ~ Quarterly_Return_Pct  + Price_Volatility  + Return_on_Equity  + Liquidity_CashRatio  + Cash_Flow_USD  +
              Net_Profit_USD  + EPS_USD  + PE_Ratio  + PB_Ratio  + MarketCapM + Sector,
            data = full_financials, family = binomial(link = "logit"))
table(full_financials$Industry_Group)
table(full_financials$clu_ward_4)
summary(m1.1)
summary(m2.1) #sector significant negatively
summary(m3.1) #sector healthcare positive
summary(m4.1) #not many individuals
summary(m6.1) #too many variables for the sample size


#here we check for variables that are significant in more than 2 models
variables <- as.data.frame(m1.1$coefficients)
variables$zvalues1 <-summary(m1.1)$coefficients[, "Pr(>|z|)"]
variables$zvalues2 <-summary(m2.1)$coefficients[, "Pr(>|z|)"]
variables$zvalues3 <-summary(m3.1)$coefficients[, "Pr(>|z|)"]
variables$zvalues4 <-summary(m4.1)$coefficients[, "Pr(>|z|)"]
variables <- variables[-1]
variables$sum <- rowSums(variables < 0.05)
variables <- variables[variables$sum >1,]
variables <- variables[rownames(variables) != "(Intercept)", ]
v <- rownames(variables)
v


#however we think that anova is more useful
names(numeric_financials_f1)
anova(aov(Cash_Flow_USD~clu_ward_4, data= numeric_financials_f1))
#now we perform anovas with all variables to check wether they are significant or not
variables1 <- colnames(numeric_financials_f1)
str(numeric_financials_f1)
variables1 <- setdiff(variables1, "clu_ward_4")
anova_pvalues <- sapply(variables1, function(var) {
  formula <- as.formula(paste(var, "~ clu_ward_4"))
  model <- aov(formula, data = numeric_financials_f1)
  summary(model)[[1]][["Pr(>F)"]][1]
})
anova_pvalues < 0.05
variables1 <- setdiff(variables1, "Cash_Flow_USD")
sort(anova_pvalues)
#all of thems except cash flow
# Clustering with only significant variables
summary(numeric_financials_f1)
significant_financials <- numeric_financials_f1[,variables1]
significant_financials_st <- as.data.frame(scale(significant_financials))


#outliers
significant_financials_st$sum <- rowSums(abs(significant_financials_st) > 4)
colSums(abs(significant_financials_st) > 4)
outliers <- significant_financials_st[significant_financials_st$sum > 1,]
table(significant_financials_st$sum)
significant_financials_st <- significant_financials_st[-5]




###############################################################
# Hierarchical method (ward) with significant variables only ##
###############################################################




set.seed(123)


# we perform Ward


fviz_nbclust(significant_financials_st, hcut, method = "wss",) +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


dist_financials.3 <- dist(significant_financials_st)
hclust_ward_f3 <- hclust(dist_financials.3, method = "ward.D2")
cut_ward4_f3 <- cutree(hclust_ward_f3, k = 4)
table(cut_ward4_f3)
table(numeric_financials_f1$clu_ward_4)
#exactly the same
p2 <- fviz_cluster(list(data = significant_financials_st, cluster = cut_ward4_f3),
                   ellipse.type = "convex", repel = FALSE,
                   show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "financials Clustering (Kmeans2)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
p2
plot_grid(p1, p2, ncol = 2)
dev.off()




clmethods <- c("hierarchical","kmeans")
significant_result <- clValid(significant_financials_st, nClust =4,clMethods = clmethods, validation = "internal")
# Summary
summary(significant_result) #we have better visualisations, but worse metrics
summary(definitive_result)


###############################################################
# Hierarchical and non hierarchical combination              ##
###############################################################


# Set the random seed for reproducibility
set.seed(123)


# Perform k-means clustering with 60 clusters on the filtered standardized financial data
clusters_mix <- kmeans(x = data_f1, centers = 60, nstart = 50, iter.max = 50)


# Display the sizes of each cluster
clusters_mix$size


# Display the number of iterations performed in the clustering algorithm
clusters_mix$iter


# Display the cluster centers
clusters_mix$centers


# Display the within-cluster sum of squares for each cluster
clusters_mix$withinss


# Display the between-cluster sum of squares
clusters_mix$betweenss


# Display the total sum of squares
clusters_mix$totss


# Display the total within-cluster sum of squares
clusters_mix$tot.withinss


# Convert the cluster centers to a data frame
financial_km <- as.data.frame(clusters_mix$centers)


# Calculate the pairwise distances between the cluster centers using the Euclidean distance
dist_mat_st <- dist(financial_km, method = "euclidean")


# Perform hierarchical clustering using the average linkage (Ward) method
hclust_avg_ward_mix <- hclust(dist_mat_st, method = "ward.D2")


# Plot the dendrogram of the hierarchical clustering
plot(hclust_avg_ward_mix, hang = -1, cex = 0.7)


# Highlight clusters by drawing rectangles around them
rect.hclust(hclust_avg_ward_mix, k = 7, border = 1:9)


# Visualize the dendrogram with ten clusters
fviz_dend(x = hclust_avg_ward_mix, k = 10, cex = 0.6) +
  geom_hline(yintercept = 2, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Euclidean distance, Ward, K = 10")


# Perform clustering using the hierarchical results and k-means centers
fviz_cluster(object = list(data = financial_km, cluster = cutree(hclust_avg_ward_mix, k = 4)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 12) +
  labs(title = "Hierarchical clustering + PCA projection",
       subtitle = "Euclidean distance, Ward, K = 4") +
  theme_bw() +
  theme(legend.position = "bottom")


# Cut the hierarchical clustering dendrogram at K = 4 and assign cluster labels
cut_avg_ward_mix <- cutree(hclust_avg_ward_mix, k = 4)


# Add the 'clu_ward_n' column with cluster labels to the financial_km data frame
financial_km$clu_ward_n <- factor(cut_avg_ward_mix)


# Display a summary of the 'clu_ward_n' column
summary(financial_km$clu_ward_n)


ward_for_obs <- cut_avg_ward_mix[clusters_mix$cluster]


financial <- as.data.frame(clusters_mix$cluster)
financial <- data.frame(cluster = factor(ward_for_obs))




# Combine the standardized financial data with the cluster assignments
financial_f_st_final <- cbind(data_f1, financial)


# Display a cross-tabulation of cluster labels for 'cluster'
table(financial_f_st_final$cluster)


# Calculate the mean values for each variable within each cluster
t1 <- financial_f_st_final %>%
  group_by(cluster) %>%
  summarise_all("mean")


# Reshape the data for plotting
cluster_pro_w <- t1 %>%
  pivot_longer(!cluster, names_to = "Variable", values_to = "z_values")


cluster_pro_w$cluster <- as.factor(cluster_pro_w$cluster)


# Plot the standardized profiles for each cluster
ggplot(cluster_pro_w, aes(x = Variable, y = z_values, group = cluster)) +
  geom_line(aes(color = cluster), linetype = "F1", size = 1) +
  geom_point(aes(color = cluster), size = 5) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "#993333", size = 8, angle = 45)) +
  labs(x = "Financial metrics",
       y = "Standardized values",
       colour = "Cluster - Ward method",
       title = "Cluster: standardized profiles")


financial_f_st_final$cluster1 <- ifelse(financial_f_st_final$cluster == 1, 1, 0)


mixed_model <- glm(cluster1 ~ Quarterly_Return_Pct  + Price_Volatility  + Return_on_Equity  + Liquidity_CashRatio  + Cash_Flow_USD  +
                     Net_Profit_USD + EPS_USD + PE_Ratio + PB_Ratio + MarketCapM,
                   data = financial_f_st_final, family = binomial(link = "logit"))
summary(mixed_model)#seems a better model
summary(m1)
table(financial_f_st_final$cluster) #it isn't fair as it is a really big cluster
table(numeric_financials_f1$clu_ward_4)
fviz_cluster(
  list(data = financial_f_st_final[,-11], cluster = financial_f_st_final$cluster),
  ellipse.type = "convex",
  repel = FALSE,
  show.clust.cent = FALSE,
  labelsize = 8
) +
  labs(title = "financials Clustering (k-means)",
       subtitle = "Euclidean + PCA") +
  theme_minimal()
#really worse clusters
financial_f_st_final$cluster <- as.numeric(financial_f_st_final$cluster)
evaluate_clusters <- function() {
  data <- financial_f_st_final[,-11]
  clusters <- financial_f_st_final$cluster
  dist_matrix <- dist(data, method = "euclidean")
  sil <- silhouette(clusters, dist_matrix)
  stats <- cluster.stats(d = dist_matrix, clustering = clusters)
  list(
    silhouette_avg = mean(sil[, 3]),
    dunn_index = stats$dunn,
    connectivity = stats$connectivity
  )
}
results <- evaluate_clusters()
print(results) #really worse
# Summary
summary(result_removing_too_many_outliers)
summary(significant_result)
summary(definitive_result)

# Save Figures
p_ward <- fviz_cluster(list(data = data_f1, cluster = cut_ward4.1),
                       ellipse.type = "convex", repel = FALSE,
                       show.clust.cent = FALSE, labelsize = 8) +
  labs(title = "Ward clustering (4 clusters)", subtitle = "Euclidean + PCA") +
  theme_minimal()
ggsave("figures/pca_ward_4clusters.png", p_ward, width = 8, height = 6, dpi = 300)

p_heat <- ggplot(df, aes(x = Kmeans, y = Hierarchical, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "white", size = 4) +
  scale_fill_gradient(low = "lightgreen", high = "red") +
  labs(title = "Hierarchical (Ward) vs K-means", x = "K-means cluster", y = "Ward cluster") +
  theme_minimal()
ggsave("figures/heatmap_ward_vs_kmeans.png", p_heat, width = 7, height = 5, dpi = 300)

t1 <- numeric_financials_3[, -12] %>% group_by(clu_ward_4) %>% summarise_all("mean")
prof <- t1 %>% pivot_longer(!clu_ward_4, names_to = "Variable", values_to = "z_values")
p_prof <- ggplot(prof, aes(x = Variable, y = z_values, group = clu_ward_4)) +
  geom_line(aes(color = clu_ward_4), linewidth = 1) +
  geom_point(aes(color = clu_ward_4), size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Financial metrics", y = "Standardized values",
       colour = "Cluster (Ward)", title = "Cluster standardized profiles")
ggsave("figures/cluster_profiles.png", p_prof, width = 9, height = 5, dpi = 300)

summary(definitive_result)
