library(tidyverse)
library(ggplot2)
library(Rtsne)
library(flexclust)
library(foreach)
library(caret)

set.seed(1)
wine = read.csv("../STA380/data/wine.csv") 
wine_d=wine[!duplicated(wine[, 1:11]), ]
wine_chems=wine_d[,c(1:11)]

wine_chems = scale(wine_chems, center=TRUE, scale=TRUE)	
# a look at the correlation matrix
cor(wine_chems)

# a quick heatmap visualization
ggcorrplot::ggcorrplot(cor(wine_chems))

# looks a mess -- reorder the variables by hierarchical clustering
ggcorrplot::ggcorrplot(cor(wine_chems), hc.order = TRUE)

# Perform t-SNE
tsne_result <- Rtsne(wine_chems, perplexity = 100, dims = 2, verbose = TRUE)

# Plot the t-SNE result
plot(tsne_result$Y, col = "blue", pch = 20, main = "t-SNE Result")

# create a tidy summary of the loadings
tSNEwine = tsne_result$Y %>%
  as.data.frame()

#Let's plot the wines with their colors
ggplot(tSNEwine, aes(x = V1, y = V2, color = wine_d$color)) +
  geom_point(size = 3) 

wine_c_data=tSNEwine[,c('V1','V2')]

# Run k-means with 2 clusters
clust1 = kmeans(wine_c_data, 2,nstart=25)
clust1$totss
clust1$betweenss
clust1$withinss
clust1$size

which.max(clust1$size)

#Since we have more white wines in the data set, let's name bigger cluster as white
wine_c_data$cc = ifelse(clust1$cluster==which.max(clust1$size),"white","red")

summary(clust1$cluster)
# Create the scatter plot
ggplot(wine_c_data, aes(x = V1, y = V2, color = cc)) +
  geom_point(size = 3)  +
  labs(title = "Wines with their predicted Color Categories", x = "V1", y = "V2")

confusionMatrix(as.factor(wine_c_data$cc),as.factor(wine_d$color))
