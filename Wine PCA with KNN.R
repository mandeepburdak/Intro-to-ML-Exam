library(tidyverse)
library(ggplot2)
wine = read.csv("../data/wine.csv") 
wine_chems=wine[,c(1:11)]
wine_chems = scale(wine_chems, center=TRUE, scale=TRUE)	
# a look at the correlation matrix
cor(wine_chems)

# a quick heatmap visualization
ggcorrplot::ggcorrplot(cor(wine_chems))

# looks a mess -- reorder the variables by hierarchical clustering
ggcorrplot::ggcorrplot(cor(wine_chems), hc.order = TRUE)

# Now look at PCA of the (average) survey responses.  
# This is a common way to treat survey data
PCAwine = prcomp(wine_chems, scale=FALSE,rank. = 2)

## variance plot
plot(PCAwine)
summary(PCAwine)

# first few pcs
# try interpreting the loadings
# the question to ask is: "which variables does this load heavily on (positive and negatively)?"
round(PCAwine$rotation[,1:2],2) 

# create a tidy summary of the loadings
loadings_summary = PCAwine$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Question')

# This seems to pick out characteristics of
# sulfury vs vinegary wines?
loadings_summary %>%
  select(Question, PC1) %>%
  arrange(desc(PC1))

# this just seems to load positively on alcohol
# which is negatively correlated with density
loadings_summary %>%
  select(Question, PC2) %>%
  arrange(desc(PC2))

wine = merge(wine, PCApilot$x[,1:2], by="row.names")
wine = rename(wine, Show = Row.names)

#Let's plot the wines with their colors
ggplot(wine, aes(x = PC1, y = PC2, color = color)) +
  geom_point(size = 3) 

wine_c_data=wine[,c('PC1','PC2')]

library(flexclust)
library(foreach)

# Load, center and scale the data
# cars = read.csv('../data/cars.csv', header=TRUE)
# X = cars[,-(1:9)]
# X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(wine_c_data,"scaled:center")
sigma = attr(wine_c_data,"scaled:scale")

# Run k-means with 6 clusters and 25 re-starts
clust1 = kmeans(wine_c_data, 2, nstart=1)
clust1$totss
clust1$betweenss
clust1$withinss
clust1$size

# Load the required library
library(ggplot2)

#Since we have more white wines in the data set, let's name bigger cluster as white
wine_c_data$cc = ifelse(clust1$cluster<=1,"white","red")

summary(clust1$cluster)
# Create the scatter plot
ggplot(wine_c_data, aes(x = PC1, y = PC2, color = cc)) +
  geom_point(size = 3)  +
  labs(title = "Wines with their predicted Color Categories", x = "PC1", y = "PC2")

library(caret)
confusionMatrix(as.factor(wine_c_data$cc),as.factor(wine$color))
