library(tidyverse)
library(igraph)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

# Association rule mining
# Adapted from code by Matt Taddy

# Read in playlists from users
# This is in "long" format -- every row is a single artist-listener pair
g_raw = read.csv("../data/groceries.txt",header=FALSE)
long_df <- g_raw %>%
  mutate(cart_id = row_number()) %>%
  pivot_longer(cols = starts_with("V"), names_to = "item_position", values_to = "item_name") %>%
  arrange(cart_id, as.numeric(gsub("V", "", item_position))) %>%
  select(-item_position)

x=long_df$item_name != ""
long_df<-subset(long_df,x)

## Cast this variable as a special arules "transactions" class.
playtrans = as(split(long_df$item_name, long_df$cart_id), "transactions")
summary(playtrans)

# Now run the 'apriori' algorithm
# Look at rules with support > .005 & confidence >.1 & length (# artists) <= 4
musicrules = apriori(playtrans, 
	parameter=list(support=.005, confidence=.1, maxlen=4))
     

# Look at the output... so many rules!
inspect(musicrules)

## Choose a subset
inspect(subset(musicrules, subset=lift > 5))
inspect(subset(musicrules, subset=confidence > 0.6))
inspect(subset(musicrules, subset=lift > 10 & confidence > 0.5))

# plot all the rules in (support, confidence) space
# notice that high lift rules tend to have low support
plot(musicrules)

# can swap the axes and color scales
plot(musicrules, measure = c("support", "lift"), shading = "confidence")

# "two key" plot: coloring is by size (order) of item set
plot(musicrules, method='two-key plot')

# can now look at subsets driven by the plot
inspect(subset(musicrules, support > 0.035))
inspect(subset(musicrules, confidence > 0.6))
inspect(subset(musicrules, lift > 20))


# graph-based visualization
# export
# associations are represented as edges
# For rules, each item in the LHS is connected
# with a directed edge to the item in the RHS. 
playlists_graph = associations2igraph(subset(musicrules, lift>1), associationsAsNodes = FALSE)
igraph::write_graph(playlists_graph, file='groceries.graphml', format = "graphml")
