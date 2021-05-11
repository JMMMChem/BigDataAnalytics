###
# ASSIGNMENT NETWORK ANALYSIS
# MOVIE "CASINO" BY MARTIN SCORSESE (FUCKIN BOSS)
###

###################################################################
# Path, libraries, upload graph
###################################################################

## SET PATH

#setwd("~/5 MASTER BIG DATA ANALYTICS/2B Network analysis and data visualization")
setwd("C:/Users/silvi/Documents/3.-Máster/IV/3.- Network analysis and data visualization/A1")


## Libraries

#install.packages("rgexf")
library("rgexf")
library("igraphdata")
library("igraph")
library("CINNA")
library("dplyr")


## UPLOAD GRAPH

#graph <- read.gexf("C:/Users/Jose Maria/Dropbox/Mi PC (DESKTOP-26FICGE)/Documents/5 MASTER BIG DATA ANALYTICS/2B Network analysis and data visualization/CASINO.gexf")
graph1 <- read.gexf("CASINO.gexf")

# convert gexf to igraph
graph <- gexf.to.igraph(graph1)

# Names 
names(graph1)
names(graph) # null

# Summary
summary(graph1)  # #nodes=109, #edges=326
summary(graph)


###################################################################
# 1. Discuss the definition (node/link types, direction, weights, etc.) 
# and the most relevant features of your network
#       small-world property
#       community structure
#       most important nodes
# 3. Compute the assortativity Pearson coefficient and 
# interpret its value
###################################################################

##Get the nodes and links 

nodes <- graph1$nodes  # id, label
links <- graph1$edges  #id, source, target, weight
links <- links[,2:4] #remove id column

# count how many
count(nodes)  # 109
count(links)  # 326

# see how it looks like
head(nodes,5) # id, label
head(links,5) # source, target, weight

# We already have the id column in the nodes, so we do not need to create one
# to match the 'character' names with a recognizable number

#nodes <- data.frame(id=seq(0,nrow(nodes)-1),nodes)

# Create data frame of data to plot the grap
gCasino <- graph.data.frame(links,vertices=nodes,directed=F)
plot(gCasino)


#############
## DEGREE
#############

mean(degree(graph)) # 5.981651 = 2*326/109 =2*N/L
# <k>=6 <<< N=109
# the network is sparse

# get most connected nodes
V(graph)[degree(graph)>30] 
# ace, nicky

# order nodes wrt their degree
dg <- data.frame(name=V(graph)$name,degree=degree(graph)) 
head(dg[order(dg$degree,decreasing=T),], 5)



####################################################
## CLUSTERING COEFFICIENT (TRANSITIVITY)
####################################################

transitivity(graph) # 0.1620399 
# high value => clustered network

## DIAMETER
# largest eccentricity = length of the longest path
diameter(graph) # 6


#######################################
## AVERAGE SHORTEST PATH LENGTH
#######################################

mean_distance(graph) # 2.059803
# small compared to the network size => small-world property holds


#########################################
## ASSORTATIVITY COEFFICIENT (PEARSON)
#########################################

assortativity_degree(graph)


#########################################
## CENTRALITY MEASURES
#########################################

bt <- betweenness(graph)
head(sort(bt, decreasing = TRUE),5) #show top 5 values

ebt <- edge.betweenness(graph)
head(sort(ebt, decreasing = TRUE),5)

cl <- closeness(graph)
head(sort(cl, decreasing = TRUE),5) 

# similar interpretation to closeness
hc <- harmonic_centrality(graph)
head(sort(hc, decreasing = TRUE),5)

pr <- page_rank(graph)
head(sort(pr$vector, decreasing = TRUE),5)

#########################################
# COMMUNITIES
#########################################

# Using Louvain algorithm
communities <- cluster_louvain(as.undirected(graph))

modularity(communities)

sizes(communities)

max(sizes(communities))
which(sizes(communities)==max(sizes(communities)))

# change number to see different communities
communities[1]


######
## CONNECTEDNESS
######
is.connected(graph)
no.clusters(graph) # number of connected components
cc <- clusters(graph) # information about the connected components (cc)
cc$membership
cc$csize
cc$no

## GIANT CONNECTED COMPONENT

cc <- clusters(graph) # clustering membership
gGC <- induced.subgraph(graph,vids = which(cc$membership==which.max(cc$csize))) # giant component

cc <- clusters(gCasino)
cc$csize
gCasinoGC <- induced_subgraph(gCasino,which(cc$membership==which.max(cc$csize)))
is.connected(gCasinoGC)

###################################################################
# 2. Plot the degree distribution and discuss its behavior;
# does your network show the friendship paradox?  
###################################################################

## DEGREE DISTRIBUTION

plot(degree_distribution(graph,cumulative=F),type="s") # degree distribution
plot(degree_distribution(graph,cumulative=T),type="s") # cumulative degree distribution

# poisson like
# right-skewed
# friendship paradox holds


###################################################################
# 4. Create an ER random graph with the same number of nodes and links
# and comment its statistics with respect to those of your chosen network
###################################################################

# ER
g <- erdos.renyi.game(109,326, type ="gnm")

#DEGREE
mean(degree(g))  # 5.981651

## DIAMETER
# largest eccentricity = length of the longest path
diameter(g) # 5

## ASSORTATIVITY COEFFICIENT (PEARSON)
assortativity_degree(g)  # around 0

## CLUSTERING COEFFICIENT (TRANSITIVITY)
transitivity(g)  # 0.05
# smaller than before (0.16)

## AVERAGE SHORTEST PATH LENGTH
mean_distance(g)  # 2.80
# small compared to the network size => small-world property holds


## DEGREE DISTRIBUTION

plot(degree_distribution(g,cumulative=F),type="s") # degree distribution
plot(degree_distribution(g,cumulative=T),type="s") # cumulative degree distribution


## CONNECTEDNESS
is.connected(g)
no.clusters(g) # number of connected components
cc <- clusters(g) # information about the connected components (cc)
cc$membership
cc$csize
cc$no

## GIANT CONNECTED COMPONENT
cc <- clusters(g)
gGC <- induced.subgraph(g,vids = which(cc$membership==which.max(cc$csize)))


## CENTRALITIES
bt <- betweenness(g)
ebt <- edge.betweenness(g)
cl <- closeness(g)
hc <- harmonic_centrality(g)
pr <- page_rank(g)
head(pr$vector)


###################################################################
# 5. Create a configuration model network with the same degree distribution of 
# your chosen network and comment its statistics wrt those of your network 
###################################################################

g <-sample_degseq(degree(graph))

#DEGREE
mean(degree(g))  # 5.981651

## DIAMETER
diameter(g) # 6

## ASSORTATIVITY COEFFICIENT (PEARSON)
assortativity_degree(g) # -0.08347003
# neutral

## AVERAGE SHORTEST PATH LENGTH
mean_distance(g) # 2.67669 
# small <<109, small-world property holds

## CLUSTERING COEFFICIENT (TRANSITIVITY)
transitivity(g) #  0.1068082
# similar to original graph => clustered nodes
# we apply algorithm to detect communities

# Louvain
communities <- cluster_louvain(as.undirected(g))
modularity(communities)
sizes(communities)


## DEGREE DISTRIBUTION
plot(degree_distribution(g,cumulative=F),type="s", 
     main="Degree distribution of the CNM",
     ylab = "Degree distribution") # degree distribution
plot(degree_distribution(g,cumulative=T),type="s",
     main="Cumulative degree distribution of the CNM",
     ylab = "Cumulative degree distribution") # cumulative degree distribution






#####
# Barabasi
#####

g <- barabasi.game(109,directed=F)


#DEGREE
mean(degree(g)) #1.98
# smaller comparing

## DIAMETER
diameter(g) # 12
# larger comparing


## ASSORTATIVITY COEFFICIENT (PEARSON)
assortativity_degree(g)
# -0.188, disass.


## CLUSTERING COEFFICIENT (TRANSITIVITY)
transitivity(g)
# 0, not clustered network

## AVERAGE SHORTEST PATH LENGTH
mean_distance(g)
# 5.25, larger than original

## DEGREE DISTRIBUTION
plot(degree_distribution(g,cumulative=F),type="s", 
     main="Degree distribution of the WS",
     ylab = "Degree distribution") # degree distribution
plot(degree_distribution(g,cumulative=T),type="s",
     main="Cumulative degree distribution of the WS",
     ylab = "Cumulative degree distribution") # cumulative degree distribution

# poisson-like


# Louvain
communities <- cluster_louvain(as.undirected(g))
modularity(communities)
sizes(communities)
# 12, community structure
# more than in original

#####
# Watts-Strogatz
#####

g <- watts.strogatz.game(1,109,6,0.5)

#DEGREE
mean(degree(g)) #12
# too high comparing

## DIAMETER
diameter(g) # 3
# too small comparing


## ASSORTATIVITY COEFFICIENT (PEARSON)
assortativity_degree(g)
# neutral instead of dis.


## CLUSTERING COEFFICIENT (TRANSITIVITY)
transitivity(g)
# 0.10, quite high, clustered network

## AVERAGE SHORTEST PATH LENGTH
mean_distance(g)
# 2.12, similar to original

## DEGREE DISTRIBUTION
plot(degree_distribution(g,cumulative=F),type="s", 
     main="Degree distribution of the WS",
     ylab = "Degree distribution") # degree distribution
plot(degree_distribution(g,cumulative=T),type="s",
     main="Cumulative degree distribution of the WS",
     ylab = "Cumulative degree distribution") # cumulative degree distribution

# not poisson


# Louvain
communities <- cluster_louvain(as.undirected(g))
modularity(communities)
sizes(communities)
# 7, similar to original