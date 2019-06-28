setwd("/Users/mcanderson92/Desktop/Datahack@Yale2017")
######### Load Binary complaints network 
library(igraph)
netwk = read.table("Complaints_AdjMatrix_Binary.txt")
c.graph = graph_from_adjacency_matrix(as.matrix(netwk), mode="undirected")

######### Readin coordinates
geo = read.table("Complaints_GeoCoordinates.txt", header=T)

######### Remove missing
missing = which(is.na(geo[,2]) | geo[,2]>42.1 | geo[,2]<41.5)
geo.full = geo[-missing,]
netwk = as.matrix(netwk)
netwk.full = netwk[-missing, -missing]
colnames(netwk.full) = row.names(netwk.full)

######## Create weighted network
weighted.c.graph = graph_from_adjacency_matrix(as.matrix(netwk.full), mode="undirected", weighted = T)

####### Detect community structure using walktrap algorithm
cluster_weighted = cluster_walktrap(weighted.c.graph, weights = E(weighted.c.graph)$weight)

###### This assigns 
community = (membership(cluster_weighted))

##### This gives you the total # of communities 
length(sizes(cluster_weighted))