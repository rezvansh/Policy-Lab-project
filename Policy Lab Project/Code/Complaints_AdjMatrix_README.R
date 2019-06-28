
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
c.graph = graph_from_adjacency_matrix(as.matrix(netwk.full), mode="undirected")

#### Other attributes
attributes = read.csv("Complaints_Attributes.csv")


