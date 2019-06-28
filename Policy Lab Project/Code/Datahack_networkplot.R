
######### Load Binary complaints network 
setwd(~/Desktop)
library(igraph)
netwk = read.table("Complaints_AdjMatrix_Binary.txt")
newedges<-read.table("Coordinates_Edges.csv", header = TRUE)
colnames(newedges)=c("Lat1", "Lat2", "Lat3", "Lat4")
#newedges=as.data.frame(newedges)
c.graph = graph_from_adjacency_matrix(as.matrix(netwk), mode="undirected")

######### Readin coordinates
geo = read.table("Complaints_GeoCoordinates.txt", header=T)

######### Remove missing
missing = which(is.na(geo[,2]) | geo[,2]>42.1 | geo[,2]<41.5)
geo.full = geo[-missing,]
netwk = as.matrix(netwk)
netwk.full = netwk[-missing, -missing]
colnames(netwk.full) = row.names(netwk.full)
c.graph = graph_from_adjacency_matrix(as.matrix(netwk.full), mode="undirected")

#### Other attributes
attributes = read.csv("Complaints_Attributes.csv")

lo = layout.norm(as.matrix(geo.full[,2:3]))

#Plot Network
plot<- plot(c.graph, layout = lo, vertex.label = NA, vertex.size = 0.5, xlim = c(-1,0.55), ylim = c(-0.25, 0.25))


#map policy misconduct locations
#geocomplaints = as.list(geocomplaints)
#attach(geocomplaints)
events <- get_map(location = c(-87.6298, 41.8781), source="google", zoom=10)
#events is background map
ggmap(events) + geom_point(aes(x=geo.full$Longitude, y=geo.full$Latitude), data=geo.full, fill="red", colour="black", pch=46, alpha=1)
#zoom in to smaller areas so you can see detail
points(c.graph)
?geom_segment

ggmap(events) + geom_segment(aes(x=Lat1, y=Lat2, xend = Lat3, yend = Lat4), data=newedges, size = 0.5) + 
  geom_point(aes(x=geo.full$Longitude, y=geo.full$Latitude), data=geo.full, fill="red", colour="black", pch=46, alpha=1)
