
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

######## Create weighted network
weighted.c.graph = graph_from_adjacency_matrix(as.matrix(netwk.full), mode="undirected", weighted = T)

####### Detect community structure using walktrap algorithm
cluster_weighted = cluster_walktrap(weighted.c.graph, weights = E(weighted.c.graph)$weight)

###### This assigns 
community = (membership(cluster_weighted))
size.com = table(community)
community.id = names(size.com)
small.com = community.id[size.com<10]
merged = community
merged[community %in% small.com] = 139

library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

toy = read.csv("toy.complaint_data.csv")
tab.crid = table(toy[,1])
NumOfficers = as.numeric(tab.crid[match(geo.full[,1], names(tab.crid))])

edges = read.table("Coordinates_Edges.csv")
colnames(edges) = c("X1","Y1","X2","Y2")
library(ggplot2)
library(ggmap)
edges = edges[edges$X1!=0,]

events <- get_map(location = c(-87.6298, 41.8781), source="google", zoom=10, color="bw")
#events is background map

#ggplot(data=geo.full, aes(x=Longitude, y=Latitude)) + 
#geom_segment(data=edges, aes(x=X1, y=Y1, xend=X2, yend=Y2)) + geom_point(aes(x=geo.full$Longitude, y=geo.full$Latitude), data=geo.full, fill="red")

#geom_point() +
#geom_segment(data=edges, aes(x=X1, y=Y1, xend=X2, yend=Y2))

p = ggmap(events)
E(c.graph)$weight = 1
c.graph = simplify(c.graph)
 
yrange=ggplot_build(p)$panel$ranges[[1]]$y.range
xrange=ggplot_build(p)$panel$ranges[[1]]$x.range 
 
V(c.graph)$lon = geo.full$Longitude
V(c.graph)$lat = geo.full$Latitude 
G = c.graph 
G$layout = cbind(V(G)$lon,V(G)$lat)
E(G)$color = rgb(0,0,0,E(G)$weight/30)
Community = as.character(merged)
N = length(unique(Community))
name.com = unique(Community)
pdf("Geo_Community.pdf")
for (i in 1:N){
  ComID = ifelse(Community==name.com[i], 0, 1)
  alpha = ifelse(Community==name.com[i], 1, 0.3)
  plot_vector<- as.data.frame(cbind(V(G)$lon,V(G)$lat, ComID, alpha, NumOfficers))
  print(p +  geom_point(aes(x=V1,y=V2, color=as.factor(ComID), alpha=alpha,  size=log(NumOfficers)),plot_vector))
  cat(sprintf("\r%d", i))
}
dev.off()

E(c.graph)$weight = 1
c.graph = simplify(c.graph)

yrange=ggplot_build(p)$panel$ranges[[1]]$y.range 
xrange=ggplot_build(p)$panel$ranges[[1]]$x.range

V(c.graph)$lon = geo.full$Longitude
V(c.graph)$lat = geo.full$Latitude

#w = which(V(c.graph)$lon>yrange[1] & V(c.graph)$lon<yrange[2] 
# & V(c.graph)$lat>xrange[1] & V(c.graph)$lat<xrange[2])

#G=induced_subgraph(c.graph,w)
G = c.graph
G$layout = cbind(V(G)$lon,V(G)$lat)
E(G)$color = rgb(0,0,0,E(G)$weight/30)

plot_vector<- as.data.frame(cbind(V(G)$lon,V(G)$lat))
edgelist <- get.edgelist(G)
edgelist[,1]<-as.numeric(match(edgelist[,1],V(G)$name))
edgelist[,2]<-as.numeric(match(edgelist[,2],V(G)$name))


edges <- data.frame(plot_vector[edgelist[,1],], plot_vector[edgelist[,2],],E(G)$color)
colnames(edges) <- c("X1","Y1","X2","Y2","Color")

pdf("Layered_Map.pdf")
p + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), data=edges, size = 0.5, colour=edges$Color) + geom_point(aes(V1, V2), data=plot_vector)+ geom_point(aes(x=V1,y=V2),plot_vector,size=0.01,alpha=0.5,color="yellow")
dev.off()

########### Stratify by attibutes

toy = read.csv("toy.complaint_data.csv")
toy = toy[match(geo.full[,1], toy[,1]),]
library(stringr)
temp = str_split_fixed(toy$incident_date,"  ",2)
date = str_split_fixed(temp[,1], "-", 3)
time = str_split_fixed(temp[,2], ":", 2)
year = date[,3]
hour = time[,1]

### stratify by year
#name.year = unique(year)
name.year = as.character(c(2011:2015))
n.year = length(name.year)
out.dir = "ByYear"
out.name = paste(out.dir, "/", name.year, ".pdf", sep="")
pdf("ByYear.pdf")
for (i in 1:n.year){
  temp.index = which(year==name.year[i])
  G=induced_subgraph(c.graph,temp.index)
  G$layout = cbind(V(G)$lon,V(G)$lat)
  E(G)$color = rgb(0,0,0,E(G)$weight/30)

  plot_vector<- as.data.frame(cbind(V(G)$lon,V(G)$lat))
  edgelist <- get.edgelist(G)
  edgelist[,1]<-as.numeric(match(edgelist[,1],V(G)$name))
  edgelist[,2]<-as.numeric(match(edgelist[,2],V(G)$name))
 
  edges <- data.frame(plot_vector[edgelist[,1],], plot_vector[edgelist[,2],], rep("red", nrow(edgelist)))
  colnames(edges) <- c("X1","Y1","X2","Y2","Color")
#  pdf(file=out.name[i])
  print(p + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), data=edges, size = 0.5, colour=edges$Color) + geom_point(aes(V1, V2), data=plot_vector) + 
  ggtitle( paste("Year " ,name.year[i], sep="") ) )
#  dev.off()
}
dev.off()





#### stratify by hour
name.hour = as.character(0:23)
index.biased = which(hour==0 & time[,2]=="00")
index.left = c(1:nrow(geo.full))[-index.biased]
t.graph = induced_subgraph(c.graph,index.left)
hour = hour[index.left]
n.hour = length(name.hour)
out.dir = "ByHour"
out.name = paste(out.dir, "/", name.hour, ".pdf", sep="")
pdf("ByHour.pdf")
for (i in 1:n.hour){
  temp.index = which(hour==name.hour[i])
  G=induced_subgraph(t.graph,temp.index)
  G$layout = cbind(V(G)$lon,V(G)$lat)
  E(G)$color = rgb(0,0,0,E(G)$weight/30)

  plot_vector<- as.data.frame(cbind(V(G)$lon,V(G)$lat))
  edgelist <- get.edgelist(G)
  edgelist[,1]<-as.numeric(match(edgelist[,1],V(G)$name))
  edgelist[,2]<-as.numeric(match(edgelist[,2],V(G)$name))

  edges <- data.frame(plot_vector[edgelist[,1],], plot_vector[edgelist[,2],], rep("red", nrow(edgelist)))
  colnames(edges) <- c("X1","Y1","X2","Y2","Color")
#  pdf(file=out.name[i])
  print(p + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), data=edges, size = 0.5, colour=edges$Color) + geom_point(aes(V1, V2), data=plot_vector)+ geom_point(aes(x=V1,y=V2),plot_vector,size=0.01,alpha=0.5,color="yellow") +
  ggtitle( paste("Hour " ,name.hour[i], sep="") ) )
#  dev.off()

}
dev.off()






