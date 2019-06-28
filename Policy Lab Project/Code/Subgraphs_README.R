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
attributes = read.csv("MyData.csv")

lo = layout.norm(as.matrix(geo.full[,2:3]))

#Plot Network
plot(c.graph, layout = lo, vertex.label = NA, vertex.size = 0.5, xlim = c(-1,0.55), ylim = c(-0.25, 0.25))

#Add attributes
V(c.graph)$name
V(c.graph)$Year=as.numeric(attributes$year[match(V(c.graph)$name,attributes$CRID)])
V(c.graph)$Year
cats = as.factor(V(c.graph)$Year)
cat.colors = rainbow(length(unique(V(c.graph)$Year)))
V(c.graph)$color = cat.colors[cats]
pdf("AllYears.pdf", width =10, height = 10)
plot(c.graph, vertex.color = V(c.graph)$color, layout = lo, vertex.label = NA, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5),  margin = 1, vertex.frame.color = "black", edge.width = 0.05, edge.color = "gray30", vertex.size = 2.5)
legend(-1.95,-.95, legend = levels(cats),fill= cat.colors, bty = "n", ncol = 6, cex = .75)
#Removed blank category from levels(cats) and added "Other"
dev.off()

plot(V(c.graph)$Year=="2012", vertex.color = V(c.graph)$color, layout = lo, vertex.label = NA, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5),  margin = 1, vertex.frame.color = "black", edge.width = 0.05, edge.color = "gray30", vertex.size = 2.5)


V(c.graph)$Year[1]

subset2011 = c()
subset2012 = c()
subset2013 = c()
subset2014 = c()
subset2015 = c()


for(i in 1:length(V(c.graph)$Year)){
  if (V(c.graph)$Year[i] == 2011){
    #print(V(c.graph)$name[i])
    subset2011 <- c(subset2011,V(c.graph)$name[i])
  }
  
}

geo2011 = geo[-missing,]
geo2011_lay = subset(geo2011, geo2011[,1] %in% as.numeric(subset2011))
lo2011 = layout.norm(as.matrix(geo2011_lay[,2:3]))
graph2011 = induced_subgraph(c.graph, subset2011)
vcount(graph2011)

png("2011_graph_transparent.png",bg="transparent")
plot(graph2011, layout = lo2011, vertex.label = NA, vertex.size = 2, vertex.color = "blue")
dev.off()

for(i in 1:length(V(c.graph)$Year)){
  if (V(c.graph)$Year[i] == 2012){
    #print(V(c.graph)$name[i])
    subset2012 <- c(subset2012,V(c.graph)$name[i])
  }
  
}

geo2012 = geo[-missing,]
geo2012_lay = subset(geo2012, geo2012[,1] %in% as.numeric(subset2012))
lo2012 = layout.norm(as.matrix(geo2012_lay[,2:3]))
graph2012 = induced_subgraph(c.graph, subset2012)
vcount(graph2012)
png("2012_graph_transparent.png",bg="transparent")
plot(graph2012, layout = lo2012, vertex.label = NA, vertex.size = 2, vertex.color = "purple")
dev.off()

for(i in 1:length(V(c.graph)$Year)){
  if (V(c.graph)$Year[i] == 2013){
    #print(V(c.graph)$name[i])
    subset2013 <- c(subset2013,V(c.graph)$name[i])
  }
  
}

geo2013 = geo[-missing,]
geo2013_lay = subset(geo2013, geo2013[,1] %in% as.numeric(subset2013))
lo2013 = layout.norm(as.matrix(geo2013_lay[,2:3]))
graph2013 = induced_subgraph(c.graph, subset2013)
vcount(graph2013)
png("2013_graph_transparent.png",bg="transparent")
plot(graph2013, layout = lo2013, vertex.label = NA, vertex.size = 2, vertex.color = "green")
dev.off()

for(i in 1:length(V(c.graph)$Year)){
  if (V(c.graph)$Year[i] == 2014){
    #print(V(c.graph)$name[i])
    subset2014 <- c(subset2014,V(c.graph)$name[i])
  }
  
}

geo2014 = geo[-missing,]
geo2014_lay = subset(geo2014, geo2014[,1] %in% as.numeric(subset2014))
lo2014 = layout.norm(as.matrix(geo2014_lay[,2:3]))
graph2014 = induced_subgraph(c.graph, subset2014)
vcount(graph2014)
png("2014_graph_transparent.png",bg="transparent")
plot(graph2014, layout = lo2014, vertex.label = NA, vertex.size = 2, vertex.color = "yellow")
dev.off()

for(i in 1:length(V(c.graph)$Year)){
  if (V(c.graph)$Year[i] == 2015){
    #print(V(c.graph)$name[i])
    subset2015 <- c(subset2015,V(c.graph)$name[i])
  }
  
}

geo2015 = geo[-missing,]
geo2015_lay = subset(geo2015, geo2015[,1] %in% as.numeric(subset2015))
lo2015 = layout.norm(as.matrix(geo2015_lay[,2:3]))
graph2015 = induced_subgraph(c.graph, subset2015)
vcount(graph2015)
png("2015_graph_transparent.png",bg="transparent")
plot(graph2015, layout = lo2015, vertex.label = NA, vertex.size = 2, vertex.color = "orange")
dev.off()

saveGIF( {  col <- rep("grey40", vcount(c.graph))
plot(c.graph, vertex.color="blue", layout=lo)

step.1 <- V(c.graph)$Year=="2011"
col[step.1] <- "#ff5100"
plot(c.graph, vertex.color="green", layout=lo)

step.2 <- V(c.graph)$Year=="2012"
col[setdiff(step.2, step.1)] <- "#ff9d00"
plot(c.graph, vertex.color="yellow", layout=lo) 

step.3 <- V(c.graph)$Year=="2013"
col[setdiff(step.3, step.2)] <- "#FFDD1F"
plot(c.graph, vertex.color="purple", layout=lo)  },
interval = .8, movie.name="network_animation.gif" )


