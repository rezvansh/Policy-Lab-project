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
c.graph = graph_from_adjacency_matrix(as.matrix(netwk.full), mode="undirected")

#### Other attributes
attributes = read.csv("Complaints_Attributes.csv")

lo = layout.norm(as.matrix(geo.full[,2:3]))

#Plot Network
plot(c.graph, layout = lo, vertex.label = NA, vertex.size = 0.5, xlim = c(-1,0.55), ylim = c(-0.25, 0.25))

#Add attributes
V(c.graph)$name

#Category
V(c.graph)$Category=as.character(attributes$Category[match(V(c.graph)$name,attributes$CRID)])
V(c.graph)$Category
cats = as.factor(V(c.graph)$Category)
cat.colors = rainbow(length(unique(V(c.graph)$Category)))
V(c.graph)$color = cat.colors[cats]
pdf("Category2.pdf", width =10, height = 10)
plot(c.graph, vertex.color = V(c.graph)$color, layout = lo, vertex.label = NA, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5),  margin = 0.5, vertex.frame.color = "black", edge.width = 0.05, edge.color = "gray30", vertex.size = 2.5)
#Removed blank category from levels(cats) and added "Other"
legend(.5,1, legend = c("Other", levels(cats)[-1]),fill= cat.colors, bty = "n", cex = .75)
dev.off()
