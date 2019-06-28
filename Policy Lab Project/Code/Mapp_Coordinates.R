
main = read.csv("AllComplaints_WithGeoCoordinates.csv")
data.of = read.csv("toy.officer_data.csv")
data.cp = read.csv("toy.complaint_data.csv")

main.sub = main[ main[,1] %in% data.cp[,1] ,]
get_missing = function(x){mean(is.na(x) | x=="")}
get_missing(main.sub$Latitude)

write.csv(main.sub, "Complaints_WithGeoCoordinates.csv")

CRID = unique(intersect(data.cp[,1], main[,1]))
###
jaccard.index = function(x,y){length(intersect(x,y))/length(union(x,y))}
n = length(CRID)
coordi = main.sub[match(CRID, main.sub[,1]),c("Latitude","Longitude")]
coord.mat = data.frame(CRID, coordi)
write.table(coord.mat, "Complaints_GeoCoordinates.txt", quote=F, col.names=T, row.names=F, sep="\t")

adj.mat = matrix(0, ncol=n, nrow=n)
officer.list = list()
for (i in 1:n){
  officer.list[[i]] = unique( data.cp$officer_id[data.cp$crid==CRID[i]] )
  cat(sprintf("\r%d", i))
}
for (i in 1:(n-1)){
  for (j in (i+1):n ){
    adj.mat[i,j] = jaccard.index(officer.list[[i]], officer.list[[j]])
  }
  cat(sprintf("\r%d", i))
}
adj.mat[lower.tri(adj.mat)] = adj.mat[upper.tri(adj.mat)]
colnames(adj.mat) = CRID
rownames(adj.mat) = CRID

binary.mat = adj.mat
binary.mat[adj.mat!=0] = 1

write.table(adj.mat, "Complaints_AdjMatrix_Jaccard.txt", quote=F, col.names=T, row.names=T)
write.table(adj.mat, "Complaints_AdjMatrix_Binary.txt", quote=F, col.names=T, row.names=T)

library(igraph)
c.graph = graph_from_adjacency_matrix(adj.mat)


####################################
library(stringr)
library(ggplot2)
time = data.cp$incident_date
time.mat = str_split_fixed(time,"  ",2)
date = time.mat[,1]
time = time.mat[,2]
date.mat = str_split_fixed(date, "-", 3)
day = date.mat[,1]
month = date.mat[,2]
year = date.mat[,3]
hour = str_split_fixed(time, ":", 2)[,1]
hist(as.numeric(hour), breaks=24)
data.cp$month = month
data.cp$year = year


table.categ = table(data.cp$complaintcategory)
table.order = table.categ[order(table.categ)]

##### circular plot
library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

circular_plot = function(myhour){
  angles = seq(from=0, to=345, by=15)
  hours = 0:23
  table.hr = table(myhour)
  frequency = table.hr[match(hours, names(table.hr))]
  hour_circular = data.frame(Angle=angles, Number_Complaints=frequency , Hours=hours)
  rownames(hour_circular) = hours

  ggplot(hour_circular, aes(x = Angle, y = Number_Complaints, label=Hours)) + 
  coord_polar(theta = "x", start = -pi/24) + geom_bar(stat = "identity", fill=rep(c("red","darkblue"), each=12) ) + 
  scale_x_continuous(breaks = seq(0, 360, 15)) +
  geom_text(aes(label=Hours, y=(max(Number_Complaints)+100))) + 
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank())
}


subhour = hour[-which(hour==0 & time=="0:00")]
pdf("Complaints_Breakdown_Time.pdf")
circular_plot(hour)
dev.off()

pdf("Complaints_Breakdown_Time_Without0000.pdf")
circular_plot(subhour)
dev.off()


pdf("Complaints_Breakdown_Year.pdf", width=10)
ggplot(data.cp, aes(x=year, fill=Category)) + geom_histogram(stat="count")
dev.off()



