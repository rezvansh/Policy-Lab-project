
x = read.csv("AllComplaints_WithGeoCoordinates.csv")
officer = read.csv("toy.officer_data.csv")
officer.ID = unique(officer$officer_id)
n.officer = length(officer.ID)
comp = read.csv("toy.complaint_data.csv")
comp.ID = unique(comp[,1])
n.comp = length(comp.ID)

#officer.pairs = combn( officer.ID, m=2  )
#officer.comp.share = rep(0, ncol(officer.pairs))

######## pca analysis

mat = matrix(0, ncol=n.officer, nrow=n.comp)
for (i in 1:n.comp){
  of.id = comp$officer_id[comp[,1] == comp.ID[i]]
  mat[i, of.id] = 1
}




officer.netwk = matrix(0, ncol=n.officer, nrow=n.officer)
for (i in 1:n.comp){
  of.id = comp$officer_id[comp[,1] == comp.ID[i]]
  k = length(of.id)
  if (k>1){
    of.index = match(of.id, officer.ID)
    of.pairs = combn(x=of.index, m=2)
    n.pairs = ncol(of.pairs)
    for (ip in 1:n.pairs){
       officer.netwk[ of.pairs[1,ip], of.pairs[2, ip] ] = officer.netwk[ of.pairs[1,ip], of.pairs[2, ip] ] + 1
    }
  }
  cat(sprintf("\r%d",i))
}


############# basic patterns
### number of complaints 
num.comp = table(comp$officer_id)

toy = read.csv("toy.complaint_data.csv")

toy = toy[match(geo.full[,1], toy[,1]),]

library(stringr)

temp = str_split_fixed(toy$incident_date,"  ",2)

date = str_split_fixed(temp[,1], "-", 3)

time = str_split_fixed(temp[,2], ":", 2)

year = date[,3]

hour = time[,1]

appt.year = str_split_fixed(officer$appointed.date, "-", 3)[,1]
officer$AppointmentYear = as.numeric(appt.year)
officer.sub = officer[appt.year<2009, ]
officer.sub$complaints = num.comp[match(officer.sub$officer_id, names(num.comp))]

summary(lm(complaints ~ age + gender + race + rank + AppointmentYear, 
  data = officer.sub))



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














