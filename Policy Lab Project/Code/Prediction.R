
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
fit = prcomp(mat, center=F, scale=F, tol=0.1)





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

sex.netwk = matrix(1, ncol=n.officer, nrow=n.officer)
of.id.male = comp$officer_id[officer$gender=="MALE"]
of.id.female = comp$officer_id[officer$gender=="FEMALE"]
male.id = match(of.id.male, officer.ID)
female.id = match(of.id.female, officer.ID)
male.pairs = combn(x=male.id, m=2)
female.pairs = combn(x=female.id, m=2)
for (i in 1:ncol(male.pairs)){
  sex.netwk[ male.pairs[1,i], male.pairs[2, i] ] = sex.netwk[ male.pairs[1,i], male.pairs[2, i] ] - 1
  cat(sprintf("\r%d",i))
}

for (i in 1:ncol(female.pairs)){
  sex.netwk[ female.pairs[1,i], female.pairs[2, i] ] = sex.netwk[ female.pairs[1,i], female.pairs[2, i] ] - 1
  cat(sprintf("\r%d",i))
}












