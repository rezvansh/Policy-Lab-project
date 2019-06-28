##Policy Misconduct
######### Read in coordinates
rm(list=ls())
setwd("~/Desktop")
geo<-read.table("Complaints_GeoCoordinates (1).txt", header = T)
######### Remove missing
missing = which(is.na(geo[,2]) | geo[,2]>42.1 | geo[,2]<41.5)
geo.full = geo[-missing,]
#map policy misconduct locations
#geocomplaints = as.list(geocomplaints)
#attach(geocomplaints)
#install.packages("ggmap")
library(ggmap)
events <- get_map(location = c(-87.6298, 41.8781), source="google", zoom=10)
#events is background map
ggmap(events) + geom_point(aes(x=geo.full$Longitude, y=geo.full$Latitude), data=geo.full, fill="red", colour="black", pch=18, alpha=1)
#zoom in to smaller areas so you can see detail