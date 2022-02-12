
## script to read liar data and plot

library(rLIDAR)

LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")
# Read LAS file
LAS<-readLAS(LASfile,short=TRUE)
# subset the data for height
xyz<-subset(LAS[,1:3],LAS[,3] >= 1.37)
# get LiDAR clusters
set.seed(1)
clLAS<-kmeans(xyz, 32)
# Set the points id
id<-as.factor(clLAS$cluster)
# Set the xyid input
xyid<-cbind(xyz[,1:2],id)
# Compute the LiDAR convex hull of the clusters
chullTrees<-chullLiDAR2D(xyid)
# Plotting the LiDAR convex hull
library(sp)
plot(SpatialPoints(xyid[,1:2]),cex=0.5,col=xyid[,3])
plot(chullTrees$chullPolygon,add=TRUE, border='green')
# Get the ground-projected area of LiDAR convex hull
chullList<-chullTrees$chullArea
summary(chullList) # summary




