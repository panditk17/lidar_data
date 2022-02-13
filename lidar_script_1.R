
## script to read liar data and plot

library(rLiDAR)

LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")

# Read LAS file
LAS<-readLAS(LASfile,short=TRUE)

# subset the data for height
xyz<-subset(LAS[,1:3],LAS[,3] >= 1.37)

# get LiDAR clusters
set.seed(1)
clLAS<-kmeans(xyz, 32)


ppp2<-kmeans(ppp,16)

id<-as.factor(ppp2$cluster)
ppp4<-cbind(ppp[,1:2],id)
chullTrees<-chullLiDAR2D(ppp4)

plot(SpatialPoints(ppp[,1:2]),cex=0.5,col=ppp[,3])
plot(chullTrees$chullPolygon,add=TRUE, border='green')



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

### plot 3D of lidar convex hull

# Set the alpha
alpha<-0.3

# Set the plotCAS parameter
plotit=TRUE

# Set the convex hull color
col="forestgreen"

# Combining xyz and id
xyzid<-cbind(xyz,id)

# Get the volume and surface area
library(rgl)
open3d()

plot3d()

volumeList<-chullLiDAR3D(xyzid=ppp, plotit=plotit, col=col,alpha=alpha)


volumeList<-chullLiDAR3D(xyzid=xyzid, plotit=plotit, col=col,alpha=alpha)
summary(volumeList) # summary

plot3d(xyzid[,1:3], add=TRUE) # add the 3D point cloud
axes3d(c("x+", "y-", "z-")) # axes
grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid
title3d(xlab = "UTM Easthing", ylab = "UTM Northing",zlab = "Height", col="red")
aspect3d(1,1,0.7) # scale

library(raster)
jjj<-raster("OSBS_chm.tif")
plot(jjj)
data(chm)  
  
  
  # Importing the LiDAR-derived CHM file
  data(chm) # or set a CHM. e.g. chm<-raster("CHM_stand.asc")
#=======================================================================#
# Example 01: Smoothing the CHM using a Gaussian filter
#=======================================================================#
# Set the ws:
ws<-3 # dimension 3x3
# Set the filter type
filter<-"Gaussian"
# Set the sigma value
sigma<-0.6
# Smoothing CHM
sCHM<-CHMsmoothing(jjj, filter, ws, sigma)
#=======================================================================#
# Example 02: Smoothing the CHM using a mean filter
#=======================================================================#
# Set the ws:
ws<-5 # dimension 5x5
# Set the filter type
filter<-"mean"
# Smoothing and plotting LiDAR-derived CHM
sCHM<-CHMsmoothing(jjj, filter, ws)
plot(sCHM)

