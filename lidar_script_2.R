
## script to read liar data and plot

library(rLiDAR)

LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")

# Read LAS file
LAS<-readLAS(LASfile,short=TRUE)

library(raster)

las2<-raster("../lid_data/OSBS_chm.tif")
plot(las2)

extent<-c(xmin=404000,xmax=404050,ymin=3285000,ymax=3285050)

las3<-crop(las2,extent)
plot(las3)

xyz<-rasterToPoints(las3)

# subset the data for height
# xyz<-subset(LAS[,1:3],LAS[,3] >= 1.37)

# get LiDAR clusters
set.seed(1)
clLAS<-kmeans(xyz, 100)

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

# plot3d()

# volumeList<-chullLiDAR3D(xyzid=ppp, plotit=plotit, col=col,alpha=alpha)


volumeList<-chullLiDAR3D(xyzid=xyzid, plotit=plotit, col=col,alpha=alpha)
summary(volumeList) # summary

plot3d(xyzid[,1:3], add=TRUE) # add the 3D point cloud
axes3d(c("x+", "y-", "z-")) # axes
grid3d(side=c('x+', 'y-', 'z'), col="gray") # grid
title3d(xlab = "UTM Easthing", ylab = "UTM Northing",zlab = "Height", col="red")
aspect3d(1,1,0.7) # scale


sCHM<-CHMsmoothing(las3, filter="mean", ws=5) # smoothing CHM
loc<-FindTreesCHM(sCHM, fws=5, minht=8) # or import a tree list
# Set the maxcrown parameter
maxcrown=10.0
# Set the exclusion parameter
exclusion=0.3 # 30
# Compute individual tree detection canopy area
canopy<-ForestCAS(las3, loc, maxcrown, exclusion)

boundaryTrees<-canopy[[1]]
# Plotting the individual tree canopy boundary over the CHM
plot(las3, main="LiDAR-derived CHM")
# adding tree canopy boundary
plot(boundaryTrees, add = TRUE, border = 'red', bg = 'transparent')



#============================================================================#
# Retrieving the list of individual trees detected for canopy area calculation
#============================================================================#
canopyList<-canopy[[2]] # list of ground-projected areas of individual tree canopies
summary(canopyList) # summary
# Spatial location of the trees
library(sp)
XY<-SpatialPoints(canopyList[,1:2]) # Spatial points
plot(XY, col = "black", add = TRUE, pch = "*") # adding tree location to the plot




LiDARForestStand(crownshape = c( "cone"), CL = 14, CW = canopyList[,4], HCB = 10,
                 X = canopyList[,1], Y = canopyList[,2], dbh = 0.3, crowncolor = "forestgreen",
                 stemcolor = "chocolate4", resolution="high", mesh=TRUE)



# Set the dimensions of the displayed forest stand
xlength<-20 # x length
ylength<-15 # y length
# Set the space between trees
sx<-3 # x space length
sy<-2 # y space length
# Tree location grid
XYgrid <- expand.grid(x = seq(1,xlength,sx), y = seq(1,ylength,sy))
XYgrid<-cbind(canopyList[,1],canopyList[,2])
# Get the number of trees
Ntrees<-nrow(XYgrid)
Ntrees<-nrow(canopyList)
# Plot a virtual Eucalyptus forest plantation stand using the halfellipsoid tree crown shape
# Set stand trees parameters
meanHCB<-5 # mean of the height at canopy base
sdHCB<-0.1 # standard deviation of the height at canopy base
HCB<-rnorm(Ntrees, mean=meanHCB, sd=sdHCB) # height at canopy base
CL<-HCB*1.4 # tree crown height
CW<-HCB*0.5 # tree crown diameter
CW<-sqrt(canopyList[,4]*4/3.14)
open3d() # open a rgl window
# Plotting the stand
for( i in 1:Ntrees){
  LiDARForestStand(crownshape = "cone", CL = CL[i], CW = CW[i],
                   HCB = HCB[i], X = XYgrid[i,1], Y = XYgrid[i,2], dbh = 0.4,
                   crowncolor = "forestgreen", stemcolor = "chocolate4",
                   resolution="high", mesh=TRUE)
}


