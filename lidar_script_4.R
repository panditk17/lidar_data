## lidar data analysis
## modification of previous lidar script version 3
## 2023
###########################################333333


library(rLiDAR)

# LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")
library(raster)
library(lidR)
library(rLiDAR)
remove.packages("lidR")

# Read LAS file
LAS<-readLAS("../lid_data/OSFDP_NEON_2019.las",short=TRUE)

las1 = readLAS("../lid_data/OSFDP_NEON_2019.las")

# las1 = readLAS("../lid_data/OSFDP_NEON_2019.las",select = "xyz", filter = "-keep_first")
LAS = clip_rectangle(las1, 404000, 3285000, 404050, 3285050)
writeLAS(LAS,"../lid_data/clipped_LAS.las")

LAS=data.frame(LAS)
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

### plot 3D of lidar convex hull

# Set the alpha
alpha<-0.3

# Set the plotCAS parameter
plotit=TRUE

# Set the convex hull color
col="forestgreen"
