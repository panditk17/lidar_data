## codes to create canopy height model from cloud data

plot(las)

hist(las$Z)
col <- height.colors(50)
# Points-to-raster algorithm with a resolution of 1 meter
chm <- grid_canopy(las, res = 1, p2r())
plot(chm, col = col)




dtm = grid_terrain(las, algorithm = tin())

plot(dtm)

chm2<-chm-dtm
plot(chm2)

writeRaster(chm,"../lid_data/clipped_chm.tif",overwrite=TRUE)
# Points-to-raster algorithm with a resolution of 0.5 meters replacing each
# point by a 20-cm radius circle of 8 points
chm <- grid_canopy(las, res = 0.5, p2r(0.2))
plot(chm, col = col)
# Basic triangulation and rasterization of first returns
chm <- grid_canopy(las, res = 0.5, dsmtin())
plot(chm, col = col)
# Khosravipour et al. pitfree algorithm
chm <- grid_canopy(las, res = 0.5, pitfree(c(0,2,5,10,15), c(0, 1.5)))
plot(chm, col = col)

las = readLAS("../lid_data/OSFDP_NEON_2019.las", filter = "-keep_xy 404000, 3285000, 404050, 3285050")
