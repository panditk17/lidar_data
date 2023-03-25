## codes to create 3D plots from lidar cloud
## using packages lidR and magrittr
## 07-16-2022
## Karun Pandit
## ***************************************

library(lidR)
las1 = readLAS("../lid_data/OSFDP_NEON_2019.las", filter = "-keep_xy 404000, 3285000, 404100, 3285100")
las1 = readLAS("../lid_data/OSFDP_NEON_2019.las")


plot(las1, color = "RGB", trim = 1800, bg = "white")
## create dtm 
dtm <- grid_terrain(las1, 1, knnidw())
plot(dtm, col = gray.colors(50, 0, 1))

## normalize las file 
las<-las1-dtm
plot(las, color = "RGB", trim = 1800, bg = "white")
plot(las)

dtm = grid_terrain(las, algorithm = tin())
ttops <- find_trees(las, lmf(ws = 5))

## creating figure based on normalized las

plot_dtm3d(dtm, bg="white")

x = plot(las,bg="white")
add_dtm3d(x, dtm, bg="white")
add_treetops3d(x, ttops)

library(magrittr)
plot(las) %>% add_dtm3d(dtm) %>% add_treetops3d(ttops)

p2<-ggplot(las@data, aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  theme_minimal() +
  coord_equal()+
  scale_color_gradientn(colours = viridis(50))+
  ylab("Height (m)")+
  xlab("Transect distance (m)")+
  theme(axis.text.x = element_text(size=14),  
        axis.text.y = element_text(size=14),axis.title.x = element_text(face="bold",size=16),
        axis.title.y = element_text(face="bold",size=16))

ggsave("p3.png",width=15,height=5,dpi=600)



