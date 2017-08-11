# accra






packages <- c("dplyr", "rgdal", "raster", "rgeos", "geosphere")
lapply(packages, library, character.only = TRUE)

setwd("H:/GitHub/accra_drains/")
source(paste0(getwd(), "/", "01_helper_length_calc.R"))



# load data ----
# waterways
sewage <- shapefile(paste0(getwd(), "/shapefiles/waterways/", "accra_waterw_con_ed_level_4.shp"))
sewage <- spTransform(sewage, CRS("+proj=longlat +datum=WGS84"))
sewage <- sewage[order(sewage@data$id2), ]
sewage.prj <- spTransform(sewage, CRS("+init=epsg:32630"))

# neighborhood weighted mid point
neighb <- read.csv(paste0(getwd(), "/data/", "neighborhoods_all_clean.csv"), sep = ",")
neighbcoord <- read.csv(paste0(getwd(), "/data/", "points_coordinates_output.csv"), sep = ",")

# # neighborhood shape file
# ama <- shapefile(paste0(getwd(), "/shapefiles/neighborhood/", "AMA_projected.shp"))
# ama <- spTransform(ama, CRS("+proj=longlat +datum=WGS84"))
# #***********************************************************************************************

source(paste0(getwd(), "/", "02_preset_analysis_options.R"))
source(paste0(getwd(), "/", "03_function.R"))

df <- point2end(neighbcoord)



# #***********************************************************************************************
# test 
point2end(data.frame("lon" = c(-0.1579323, -0.2179325, -0.2579425), "lat" = c(5.560829, 5.660829, 5.580829)))

#
lon <- as.numeric(format(runif(50, -.26, -0.15), digits = 7))
lat <- as.numeric(format(runif(50, 5.53, 5.67), digits = 7))
test.longlat <- data.frame(lon, lat)
test.df <- point2end(test.longlat)

plot(sewage, col = "lightblue", axes=TRUE)
points(pts.neighb.sp, col="red", pch=20, cex=1)
points(pts.neigh.drain, col="green", pch=10, cex=1) #closest point to drain


plot(sewage[sewage$id2 == 14, ], col = "lightblue", axes=TRUE)
points(pts.neighb.sp[pts.neighb.sp$ID==1, ], col="blue", pch=20, cex=1)
points(pts.neigh.drain[pts.neigh.drain$ID==1, ], col="yellow", pch=10, cex=1) #closest point to drain

df %>% filter(iteration == 1)


# #***********************************************************************************************
# # test to cut drain segment
# 
# #setup circle/ line to create spatial polygons/ lines data frame & then later delete this first entry
# circles.prj1 <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[1],], width=d, byid=TRUE)
# circles.prj1 <- spTransform(circ.st, CRS('+proj=longlat +datum=WGS84'))
# line1 <- gIntersection(circles.prj1[1,], sewage[sewage$id2 == circles.prj1$ID[1], ], byid=c(TRUE, TRUE))
# line1$ID <- circles.prj1$ID[1]
# 
# for (i in 1:length(pts.neigh.drain)){
#         d <- gDistance(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i], ], pts.neigh.drain.sp[i, ])
#         circles.prj <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i],], width=d, byid=TRUE)
#         circles.prj <- spTransform(circles.prj, CRS('+proj=longlat +datum=WGS84'))
#         circles.prj1 <- rbind(circles.prj1, circles.prj)
#         
#         line2 <- gIntersection(circles.prj1[i,], sewage[sewage$id2 == circles.prj1$ID[i], ], byid=c(TRUE, TRUE))
#         line2$ID <- circles.prj1$ID[i]
#         line1 <- rbind(line1, line2)
#         }
# 
# # delete initial entries
# circles.prj1[1,] <- NA
# circles.prj1 <- circles.prj1[!is.na(circles.prj1@data$ID), ]
# line1[1,] <- NA
# line1 <- line1[!is.na(line1@data$ID), ]
# 











# circles.prj1 <- spTransform(circles.prj1, CRS('+proj=longlat +datum=WGS84'))
# dfff <- SpatialPolygonsDataFrame(circ, data=circ@data)







# circles.prj <- spTransform(circles.prj, CRS('+proj=longlat'))

plot(circles.prj, add=T, col=4)

line <- gIntersection(circles.prj, sewage.prj[sewage.prj$id2 == 14, ], byid=c(TRUE, TRUE))
line <- spTransform(line, CRS('+proj=longlat'))
plot(line1, add= T)

###
d <- gDistance(pts.drain.end.prj[pts.neigh.drain.sp$ID2[2], ], pts.neigh.drain.sp[2, ])
circles.prj <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[2],], width=d, byid=TRUE)
# circles.prj <- spTransform(circles.prj, CRS('+proj=longlat'))

plot(circles.prj, add=T)

line <- gIntersection(circles.prj, sewage.prj[sewage.prj$id2 == 22, ], byid=c(TRUE, TRUE))
line <- spTransform(line, CRS('+proj=longlat'))
plot(line, add= T)
# #***********************************************************************************************



# Plot example
plot(sewage, col = "lightblue", axes=TRUE)
points(pts.neighb.sp, col="red", pch=20, cex=1)
points(pts.neigh.drain, col="green", pch=10, cex=1) #closest point to drain


# plot(sewage[sewage$id2 == 14, ], col = "lightblue", axes=TRUE)
points(pts.neighb.sp[pts.neighb.sp$ID==1, ], col="blue", pch=20, cex=1)
points(pts.neigh.drain[pts.neigh.drain$ID==1, ], col="orange", pch=10, cex=1) #closest point to drain
plot(sewage.part[sewage.part$ID == 2, ], col = "blue", axes=TRUE, add=TRUE)
# plot(sewage[sewage$id2 == 2, ], col = "lightblue", axes=TRUE, add=TRUE)
plot(sewage.part[sewage.part$ID == 2, ], col = "blue", axes=TRUE, add=TRUE)


# plotting process:
point <- 66

df[df$iteration == point, ]
plot.lines <- df[df$iteration == point, ]$drain
max <- length(plot.lines)
plot.lines.reg <- plot.lines[-max]

plot(sewage, col = "lightblue", axes=TRUE)
points(pts.neighb.sp[pts.neighb.sp$ID==point, ], col="blue", pch=20, cex=1)
points(pts.neigh.drain[pts.neigh.drain$ID==point, ], col="orange", pch=10, cex=1) #closest point to drain
plot(sewage.part[sewage.part$iteration == point, ] , col = "blue", axes=TRUE, add=TRUE)
for (i in plot.lines.reg){
        plot(sewage[sewage$id2 == i, ], col = "blue", axes=TRUE, add=TRUE)
}




# plot(circles.prj1[circles.prj1$iteration == 70, ], axes=TRUE, add=TRUE)


plot(sewage[sewage$id2 == 8, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2 == 7, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2 == 6, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2 == 5, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2 == 4, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2 == 3, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2 == 2, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2 == 1, ], col = "blue", axes=TRUE, add=TRUE)

