#################################################################################
# Drains & Neighborhoods - Accra, Ghana
#################################################################################
#################################################################################
# To Do: 
#       [x] define end point of drains
#       [x] define levels of drains (1,2,3) in ArcGIS and simplify network
#       [ ] identify which id of drain the point connects to
#       [ ] identify which level drain the point connects to
#       [x] d0 ... distance from neighborhood to first intersection of drain
#       [ ] d3 ... distance from crosspoint to crosspoint on level 3
#       [ ] d2 ... distance from crosspoint to crosspoint on level 2
#       [x] d1 ... distance from crosspoint to endpoint on level 1

#       [ ] distance from neighborhood to first intersection of drain
#       [ ] buffer calc... line segment length (andrew's function)
#       [x] calculate intersection points of each drain segment
#       [x] calc cut - length of drain
#       [ ] ^^ loop
#       [ ] calc distance from intersection to end point of drain, along drain.
#       [ ] define flow of water?
#       [ ] 
#################################################################################

library(rgdal)
library(raster)
library(rgeos)
library(geosphere)
library(dplyr)

setwd("H:/GitHub/accra_drains/")
source(paste0(getwd(), "/", "helper_length_calc.R"))

# data abreviations
# .sp ... spatially adjusted with spTransform
# .prj ... projected with CRS("+init=epsg:32630")


# load data ----
# waterways
sewage <- shapefile(paste0(getwd(), "/shapefiles/waterways/", "accra_waterw_con_ed_level_4.shp"))
sewage <- spTransform(sewage, CRS("+proj=longlat +datum=WGS84"))

sewage <- sewage[order(sewage@data$id2), ]

# neighborhood weighted mid point
neighbcoord <- read.csv(paste0(getwd(), "/data/", "points_coordinates_output.csv"), sep = ",")

# neighborhood shape file
ama <- shapefile(paste0(getwd(), "/shapefiles/neighborhood/", "AMA_projected.shp"))
ama <- spTransform(ama, CRS("+proj=longlat +datum=WGS84"))








# calculations ----
dat0 <- neighbcoord

# neighborhoods
pts.neighb.sp <- SpatialPointsDataFrame(dat0[, c("lon", "lat")], data.frame(ID=seq(1:nrow(dat0))),
                                        proj4string=CRS("+proj=longlat +datum=WGS84"))
sewage.prj <- spTransform(sewage, CRS("+init=epsg:32630"))

# sewage.length <- c()
# for (i in 1:length(pts.neighb.sp[ ,1])) {
#         the.points.projected <- spTransform(pts.neighb.sp[i, ], CRS( "+init=epsg:32630" ))
#         the.circles.projected <- gBuffer(the.points.projected, width=100, byid=TRUE)
#         sewage.length[i] <- line.length(the.sewage.projected, the.circles.projected)
# }


# distance to sewage + intersection point (shortest way to line)
dist2sewagedf <- data.frame(matrix(NA, nrow=1, ncol=4))
colnames(dist2sewagedf) <- c("distance", "lon", "lat", "ID2") #ID2 is the feature (drain part) it intersects with
for (i in 1:length(dat0[ ,1])) {
        dist2sewagedf[i, ] <- rbind(dist2Line(dat0[i, c("lon", "lat")], sewage)) #coordinates of intersection
}

dist2sewage <- dist2sewagedf$distance

# take out neighborhoods that are too far away
dist2sewagedf.adj <- dist2sewagedf %>% filter(!distance > 1500)

pts.neigh.drain <- SpatialPointsDataFrame(dist2sewagedf.adj[c("lon", "lat")], 
                                          data.frame(ID=seq(1:nrow(dist2sewagedf.adj)), ID2 = dist2sewagedf.adj$ID2),
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))
pts.neigh.drain.sp <- spTransform(pts.neigh.drain, CRS( "+init=epsg:32630" ))

pts.neigh.drain.sp$ID2 #intersect with which drain id


# endpoint
# points(-0.221109, 5.530075, cex=1)
endpoint <- data.frame("lon" = c(-0.221109), "lat" = c(5.530075))
endpoint.proj <- SpatialPointsDataFrame(endpoint[c("lon", "lat")],
                                        data.frame(ID=seq(1:nrow(endpoint))),
                                        proj4string=CRS("+proj=longlat +datum=WGS84"))
endpoint.proj.sp <- spTransform(endpoint.proj, CRS( "+init=epsg:32630" ))


# # plot ------
plot(sewage, col = "blue", axes=TRUE)
# plot(waterways, col = waterways$length)
points(pts.neighb.sp, col="red", cex=1)
points(pts.neigh.drain, col="green", cex=1)

points(endpoint.proj, cex=1)



# distance NOT along the drain (in meters)
gDistance(endpoint.proj.sp, pts.neigh.drain.sp[1,])



# # test draw, level of drains
# plot(sewage[sewage$level==1, ], col = "blue", axes=TRUE)
# plot(sewage[sewage$level==2, ], col = "green", axes=TRUE, add=TRUE)
# plot(sewage[sewage$level==3, ], col = "brown", axes=TRUE, add=TRUE)
# 
# plot(sewage[sewage$level==1, ], col = "blue", axes=TRUE)
# plot(sewage[sewage$id2==1, ], col = "red", axes=TRUE, add=TRUE)
# plot(sewage[sewage$id2==31, ], col = "orange", axes=TRUE, add=TRUE)


# sample run
# points(pts.neigh.drain[pts.neigh.drain$ID==45, ], col = "blue")
# points(-0.2196986,5.538641, col="red", cex=1) #ID 45
# 
# points(-0.221109, 5.530075, cex=1) #endpoint

test.points <- data.frame("lon" = c(-0.221109, -0.2196986), "lat" = c(5.530075, 5.538641))
test.points <- SpatialPointsDataFrame(test.points[c("lon", "lat")], 
                                        data.frame(ID=seq(1:nrow(test.points))),
                                        proj4string=CRS("+proj=longlat +datum=WGS84"))
test.points.sp <- spTransform(test.points, CRS( "+init=epsg:32630" ))


d11 <- gDistance(test.points.sp[1,], test.points.sp[2,]) # distance between two points

sewage.length <- c()
test.points.prj <- spTransform(test.points.sp, CRS( "+init=epsg:32630" ))
circles.prj <- gBuffer(test.points.prj[1,], width=d11, byid=TRUE)
sewage.length <- line.length(sewage.prj, circles.prj)

points(test.points, col = "orange")

# points(pts.neigh.drain, col = "orange")
# with(pts.neigh.drain, text(pts.neigh.drain, labels = pts.neigh.drain$ID))



# find intersection of 2 drains
i2 <- gIntersection(sewage[sewage$id2==1, ], sewage[sewage$id2==2, ], byid=F)
i3 <- gIntersection(sewage[sewage$id2==2, ], sewage[sewage$id2==3, ])



points(i2)
points(i3)

i2 <- spTransform(i2, CRS( "+init=epsg:32630" ))
i3 <- spTransform(i3, CRS( "+init=epsg:32630" ))

df <- rbind(i2, i3)

# dist between 2 and 3 int. pts
d22 <- gDistance(i2, i3)

sewage.length <- c()
test.points.projected <- spTransform(df, CRS( "+init=epsg:32630" ))
test.circles.projected <- gBuffer(test.points.projected[1,], width=d22, byid=TRUE)
sewage.length <- line.length(the.sewage.projected, test.circles.projected)




# all coordinates of each point for each line ----
endpoints <- lapply(slot(sewage, "lines"), function(x) lapply(slot(x, "Lines"), function(y) slot(y, "coords")))

# select endpoints of each line == intersections of drains
# for some reason for drain 15 and 25, the endpoints are at the beginning of the df, therefore head() is used
pts.drain.int <- data.frame("lon" = c(-NA), "lat" = c(NA))
for (i in 1:length(endpoints)) {
        if (i == 15 | i == 25){
                pts.drain.int[i, ] <- head(data.frame(endpoints[i]), 1)
                
        } else {
                pts.drain.int[i, ] <- tail(data.frame(endpoints[i]), 1)
        }
}

pts.drain.int$drain <- c(1:31)
rownames(pts.drain.int) <- c(1:31)
# #plot to check points
# points(df, col = "blue")
# with(df, text(df, labels = df$drain))
# points(df[df$drain==10,], col = "blue")


# sample run 2 
# p_n_d # 48  ____ pts.drain.int$drain==2
# + drain 1 length

points(pts.neigh.drain, col="green", cex=1)
points(pts.neigh.drain[pts.neigh.drain$ID==48, ], col = "orange")

# points(pts.drain.int, col="red", cex=1)
# with(pts.drain.int, text(pts.drain.int, labels = pts.drain.int$drain))




# points(pts.drain.int[pts.drain.int$drain==2, ], col = "blue")
# 
# 
# p.48 <- pts.neigh.drain[pts.neigh.drain$ID==48, ]
# test.points.projected <- spTransform(df, CRS( "+init=epsg:32630" ))
# 
# 
# 
# test.points <- SpatialPointsDataFrame(pts.drain.int[c("lon", "lat")], 
#                                       data.frame(ID=seq(1:nrow(pts.drain.int))),
#                                       proj4string=CRS("+proj=longlat +datum=WGS84"))
# test.points.sp <- spTransform(test.points, CRS( "+init=epsg:32630" ))
# 
# 
# 
# pts.drain.int[pts.drain.int$drain == 4, ]
# 
# gIntersects(test.points[test.points$ID ==3, ], sewage[sewage$id2==3, ], byid = TRUE)
# 
# over(test.points[test.points$ID ==3, ], sewage[sewage$id2==3, ], byid = TRUE)
# 
# 
# tst <- gBuffer(test.points.sp, width = 0.02)
# 
# overlay(tst, sewage)
# 


plot(sewage[sewage$id2==3, ])
points(p.48)
points(pts.drain.int[pts.drain.int$drain == 3, ])
points(pts.drain.int[pts.drain.int$drain == 4, ])


# # calculate intersections based on drain lines [obsolete now]
# combi <- read.csv(paste0(getwd(), "/data/", "combinations.csv"), sep = ",")
# 
# ii <- data.frame("x" = c(-0.221109, NA), "y" = c(5.530075, NA))
# liist <- list(c(1))
# for (i in 1:(length(sewage)-1)) {
#         ii[i+1, ] <- coordinates(gIntersection(sewage[sewage$id2==combi[i, "a"], ], sewage[sewage$id2==combi[i, "b"], ]))
#         liist[i+1] <- i+1
# }
# 
# ii$iter <- liist


