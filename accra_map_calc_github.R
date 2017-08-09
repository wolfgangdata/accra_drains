#***********************************************************************************************
# Drains & Neighborhoods - Accra, Ghana
#***********************************************************************************************
# Functionality: 
#       [1] define end point of drains
#       [2] define start point of drains
#       [3] length of each drain section
#       [4] define drain levels
#       [5] define connectivity of drains
#       [6] calculate neighb. point intersection with drains
#       [7] calculate length of intersection [6] to end point of drain section
#       [8] combine [7] with rest of drain "route" to end point 
#       [9] return data frame 
#
# TODO(W): wrap code into function, so random xy-points can be entered, then calculated
# TODO(W): cut sewer segment at intersection point as separate - for plotting

#***********************************************************************************************
# Note: data abreviations
# .sp  ... spatially adjusted with spTransform
# .prj ... projected with CRS("+init=epsg:32630")



packages <- c("dplyr", "rgdal", "raster", "rgeos", "geosphere")
lapply(packages, library, character.only = TRUE)

setwd("H:/GitHub/accra_drains/")
source(paste0(getwd(), "/", "helper_length_calc.R"))


# load data ----
# waterways
sewage <- shapefile(paste0(getwd(), "/shapefiles/waterways/", "accra_waterw_con_ed_level_4.shp"))
sewage <- spTransform(sewage, CRS("+proj=longlat +datum=WGS84"))
sewage <- sewage[order(sewage@data$id2), ]
sewage.prj <- spTransform(sewage, CRS("+init=epsg:32630"))

# neighborhood weighted mid point
neighbcoord <- read.csv(paste0(getwd(), "/data/", "points_coordinates_output.csv"), sep = ",")

# neighborhood shape file
ama <- shapefile(paste0(getwd(), "/shapefiles/neighborhood/", "AMA_projected.shp"))
ama <- spTransform(ama, CRS("+proj=longlat +datum=WGS84"))
#***********************************************************************************************


# calculate start and end points of each line segment ----
# all coordinates of each point for each line
linepoints <- lapply(slot(sewage, "lines"), 
                    function(x) lapply(slot(x, "Lines"), function(y) slot(y, "coords")))

# select endpoints of each line == intersections of drains sections
# for some reason for drain 15 and 25, the endpoints are at the beginning of the df, therefore 
#   head() is used
pts.drain.end <- data.frame("lon" = c(NA), "lat" = c(NA))
for (i in 1:length(linepoints)) {
        if (i == 15 | i == 25){
                pts.drain.end[i, ] <- head(data.frame(linepoints[i]), 1)
                
        } else {
                pts.drain.end[i, ] <- tail(data.frame(linepoints[i]), 1)
        }
}

pts.drain.end$drain <- c(1:31)
rownames(pts.drain.end) <- c(1:31)


# pts beginning of drain
pts.drain.start <- data.frame("lon" = c(-NA), "lat" = c(NA))
for (i in 1:length(linepoints)) {
        if (i == 15 | i == 25){
                pts.drain.start[i, ] <- tail(data.frame(linepoints[i]), 1)
                
        } else {
                pts.drain.start[i, ] <- head(data.frame(linepoints[i]), 1)
        }
}
pts.drain.start$drain <- c(1:31)
rownames(pts.drain.start) <- c(1:31)
#***********************************************************************************************


# # calculate length of each drain part ----
# pts.drain.end #end point
# pts.drain.start #start point

# length of each drain
pts.drain.end.sp <- SpatialPointsDataFrame(pts.drain.end[c("lon", "lat")],
                                           data.frame(ID=seq(1:nrow(pts.drain.end))),
                                           proj4string=CRS("+proj=longlat +datum=WGS84"))
pts.drain.end.prj <- spTransform(pts.drain.end.sp, CRS("+init=epsg:32630"))

pts.drain.start.sp <- SpatialPointsDataFrame(pts.drain.start[c("lon", "lat")],
                                           data.frame(ID=seq(1:nrow(pts.drain.start))),
                                           proj4string=CRS("+proj=longlat +datum=WGS84"))
pts.drain.start.prj <- spTransform(pts.drain.start.sp, CRS("+init=epsg:32630"))

sewage.length.df <- data.frame("length" = c(NA), "drain" = c(NA))
for (i in 1:length(sewage.prj)){
        d <- gDistance(pts.drain.end.prj[i, ], pts.drain.start.prj[i, ]) #d of beg and end point of drain
        circles.prj <- gBuffer(pts.drain.end.prj[i,], width=d, byid=TRUE)
        sewage.length.df[i, ] <- c(line.length(sewage.prj[i, ], circles.prj), i)
}

# add levels to drains
a <- c(1:12)
b <- c(13:27)
c <- c(28:31)
drain.levels <- data.frame("drain"=c(a,b,c), 
                           "level"=c(rep(1,length(a)),rep(2,length(b)), rep(3,length(c))))
sewage.length.df <- left_join(sewage.length.df, drain.levels, by="drain")

# # alternative way of measuring length of all line segments
# sewage.length.df$test2 <- SpatialLinesLengths(sewage.prj)
# sewage.length.df$test3 <- sewage.length.df$test2 - sewage.length.df$length
#***********************************************************************************************

# connections of each drain to reach end point
p01 <- sewage.length.df %>% filter(drain %in% c( ))
p02 <- sewage.length.df %>% filter(drain %in% c(1))
p03 <- sewage.length.df %>% filter(drain %in% c(1:2))
p04 <- sewage.length.df %>% filter(drain %in% c(1:3))
p05 <- sewage.length.df %>% filter(drain %in% c(1:4))
p06 <- sewage.length.df %>% filter(drain %in% c(1:5))
p07 <- sewage.length.df %>% filter(drain %in% c(1:6))
p08 <- sewage.length.df %>% filter(drain %in% c(1:7))
p09 <- sewage.length.df %>% filter(drain %in% c(1:8))
p10 <- sewage.length.df %>% filter(drain %in% c(1:9))
p11 <- sewage.length.df %>% filter(drain %in% c(1:10))
p12 <- sewage.length.df %>% filter(drain %in% c(1:11))
p13 <- sewage.length.df %>% filter(drain %in% c(1))
p14 <- sewage.length.df %>% filter(drain %in% c(1:2))
p15 <- sewage.length.df %>% filter(drain %in% c(1:3))
p16 <- sewage.length.df %>% filter(drain %in% c(1:4))
p17 <- sewage.length.df %>% filter(drain %in% c(1:4, 16))
p18 <- sewage.length.df %>% filter(drain %in% c(1:4, 16:17))
p19 <- sewage.length.df %>% filter(drain %in% c(1:4, 16:18))
p20 <- sewage.length.df %>% filter(drain %in% c(1:5))
p21 <- sewage.length.df %>% filter(drain %in% c(1:5, 20))
p22 <- sewage.length.df %>% filter(drain %in% c(1:6))
p23 <- sewage.length.df %>% filter(drain %in% c(1:7))
p24 <- sewage.length.df %>% filter(drain %in% c(1:8))
p25 <- sewage.length.df %>% filter(drain %in% c(1:9))
p26 <- sewage.length.df %>% filter(drain %in% c(1:10))
p27 <- sewage.length.df %>% filter(drain %in% c(1:11))
p28 <- sewage.length.df %>% filter(drain %in% c(1:5, 20))
p29 <- sewage.length.df %>% filter(drain %in% c(1:4, 16))
p30 <- sewage.length.df %>% filter(drain %in% c(1:4, 16:17))
p31 <- sewage.length.df %>% filter(drain %in% c(1:4, 16:18))
#***********************************************************************************************


# where the function should start --------------------------------------------------------------
# neighborhoods
pts.neighb.sp <- SpatialPointsDataFrame(neighbcoord[, c("lon", "lat")], 
                                        data.frame(ID=seq(1:nrow(neighbcoord))),
                                        proj4string=CRS("+proj=longlat +datum=WGS84"))

# distance to sewage (shortest way to line), ID2: feature line intersects with = drain ID
dist2sewagedf <- data.frame("distance" = c(NA),"lon" = c(NA), "lat" = c(NA), "ID2" = c(NA))

# distance + coordinates of intersection + drain ID
for (i in 1:length(neighbcoord[ ,1])) {
        dist2sewagedf[i, ] <- rbind(dist2Line(neighbcoord[i, c("lon", "lat")], sewage)) 
}

# dist2sewage <- dist2sewagedf$distance

# take out neighborhoods that are too far away
# dist2sewagedf.adj <- dist2sewagedf %>% filter(!distance > 1500)

pts.neigh.drain <- SpatialPointsDataFrame(dist2sewagedf[c("lon", "lat")], 
                                          data.frame(ID=seq(1:nrow(dist2sewagedf)), ID2 = dist2sewagedf$ID2),
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))
pts.neigh.drain.sp <- spTransform(pts.neigh.drain, CRS( "+init=epsg:32630" ))

# pts.neigh.drain.sp$ID2 #intersect with which drain id


# calculate each small part of id'd section. from intersection to end point of section.
sewage.part.length.df <- data.frame("length" = c(NA), "drain" = c(NA), "iteration" = c(NA))
for (i in 1:length(pts.neigh.drain)){
        d <- gDistance(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i], ], pts.neigh.drain.sp[i, ])
        circles.prj <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i],], width=d, byid=TRUE)
        sewage.part.length.df[i, ] <- c(line.length(sewage.prj[pts.neigh.drain.sp$ID2[i], ], circles.prj), 
                                        pts.neigh.drain.sp$ID2[i], i)
}

# add drain levels
sewage.part.length.df <- left_join(sewage.part.length.df, drain.levels, by="drain")
sewage.part.length.df$iteration <- NULL


# add lenghts of all drains to start section for each iteration until endpoint reached
sewage.total.df <- data.frame("length"=c(), "drain"=c(), "level"=c(), "iteration"=c())
for (i in 1:length(sewage.part.length.df[, 1])){
        nr <- sewage.part.length.df$drain[i]
        name <- paste0("p", sprintf("%02d", nr))
        sewage.t.length.df <- rbind(get(name), sewage.part.length.df[i, ])
        sewage.t.length.df$iteration <- i
        sewage.total.df <- rbind(sewage.total.df, sewage.t.length.df)
}

sewage.total.df



df1 <- sewage.total.df %>% group_by(iteration) %>% summarise(length=sum(length))



# df <- dist2sewagedf
# df$distance <- NULL
# sum(duplicated(df, incomparables = FALSE))


#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************


plot(sewage, col = "blue", axes=TRUE)
points(pts.neighb.sp, col="red", pch=20, cex=1)
points(pts.neigh.drain, col="green", pch=10, cex=1) #closest point to drain
points(pts.drain.start.sp, col="orange", pch=1, cex=1)
points(pts.drain.end.sp, col="brown", pch=5, cex=1)


# test draw, level of drains
plot(sewage[sewage$level==1, ], col = "blue", axes=TRUE)
plot(sewage[sewage$level==2, ], col = "green", axes=TRUE, add=TRUE)
plot(sewage[sewage$level==3, ], col = "brown", axes=TRUE, add=TRUE)


# test with point #71
plot(sewage, col = "lightblue", axes=TRUE)
points(pts.neighb.sp[pts.neigh.drain.sp$ID==71, ], col="red", pch=20, cex=1)
points(pts.neigh.drain[pts.neigh.drain$ID==71, ], col="green", pch=10, cex=1) #closest point to drain
plot(sewage[sewage$id2==1, ], col = "black", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2==2, ], col = "blue", axes=TRUE, add=TRUE)
plot(sewage[sewage$id2==3, ], col = "black", axes=TRUE, add=TRUE)


pt <- c(-0.2179323, 5.560829)
points(-0.2179323, 5.560829)

line <- (sewage[sewage$id2==3, ])


# sample points
pt <- data.frame("lon" = c(-0.2179323), "lat" = c(5.560829))
pt.sp <- SpatialPointsDataFrame(pt[c("lon", "lat")], 
                                data.frame(ID=seq(1:nrow(pt))),
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
pt.prj <- spTransform(pt.sp, CRS("+init=epsg:32630"))

gIntersection(line, pt.sp)


xy <- as.data.frame(sewage[sewage$id2==3, ], xy=TRUE) 
xy <- as.data.frame(sewage[1,], xy=TRUE) 

plot(sewage[sewage$id2==3, ])



point2end(data.frame("lon" = c(-0.2179323, -0.2179325), "lat" = c(5.560829, 5.660829)))
