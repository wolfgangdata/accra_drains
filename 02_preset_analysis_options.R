# preset options:

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

rm(a,b,c)
# # alternative way of measuring length of all line segments
# sewage.length.df$test2 <- SpatialLinesLengths(sewage.prj)
# sewage.length.df$test3 <- sewage.length.df$test2 - sewage.length.df$length
#***********************************************************************************************

