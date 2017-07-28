#################################################################################
# Drains & Neighborhoods - Accra, Ghana
#################################################################################
#################################################################################
# To Do: 
#       * define end point of drain -- done
#       * calc cut - length of drain
#       * calc distance from intersection to end point of drain, along drain.
#       * define flow of water?
#       * some kind of network analysis?
#################################################################################

library(rgdal)
library(raster)
library(rgeos)
library(geosphere)
library(dplyr)

setwd("H:/GitHub/accra_drains/")

# waterways ----
sewage <- shapefile(paste0(getwd(), "/shapefiles/waterways/", "accra_ghana_osm_waterways_con_edited.shp"))
sewage <- spTransform(sewage, CRS("+proj=longlat +datum=WGS84"))

# neighborhood weighted mid point
neighbcoord <- read.csv(paste0(getwd(), "/data/", "points_coordinates_output.csv"), sep = ",")

# neighborhood shape file
ama <- shapefile(paste0(getwd(), "/shapefiles/neighborhood/", "AMA_projected.shp"))
ama <- spTransform(ama, CRS("+proj=longlat +datum=WGS84"))


#function to calculate the length of line intersected by circle ----
line.length <- function(the.lines.projected, the.circles.projected) {
        if (gIntersects(the.lines.projected, the.circles.projected)) {
                lines_crp <- crop(the.lines.projected, the.circles.projected)
                lines_crp_length <- gLength(lines_crp)
                return(lines_crp_length)
        } else {
                return(0)
        }
}


# calculations ----
dat0 <- neighbcoord

the.points.sp <- SpatialPointsDataFrame(dat0[, c("lon", "lat")], data.frame(ID=seq(1:nrow(dat0))),
                                        proj4string=CRS("+proj=longlat +datum=WGS84"))
the.sewage.projected <- spTransform(sewage, CRS( "+init=epsg:32630" ))
sewage.length <- c()
for (i in 1:length(the.points.sp[ ,1])) {
        the.points.projected <- spTransform(the.points.sp[i, ], CRS( "+init=epsg:32630" ))
        the.circles.projected <- gBuffer(the.points.projected, width=100, byid=TRUE)
        sewage.length[i] <- line.length(the.sewage.projected, the.circles.projected)
}


# distance to sewage + intersection point (shortest way to line)
dist2sewagedf <- data.frame(matrix(NA, nrow=1, ncol=4))
colnames(dist2sewagedf) <- c("distance", "lon", "lat", "ID")
for (i in 1:length(dat0[ ,1])) {
        dist2sewagedf[i, ] <- rbind(dist2Line(dat0[i, c("lon", "lat")], sewage)) #coordinates of intersection
}

dist2sewage <- dist2sewagedf$distance

dist2sewagedf.adj <- dist2sewagedf %>% filter(!distance > 1500)
intersec.points <- SpatialPointsDataFrame(dist2sewagedf.adj[c("lon", "lat")], 
                                          data.frame(ID=seq(1:nrow(dist2sewagedf.adj))),
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))
intersec.points.sp <- spTransform(intersec.points, CRS( "+init=epsg:32630" ))


# endpoint
# points(-0.221109, 5.530075, cex=1)
endpoint <- data.frame("lon" = c(-0.221109), "lat" = c(5.530075))
endpoint.proj <- SpatialPointsDataFrame(endpoint[c("lon", "lat")], 
                                        data.frame(ID=seq(1:nrow(endpoint))),
                                        proj4string=CRS("+proj=longlat +datum=WGS84"))
endpoint.proj.sp <- spTransform(endpoint.proj, CRS( "+init=epsg:32630" ))


# plot ------
plot(sewage, col = "blue", axes=TRUE)
# plot(waterways, col = waterways$length)
points(the.points.sp, col="red", cex=1)
points(intersec.points, col="green", cex=1)

points(endpoint.proj, cex=1)



# distance NOT along the drain (in meters)
gDistance(endpoint.proj.sp, intersec.points.sp[1,])



