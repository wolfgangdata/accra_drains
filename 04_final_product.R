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
neighbcoord <- read.csv(paste0(getwd(), "/data/", "points_coordinates_output.csv"), sep = ",")

# # neighborhood shape file
# ama <- shapefile(paste0(getwd(), "/shapefiles/neighborhood/", "AMA_projected.shp"))
# ama <- spTransform(ama, CRS("+proj=longlat +datum=WGS84"))
# #***********************************************************************************************

source(paste0(getwd(), "/", "02_preset_analysis_options.R"))
source(paste0(getwd(), "/", "03_function.R"))


# test 
point2end(data.frame("lon" = c(-0.1579323, -0.2179325, -0.2579425), "lat" = c(5.560829, 5.660829, 5.580829)))

#
lon <- as.numeric(format(runif(50, -.26, -0.15), digits = 7))
lat <- as.numeric(format(runif(50, 5.53, 5.67), digits = 7))
test.longlat <- data.frame(lon, lat)
test.df <- point2end(test.longlat)
# points(pts.neighb.sp[pts.neighb.sp$ID==1, ], col="blue", pch=20, cex=1)
# points(pts.neigh.drain[pts.neigh.drain$ID==1, ], col="yellow", pch=10, cex=1) #closest point to drain


# df <- point2end(neighbcoord)

# Plot example
plot(sewage, col = "lightblue", axes=TRUE)
points(pts.neighb.sp, col="red", pch=20, cex=1)
points(pts.neigh.drain, col="green", pch=10, cex=1) #closest point to drain
