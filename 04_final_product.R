# accra






packages <- c("dplyr", "rgdal", "raster", "rgeos", "geosphere", "deSolve")
lapply(packages, library, character.only = TRUE)

setwd("H:/GitHub/accra_drains/")



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

source(paste0(getwd(), "/", "01_helper_length_calc.R"))
source(paste0(getwd(), "/", "02_preset_analysis_options.R"))
source(paste0(getwd(), "/", "03_function.R"))

# run analysis - length of drain travel
df <- point2end(neighbcoord)



# #***********************************************************************************************
# # test 
# point2end(data.frame("lon" = c(-0.1579323, -0.2179325, -0.2579425), "lat" = c(5.560829, 5.660829, 5.580829)))
# 
# lon <- as.numeric(format(runif(50, -.26, -0.15), digits = 7))
# lat <- as.numeric(format(runif(50, 5.53, 5.67), digits = 7))
# test.longlat <- data.frame(lon, lat)
# test.df <- point2end(test.longlat)
# 
# plot(sewage, col = "lightblue", axes=TRUE)
# points(pts.neighb.sp, col="red", pch=20, cex=1)
# points(pts.neigh.drain, col="green", pch=10, cex=1) #closest point to drain
# 
# 
# plot(sewage[sewage$id2 == 14, ], col = "lightblue", axes=TRUE)
# points(pts.neighb.sp[pts.neighb.sp$ID==1, ], col="blue", pch=20, cex=1)
# points(pts.neigh.drain[pts.neigh.drain$ID==1, ], col="yellow", pch=10, cex=1) #closest point to drain
# 
# df %>% filter(iteration == 1)


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







# 
# plot(circles.prj1, add=T, col=4)
# 
# line <- gIntersection(circles.prj, sewage.prj[sewage.prj$id2 == 14, ], byid=c(TRUE, TRUE))
# line <- spTransform(line, CRS('+proj=longlat'))
# plot(line1, add= T)
# 
# ###
# d <- gDistance(pts.drain.end.prj[pts.neigh.drain.sp$ID2[2], ], pts.neigh.drain.sp[2, ])
# circles.prj <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[2],], width=d, byid=TRUE)
# # circles.prj <- spTransform(circles.prj, CRS('+proj=longlat'))
# 
# plot(circles.prj, add=T)
# 
# line <- gIntersection(circles.prj, sewage.prj[sewage.prj$id2 == 22, ], byid=c(TRUE, TRUE))
# line <- spTransform(line, CRS('+proj=longlat'))
# plot(line, add= T)
# #***********************************************************************************************



# # Plot example
# plot(sewage, col = "lightblue", axes=TRUE)
# points(pts.neighb.sp, col="red", pch=20, cex=1)
# points(pts.neigh.drain, col="green", pch=10, cex=1) #closest point to drain
# 
# 
# plot(sewage[sewage$id2 == 14, ], col = "lightblue", axes=TRUE)
# points(pts.neighb.sp[pts.neighb.sp$ID==16, ], col="blue", pch=20, cex=1)
# points(pts.neigh.drain[pts.neigh.drain$ID==1, ], col="orange", pch=10, cex=1) #closest point to drain
# plot(sewage.part[sewage.part$ID == 2, ], col = "blue", axes=TRUE, add=TRUE)
# plot(sewage[sewage$id2 == 2, ], col = "lightblue", axes=TRUE, add=TRUE)
# plot(sewage.part[sewage.part$ID == 8, ], col = "blue", axes=TRUE, add=TRUE)
 


# # # plotting process ----
# plot(sewage, col = "lightblue3", axes=TRUE)
# 
# for (i in 1:length(pts.neigh.drain)){
#         point <- i
#         df[df$iteration == point, ]
#         plot.lines <- df[df$iteration == point, ]$drain
#         max <- length(plot.lines)
#         plot.lines.reg <- plot.lines[-max]
# 
#         if (i < 10) {
#                 png(paste0(getwd(), "/plots/", paste0("map_0", i, ".png") ), width = 6, height = 7, units = 'in', res = 300)
# 
#                 plot(sewage, col = "lightblue", axes=TRUE)
#                 points(pts.neighb.sp[pts.neighb.sp$ID==point, ], col="blue", pch=20, cex=1)
#                 points(pts.neigh.drain[pts.neigh.drain$ID==point, ], col="orange", pch=10, cex=1) #closest point to drain
#                 plot(sewage.part[sewage.part$iteration == point, ] , col = "blue", axes=TRUE, add=TRUE)
# 
#                 for (ii in plot.lines.reg){
#                         plot(sewage[sewage$id2 == ii, ], col = "blue", axes=TRUE, add=TRUE)
#                 }
# 
#                 dev.off()
# 
#         } else {
# 
#                 png(paste0(getwd(), "/plots/", paste0("map_", i, ".png") ), width = 6, height = 7, units = 'in', res = 300)
# 
#                 plot(sewage, col = "lightblue", axes=TRUE)
#                 points(pts.neighb.sp[pts.neighb.sp$ID==point, ], col="blue", pch=20, cex=1)
#                 points(pts.neigh.drain[pts.neigh.drain$ID==point, ], col="orange", pch=10, cex=1) #closest point to drain
#                 plot(sewage.part[sewage.part$iteration == point, ] , col = "blue", axes=TRUE, add=TRUE)
# 
#                 for (ii in plot.lines.reg){
#                         plot(sewage[sewage$id2 == ii, ], col = "blue", axes=TRUE, add=TRUE)
#                 }
# 
#                 dev.off()
#         }
# 
# }
# 
# 
# 
# 
# #***********************************************************************************************
# # plotting with random numbers ----
# lon <- as.numeric(format(runif(50, -.26, -0.15), digits = 7))
# lat <- as.numeric(format(runif(50, 5.53, 5.67), digits = 7))
# test.longlat <- data.frame(lon, lat)
# test.df <- point2end(test.longlat)
# 
# df <- test.df
# for (i in 1:length(test.longlat[, 1])){
#         point <- i
#         df[df$iteration == point, ]
#         plot.lines <- df[df$iteration == point, ]$drain
#         max <- length(plot.lines)
#         plot.lines.reg <- plot.lines[-max]
#         
#         if (i < 10) {
#                 png(paste0(getwd(), "/plots_random/", paste0("map_0", i, ".png") ))
#                 
#                 plot(sewage, col = "lightblue", axes=TRUE)
#                 points(pts.neighb.sp[pts.neighb.sp$ID==point, ], col="blue", pch=20, cex=1)
#                 points(pts.neigh.drain[pts.neigh.drain$ID==point, ], col="orange", pch=10, cex=1) #closest point to drain
#                 plot(sewage.part[sewage.part$iteration == point, ] , col = "blue", axes=TRUE, add=TRUE)
#                 
#                 for (i in plot.lines.reg){
#                         plot(sewage[sewage$id2 == i, ], col = "blue", axes=TRUE, add=TRUE)
#                 }
#                 dev.off()
#                 
#         } else {
#                 
#                 png(paste0(getwd(), "/plots_random/", paste0("map_", i, ".png") ))
#                 
#                 plot(sewage, col = "lightblue", axes=TRUE)
#                 points(pts.neighb.sp[pts.neighb.sp$ID==point, ], col="blue", pch=20, cex=1)
#                 points(pts.neigh.drain[pts.neigh.drain$ID==point, ], col="orange", pch=10, cex=1) #closest point to drain
#                 plot(sewage.part[sewage.part$iteration == point, ] , col = "blue", axes=TRUE, add=TRUE)
#                 
#                 for (i in plot.lines.reg){
#                         plot(sewage[sewage$id2 == i, ], col = "blue", axes=TRUE, add=TRUE)
#                 }
#                 dev.off()
#         }
#         
# }






source(paste0(getwd(), "/", "endemic.R"))


# neighborhood to drain: liquid sewage + decay
# enter drain to end point



# we assume a water/ drain flow rate of 10,000m per day
v <- 10000
df$time <- df$length / v * 24

# assumption of flow rate from neighborhood to drain 1,000m per day
v.d0 <- 1000
dist2sewagedf$time <- (dist2sewagedf$distance / v.d0)  * 24 



# pathogens for each neighborhood
p.neighb <- c()
p.neighb.list <- list()
n.pop <- neighb$p_total

for (i in 1:length(n.pop)){
        p.neighb <- patho.count(N.pop=n.pop[i], beta=0.04,recovery=1/30,death=0.001,birth=0.001,days=1000,N.shed=10^7,r=5,p=1/3)
        p.neighb.list[[i]] <- p.neighb
}

# output <- matrix(unlist(p.neighb.list), ncol = 1000, byrow = TRUE)


# liquid waste and time for d0 ... calculate N1
liquid.waste <- neighb$waste_liquid_sewage

# decay from neighborhood to drain + % liquid waste ... N1
decay.d0 <- c()
for (v in 1:length(p.neighb.list)){
        N0 <- list()
        N00 <- c()
        # pathogens that are disposed via liquid waste into sewer
        N0[[v]] <- round(p.neighb.list[[v]] * liquid.waste[v])
        
        # decay of pathogens to the "main" drain
        for (i in 1:lengths(p.neighb.list[v])) {
                N00[i] <- ex_decay(N0[[v]][i], dist2sewagedf$time[v])
        }
        
        decay.d0[[v]] <- N00
}


# # check math
# pathogens <- round(p.neighb.list[[1]][1] * neighb$waste_liquid_sewage[1])
# pathogens <- ex_decay(pathogens, dist2sewagedf$time[1])
# pathogens
# decay.d0[[1]][1]
# # --> check


# use pathogen values from previous step
p.neighb.list <- decay.d0

# decay
# repeat same step as above, decay of pathogens when inside drain until endpoint is reached
# time needed to reach end point (based on assumptions above)
decay.neighb <- c()
time.total <- c()

for (u in 1:length(p.neighb.list)){
        decay.1.neighb <- c()
                
        # for one neighborhood, sum of all lenths until end point = calc sum of time
        for (i in 1:lengths(p.neighb.list[u])) {
                decay.1.neighb[i] <- ex_decay(p.neighb.list[[u]][i], sum(df$time[df$iteration == u ]) )
        }
        
        # time total for each point to reach endpoint in hours
        time.total[u] <- sum(df$time[df$iteration == u ])
        
        # for all neighborhoods
        decay.neighb[[u]] <- decay.1.neighb 
}

dist2sewagedf$time
decay.neighb

# time in drain/ terrain for each neighborhood
summary.df <- data.frame("time_drain"=c(NA), "time_terrain"=c(NA), "meter_drain"=c(NA), "pathogens_day1"=c(NA), "iteration"=c(NA))
for (i in 1:max(df$iteration)){
        summary.df[i, ] <- c(sum(df$time[df$iteration == i]), dist2sewagedf$time[i], sum(df$length[df$iteration == i]), 
                             decay.neighb[[i]][1], i)
}

summary.df$time_drain <- round(summary.df$time_drain, 2)
summary.df$time_terrain <- round(summary.df$time_terrain, 2)
summary.df$meter_drain <- round(summary.df$meter_drain, 2)




###### part to figure out how to calculate pathogens per drain section

#N1
pathog.drain <- decay.d0

pathog.drain.matrix <- matrix(unlist(pathog.drain), ncol = 1000, byrow = TRUE)

#sort df by origin to end point for calculation
df <- df[with(df, order(iteration, -drain)), ]





pathog.drain.matrix[, 1]

times <- df$time[df$iteration == 1]
drains <- df$drain[df$iteration == 1]
length(drains)
dat <- ex_decay(pathog.drain.matrix[1, 1], times[1])
dat2 <- ex_decay(dat, times[2])
dat3 <- ex_decay(dat2, times[3])
dat3






pathog.drain.parts.all <- data.frame("pathogens"=c(), "drain"=c())

for (u in unique(df$iteration)) {
        
}
hours <- df$time[df$iteration == 2]
drains <- df$drain[df$iteration == 2]

pathog.drain.parts <- data.frame(matrix(nrow = length(drains), ncol = 2))
colnames(pathog.drain.parts) <- c("pathogens", "drain")

for (i in 1:length(drains)){
        if (i == 1) {
                pathog.drain.parts[i, ] <- c(ex_decay(pathog.drain.matrix[2, i], hours[i]), drains[i])
        } else {
                pathog.drain.parts[i, ] <- c(ex_decay(pathog.drain.parts[(i-1), 1], hours[i]), drains[i])
        }
}
pathog.drain.parts.all <- rbind(pathog.drain.parts.all, pathog.drain.parts)




i =2
i-1
dat <- ex_decay(pathog.drain.matrix[2, 1], times[1])
dat2 <- ex_decay(dat, times[2])
dat3 <- ex_decay(dat2, times[3])
dat3





ex_decay(pathog.drain.matrix[1, 1], df$time[df$iteration == 1 & df$drain == drains[1]])













# initial number of pathogen in first drain
pathog.initial <- data.frame("pathogen"=c(NA), "drain"=c(NA), "iteration"=c(NA))

for (i in unique(df$iteration)){
        df %>% filter(iteration == i) %>%
        .$drain -> drains
        # pathogen count, and time
        # initial drain
        pathog.initial[i,] <- c(ex_decay(pathog.drain[[i]][1], df$time[df$iteration == i & df$drain == max(drains)]), 
                                max(drains), i)
        # next drains
        
        
        }








# test bla bla blaaaaaaaaaaaaa   
for (i in unique(df$iteration)){
        df %>% filter(iteration == 1) %>%
                .$drain -> drains
        # remove first element of drain chain
        drains <- drains[drains != max(drains)]
        length(drains)
        pathog.drains <- c()
        for (u in 1:length(drains)){
        # first 1 is the iteration, second one first row ...i..............................i.....................u
        pathog.drains[1] <- ex_decay(pathog.initial[pathog.initial$iteration == 1, 1], 
                                  df$time[df$iteration == 1 & df$drain == drains[1]])   
        pathog.drains[2] <- ex_decay(pathog.drains[1], 
                                     df$time[df$iteration == 1 & df$drain == drains[1]])  
        }
        
}

patho.part[1, ] <- c(ex_decay(path.drain[[1]][1], df$time[df$iteration == 1 & df$drain == 14]), 14)
patho.part[2, ] <- c(ex_decay(patho.part[1,1], df$time[df$iteration == 1 & df$drain == 2]), 2)
patho.part[3, ] <- c(ex_decay(patho.part[2,1], df$time[df$iteration == 1 & df$drain == 1]), 1)

# neighb1, day1
df$drain[df$iteration == 2]
df$time[df$iteration == 1 & df$drain == 14]

#    [[neighb]][day]
path.drain[[2]][1]

# pathogens for each step
# function structure: ex_decay(pathogen, time)
pathoN01 <- path.drain[[1]][1]
patho14 <- ex_decay(pathoN01, df$time[df$iteration == 1 & df$drain == 14])
patho02 <- ex_decay(patho14, df$time[df$iteration == 1 & df$drain == 2])
patho01 <- ex_decay(patho02, df$time[df$iteration == 1 & df$drain == 1])
patho01

patho01 <- c()
get(paste0("patho0", 1)) <- 1


patho.part <- data.frame("patho" = c(NA), "drain" = c(NA))
for (i in 1:nrow(df)) { #drain parts
        for (u in 1:max(df$iteration)){ #neighb
                for (v in df$drain[df$iteration == u]) { #drains
                        patho.part[i, ] <- c(ex_decay(path.drain[[u]][1], df$time[df$iteration == u & df$drain == v]), v)
                                        }
        }
}


patho.part <- data.frame("patho" = c(NA), "drain" = c(NA))
for (i in 1:3) { #drain parts
        # for (u in 2){ #neighb
                for (v in length(df$drain[df$iteration == 1])) { #drains
                        patho.part[i, ] <- ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == df$drain[df$iteration == 1]])
                }
        }
}



patho.part[i, ] <- c(ex_decay(pathoN01, df$time[df$iteration == u & df$drain == v]), v)

for (v in df$drain[df$iteration == u]){
        print(v)
}


patho.part[1, ] <- c(ex_decay(path.drain[[1]][1], df$time[df$iteration == 1 & df$drain == 14]), 14)
patho.part[2, ] <- c(ex_decay(patho.part[1,1], df$time[df$iteration == 1 & df$drain == 2]), 2)
patho.part[3, ] <- c(ex_decay(patho.part[2,1], df$time[df$iteration == 1 & df$drain == 1]), 1)

patho.part[4, ] <- c(ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == 1]), 1)
patho.part[5, ] <- c(ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == 2]), 2)
patho.part[6, ] <- c(ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == 3]), 3)
patho.part[7, ] <- c(ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == 4]), 4)
patho.part[8, ] <- c(ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == 5]), 5)
patho.part[9, ] <- c(ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == 6]), 6)
patho.part[10, ] <- c(ex_decay(path.drain[[2]][1], df$time[df$iteration == 2 & df$drain == 22]), 22)

sum(patho.part$patho[patho.part$drain == 1])
sum(patho.part$patho[patho.part$drain == 2])
sum(patho.part$patho[patho.part$drain == 3])
