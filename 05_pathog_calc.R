#***********************************************************************************************
# Drains & Neighborhoods - Accra, Ghana
#***********************************************************************************************
#  [1] calculate length of drains
#       write: drains_order.csv
#  [2] calculate order of drains for travel from neighborhood to endpoint
#  [3] calculate pathogen count for neighborhood and each following drain/ point
#       write: pahtogens_drain_all_new.csv
#  [ ] visualizations in 06_ggplot_plotting.R
#
#***********************************************************************************************
# Note: data abreviations
# .sp  ... spatially adjusted with spTransform
# .prj ... projected with CRS("+init=epsg:32630")
#***********************************************************************************************

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

source(paste0(getwd(), "/", "01_helper_functions.R"))
source(paste0(getwd(), "/", "02_calc_options.R"))
source(paste0(getwd(), "/", "03_length_calc.R"))

# run - length of drain travel calculation
df <- point2end(neighbcoord)

df <- df[with(df, order(iteration, -drain)), ]
write.csv(df, paste0(getwd(), "/data/", "drains_order.csv"), row.names = F)


#***********************************************************************************************
#***********************************************************************************************
# Endemic Part ----
source(paste0(getwd(), "/", "04_endemic_model.R"))

# Note: neighborhood to drain: liquid sewage + decay

# we assume a water/ drain flow rate of 10,000m per day
# 10,000m/day = 0.417km/h = 0.116m/sec
v <- 10000
df$time <- df$length / v * 24

# assumption of flow rate from neighborhood to drain 1,000m per day
v.d0 <- 1000
dist2sewagedf$time <- (dist2sewagedf$distance / v.d0)  * 24 
#***********************************************************************************************


# pathogens for each neighborhood for 1000 days
pathog.neighb <- c()
pathog.neighb.list <- list()
n.pop <- neighb$p_total

for (i in 1:length(n.pop)){
        pathog.neighb <- patho.count(N.pop=n.pop[i], beta=0.04, recovery=1/30, death=0.001, 
                                birth=0.001, days=1000, N.shed=10^7, r=5, p=1/3)
        pathog.neighb.list[[i]] <- pathog.neighb
}

N0df <- matrix(unlist(pathog.neighb.list), ncol = nrow(neighb), byrow = F) #N0
#***********************************************************************************************


# liquid waste and time for d0 ... calculate N1
liquid.waste <- neighb$waste_liquid_sewage

# create matrix for all adjustement factors for all neighborhoods
adjust.fac.matrix <- matrix(ncol = length(N0df[1, ]), nrow = 1000)
for (i in 1:length(N0df[1, ])) {
        adjust.fac.matrix[, i] <- rbeta(length(N0df[, i]), 1, 5)
}

###
data.list <- list()
for (u in 1:length(N0df[1, ])) {
        drains <- df[df$iteration == u, ]
        ndrains <- nrow(drains) # df$drain[df$iteration == u]
        ncolumns <- sum(nrow(drains)+2)
        t0 <- dist2sewagedf$time[u]

        data <- data.frame(matrix(ncol=ncolumns, nrow=1000))
        # number of drains + neighborhood concentration --- defines number of columns
        for (i in 1:sum(nrow(drains)+2)){ #one neighborhood
                if (i == 1){
                        data[, i] <- N0df[, u]
                }
                if (i == 2){
                        data[, i] <- N_next(N0df[, u], t0,
                                            liquid.waste[u], adj_fac = adjust.fac.matrix[, u])
                } 
                if (i > 2){
                        data[, i] <- N_next(N0df[, u], t0 + sum(drains$time[1:sum(i-2)]),
                                            liquid.waste[u], adj_fac = adjust.fac.matrix[, u])
                }
        }
        colnames(data) <- c("N0", paste("N", 1:(nrow(drains)+1), sep=""))
        data.list[[u]] <- data
        ###
}
#***********************************************************************************************


# calculate/ aggregate all the drains/points 
# transpose matrix and add columns to identify drain
pathog.drains <- data.frame()

for (u in 1:length(N0df[1, ])){
        dat <- data.list[[u]]
        # dat <- data.list[[u]][,-c(1,2)] #delete N0 and N1
        dat <- data.frame(t(dat)) 
        dat$point <- rownames(dat)
        dat$drain <- c(0, 0, df$drain[df$iteration == u])
        dat$time <- c(0, dist2sewagedf$time[u], df$time[df$iteration == u])
        dat$meters <- c(0, dist2sewagedf$distance[u], df$length[df$iteration == u])
        dat$iteration <- u
        dat <- dat %>%
                dplyr::select(drain, point, time, meters, iteration, everything())
        pathog.drains <- rbind(pathog.drains, dat)
}

rownames(pathog.drains) <- NULL
#***********************************************************************************************

pathog.drains$time <- round(pathog.drains$time, 2)
pathog.drains$meters <- round(pathog.drains$meters, 2)

write.csv(pathog.drains, paste0(getwd(), "/data/", "pahtogens_drain_all_new.csv"), row.names = F)

# # pathogen in drain per day
# summary <- pathog.drains %>% dplyr::select(-c(point, time, meters, iteration) ) %>% group_by(drain) %>%
#         filter(!drain==0) %>% summarise_each(funs(sum))

