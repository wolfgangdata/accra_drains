# function

point2end <- function(neighbcoord){
        
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
        
        pts.neighb.sp <<- SpatialPointsDataFrame(neighbcoord[, c("lon", "lat")], 
                                                data.frame(ID=seq(1:nrow(neighbcoord))),
                                                proj4string=CRS("+proj=longlat +datum=WGS84"))
        
        # distance to sewage (shortest way to line), ID2: feature line intersects with = drain ID
        dist2sewagedf <- data.frame("distance" = c(NA),"lon" = c(NA), "lat" = c(NA), "ID2" = c(NA))
        
        # distance + coordinates of intersection + drain ID
        for (i in 1:length(neighbcoord[ ,1])) {
                dist2sewagedf[i, ] <- rbind(dist2Line(neighbcoord[i, c("lon", "lat")], sewage)) 
        }
        
        pts.neigh.drain <<- SpatialPointsDataFrame(dist2sewagedf[c("lon", "lat")], 
                                          data.frame(ID=seq(1:nrow(dist2sewagedf)), ID2 = dist2sewagedf$ID2),
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))
        pts.neigh.drain.sp <<- spTransform(pts.neigh.drain, CRS( "+init=epsg:32630" ))
        
        
        # calculate each small part of id'd section. from intersection to end point of section.
        sewage.part.length.df <- data.frame("length" = c(NA), "drain" = c(NA), "iteration" = c(NA))
        
        #setup circle/ line to create spatial polygons/ lines data frame & then later delete this first entry
        circles.prj1 <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[1],], width=d, byid=TRUE)
        circles.prj1 <- spTransform(circles.prj1, CRS('+proj=longlat +datum=WGS84'))
        circles.prj1$iteration <- 99
        sewage.part <- gIntersection(circles.prj1[1,], sewage[sewage$id2 == circles.prj1$ID[1], ], byid=c(TRUE, TRUE))
        sewage.part$drainID <- 99
        sewage.part$iteration <- 99
        
        for (i in 1:length(pts.neigh.drain)){
                d <- gDistance(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i], ], pts.neigh.drain.sp[i, ])
                circles.prj <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i],], width=d, byid=TRUE)
                sewage.part.length.df[i, ] <- c(line.length(sewage.prj[pts.neigh.drain.sp$ID2[i], ], circles.prj), 
                                                pts.neigh.drain.sp$ID2[i], i)
                
                circles.prj <- spTransform(circles.prj, CRS('+proj=longlat +datum=WGS84'))
                circles.prj$iteration <- i
                circles.prj1 <- rbind(circles.prj1, circles.prj)
                
                
                d <- gDistance(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i], ], pts.neigh.drain.sp[i, ])
                circles.prj <- gBuffer(pts.drain.end.prj[pts.neigh.drain.sp$ID2[i],], width=d, byid=TRUE)
                circles.prj <- spTransform(circles.prj, CRS('+proj=longlat +datum=WGS84'))
                line2 <- gIntersection(circles.prj, sewage[sewage$id2 == circles.prj$ID, ], byid=c(TRUE, TRUE))
                line2$drainID <- pts.neigh.drain.sp$ID2[i]
                line2$iteration <- i
                sewage.part <- rbind(sewage.part, line2)
        }
        
        # delete initial entries from above
        circles.prj1 <- circles.prj1[!circles.prj1$iteration == 99, ]
        sewage.part <<- sewage.part[!sewage.part$iteration == 99, ]
        
        
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
        
        
        
}
