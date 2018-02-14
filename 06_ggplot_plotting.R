#***********************************************************************************************
# Plotting for Accra Ghana Drain Simulation
#***********************************************************************************************
#  [1] summarize pathogen count for the 5 stations
#       write: plot_stations_new.csv
#  [2] create gif animation for pathogen count per station
#       save: pathog_station.gif
#  [3] create gif animation for time series pathogen count per station
#       save: station_timeplot_100days.gif
#  [4] create multiple maps and plots
#
#***********************************************************************************************
setwd("H:/GitHub/accra_drains/")

# libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggmap)
library(gganimate)
# devtools::install_github("dgrtwo/gganimate")
# library("gganimate", lib.loc="~/R/R-3.4.0/library")
library(raster)
library(broom)
library(RColorBrewer)
#***********************************************************************************************


# load/ manipulate data
pathog.drains <- read.csv(paste0(getwd(), "/data/", "pahtogens_drain_all_new.csv"))

# sewage shape file
sewage <- shapefile(paste0(getwd(), "/shapefiles/waterways/", "accra_waterw_con_ed_level_4.shp"))
sewage <- spTransform(sewage, CRS("+proj=longlat +datum=WGS84"))
sewage <- sewage[order(sewage@data$id2), ]


# neighborhood shape file
ama <- shapefile(paste0(getwd(), "/shapefiles/neighborhood/", "AMA_projected.shp"))
ama <- spTransform(ama, CRS("+proj=longlat +datum=WGS84"))
ama <- ama[order(ama$LOC_NAME), ]
ama$ID <- 1:71

# classification of neighborhoods
a <- c(6,8,9,17,28,33,34,35,67,68)
b <- c(1,30,39,45,62,71)
c <- c(2,3,4,5,7,11,13,14,15,19,21,23,31,38,48,51,53,66,70)
d <- c(10,16,29,32,42,52,57,58,59)
e <- c(12,24,25,36,37,40,41,46,50,55,60)

ama1.fort <- fortify(ama, region = "ID")
ama1.fort$color <- ifelse(ama1.fort$id %in% a, 1, 
                          ifelse(ama1.fort$id %in% b, 2,
                                 ifelse(ama1.fort$id %in% c, 3,
                                        ifelse(ama1.fort$id %in% d, 4,
                                               ifelse(ama1.fort$id %in% e, 5, NA)))))

detach("package:raster", unload=TRUE) #because of interferance with dplyr syntax

#
drain.endpoints <- read.csv(paste0(getwd(), "/data/", "drain_endpoints.csv"))

#
neighbcoord <- read.csv(paste0(getwd(), "/data/", "points_coordinates_output.csv"), sep = ",")
neighbcoord$FID <- 1:71
#***********************************************************************************************


# pathogen in drain per day
summary <- pathog.drains %>% dplyr::select(-c(point, time, meters, iteration) ) %>% group_by(drain) %>%
        filter(!drain==0) %>% summarise_each(funs(sum))


plot.df <- data.frame(t(summary))
plot.df <- melt(summary, id="drain")
plot.df$variable <- substring(plot.df$variable, 2)
plot.df$variable <- as.numeric(plot.df$variable)
#***********************************************************************************************


plot.df1 <- plot.df %>% filter(variable >= 1 & variable <= 100)
# plot.df1 <- plot.df  #all 1000 days

# create sampling stations: A, B, C, D, E
plot.df1A <- plot.df1 %>% filter(drain %in% c(1)) %>% select(-drain) %>% 
        group_by(variable) %>% summarise(value = sum(value)) %>% mutate(station = "A")
plot.df1B <- plot.df1 %>% filter(drain %in% c(14)) %>% select(-drain) %>% 
        group_by(variable) %>% summarise(value = sum(value)) %>% mutate(station = "B")
plot.df1C <- plot.df1 %>% filter(drain %in% c(15)) %>% select(-drain) %>% 
        group_by(variable) %>% summarise(value = sum(value)) %>% mutate(station = "C")
plot.df1D <- plot.df1 %>% filter(drain %in% c(5)) %>% select(-drain) %>% 
        group_by(variable) %>% summarise(value = sum(value)) %>% mutate(station = "D")
plot.df1E <- plot.df1 %>% filter(drain %in% c(16)) %>% select(-drain) %>% 
        group_by(variable) %>% summarise(value = sum(value)) %>% mutate(station = "E")

plot.stations <- bind_rows(plot.df1A, plot.df1B)
plot.stations <- bind_rows(plot.stations, plot.df1C)
plot.stations <- bind_rows(plot.stations, plot.df1D)
plot.stations <- bind_rows(plot.stations, plot.df1E)
levels(plot.stations) <- unique(plot.stations$station)
rm(plot.df1A, plot.df1B, plot.df1C, plot.df1D, plot.df1E)
#***********************************************************************************************

write.csv(plot.stations, paste0(getwd(), "/data/", "plot_stations_new.csv"), row.names = F)
#***********************************************************************************************


minimum <- 10^7
p <- ggplot(plot.stations, aes(x = station, y = value, frame=variable)) + # , frame=variable ... for gganimate
        geom_hline(yintercept = c(minimum), col = "snow4", linetype = 2) +
        geom_line(aes(group = variable)) +
        geom_point(shape = 21, colour = "white", size = 8, stroke = 2, 
                   aes(fill = value <= minimum, group = variable)) +
        theme_classic()  +
        scale_y_log10() +
        labs(title = "Pathogen Count per Station - Day",
             x = "Stations", y = "Pathogen Count (log10)") +
        theme(panel.grid.major.x= element_line(colour = "grey"),
              legend.position="none",
              plot.title=element_text(face="bold",size=25, hjust = 0.5), #title
              axis.text=element_text(size=14), #font size of labels
              axis.title=element_text(size=16))
p

gganimate(p, filename=paste0(getwd(), "/plots_animation/", "pathog_station_100.gif"), 
          ani.width=600, ani.height=600, dpi=1000, interval = .5)
#***********************************************************************************************


# # plot pathogen levels for all drains
# ggplot(plot.df1, aes(x = factor(drain), y = value)) + # , frame=variable ... for gganimate
#         geom_hline(yintercept = c(minimum), col = "grey", linetype = 2) +
#         geom_line(aes(group=variable)) +
#         geom_point(shape = 21, colour = "white", size = 5, stroke = 2,
#                    aes(fill = value >= minimum, group = variable)) +
#         theme_classic()  +
#         # scale_y_continuous(trans="log10") +
#         labs(title = "Pathogen Count per Drain - Day",
#              caption = "Simulation for Accra, Ghana",
#              x = "Drains", y = "# Pathogen") +
#         theme(legend.position="none")
#***********************************************************************************************


# animation
# time series for each station
# ggplot(data=plot.stations[plot.stations$station == "A", ], aes(x=variable, y=value, color=station)) +
plot.stations1 <- filter(plot.stations, variable <= 100)

timeplot <- ggplot(data=plot.stations1, 
                   aes(x=variable, y=value, color=station, group=1, frame=variable)) + #, frame=variable
        geom_path(aes(cumulative = TRUE, group = 1)) + #cumulative = TRUE,
        geom_point() +
        geom_hline(yintercept = c(minimum), col = "snow4", linetype = 2) +
        scale_y_log10() +
        theme_bw() +
        scale_color_brewer(palette="Set2") +
        facet_wrap(~station) +
        labs(title = "Pathogen Count per Station - Day",
             x = "Days", y = "Pathogen Count (log10)") +
        theme(legend.position="none",
              plot.title=element_text(face="bold",size=25, hjust = 0.5), #title
              strip.text = element_text(size=12), #font size of facet wrap titles
              axis.text=element_text(size=12), #font size of labels
              axis.title=element_text(size=16))

gganimate(timeplot, filename=paste0(getwd(), "/plots_animation/", "station_timeplot_100days.gif"), 
          ani.width=600, ani.height=600, dpi=1000, interval = .5)

## static graphs
ggplot(data=plot.stations1, 
       aes(x=variable, y=value, color=station, group=1)) + 
        geom_path() + 
        geom_hline(yintercept = c(minimum), col = "snow4", linetype = 2) +
        # geom_text(aes(0, minimum, label = "10^7", hjust=-0.2, vjust = -1), color="black") +
        scale_y_log10() +
        theme_bw() +
        facet_wrap(~station) +
        scale_color_brewer(palette="Set2") +
        labs(title = "Pathogen Count per Station - Simulation", 
             x = "Days", y = "Pathogen Count (log10)") +
        theme(legend.position="none",
              plot.title=element_text(face="bold",size=25, hjust = 0.5), #title
              strip.text = element_text(size=12), #font size of facet wrap titles
              axis.text=element_text(size=12), #font size of labels
              axis.title=element_text(size=16))

ggsave(filename = paste0(getwd(), "/plots_misc/", "survaillance_stations_timeseries_100days_new.png"),
       plot = last_plot(), width = 9.5, height = 6.5, dpi=350)
#***********************************************************************************************


# bloxplot
ggplot(plot.stations, aes(x=station, y=value)) + # 
        # geom_point(shape = 21, colour = "white", size = 2, stroke = 2, aes(fill=factor(station), group=variable)) +
        # geom_point() + 
        theme_classic()  +
        scale_x_discrete(limits = rev(levels(plot.stations))) +
        labs(title = "Pathogen Count per Station - Day",
             caption = "Simulation for Accra, Ghana", 
             x = "Stations", y = "# Pathogen") +
        theme(legend.position="none") + 
        geom_boxplot(aes(fill = station))
#***********************************************************************************************


# basemap <- (get_map(unlist(geocode("Accra")), zoom=13))
basemap <- get_map(location = c(long=-0.2169639, lat=5.585909), zoom=12) # define center point on map
basemap_bw <- get_map(location = c(long=-0.2169639, lat=5.585909), zoom=12, color = "bw") # define center point on map
basemap_big <- get_map(location = c(long=-0.2169639, lat=5.585909), zoom=6) # define center point on map
basemap_big2 <- get_map(location = c(long=-0.2169639, lat=5.585909), zoom=4) # define center point on map
#***********************************************************************************************


# map - blank
ggmap(basemap) + coord_map() +
        theme_bw() +
        labs(title = "Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = paste0(getwd(), "/plots_misc/", "map_blank.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************


# map - blank - zoomed out
ggmap(basemap_big2) + coord_map() +
        theme_bw() +
        labs(title = "Gulf of Guinea",
             x = "Longitude", y = "Latitude") +
        theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = paste0(getwd(), "/plots_misc/", "map_blank_w_africa.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************


# map - neighb color
ggmap(basemap) +
        geom_polygon(data=ama1.fort, aes(x=long, y=lat, fill=factor(id), group=group),
                     colour="white", alpha=.7) +
        # geom_point(data=neighbcoord, aes(x=lon, y=lat), color="grey") +
        # geom_path(data=sewage, aes(x=long, y=lat, group=group), color="darkblue", size=1) +
        coord_map() +
        theme_bw() +
        labs(title = "Neighborhoods - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(legend.position="none",
              plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(getwd(), "/plots_misc/", "map_neighb.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************


# map - neighb color + neighb name
ggmap(basemap) +
        geom_polygon(data=ama1.fort, aes(x=long, y=lat, fill=factor(id), group=group),
                     colour="white", alpha=.7) +
        geom_text(data=neighbcoord, aes(label=FID),hjust=.5, vjust=0) +
        coord_map() +
        theme_bw() +
        labs(title = "Neighborhoods - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(legend.position="none",
              plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(getwd(), "/plots_misc/", "map_neighb_name.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************


# map - neighb color
ggmap(basemap) +
        geom_path(data=sewage, aes(x=long, y=lat, group=group), color="darkblue", size=1) +
        coord_map() +
        theme_bw() +
        labs(title = "Drain Network - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(legend.position="none",
              plot.title = element_text(hjust = 0.5))


ggsave(filename = paste0(getwd(), "/plots_misc/", "map_drain.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************


# map - neighb color + drain
ggmap(basemap_bw) +
        geom_polygon(data=ama1.fort, aes(x=long, y=lat, fill=factor(color), group=group),
                     colour="white", alpha=.7) +
        geom_path(data=sewage, aes(x=long, y=lat, group=group), color="darkblue", size=1) +
        scale_fill_brewer(palette="Set2", name="Neighb.",
                          labels=c("A","B","C","D","E", "")) +
        coord_map() +
        theme_bw() +
        # theme(legend.position="none") +
        labs(title = "Neighborhoods and Drain Network - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0(getwd(), "/plots_misc/", "map_neighb_drain.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************

# neighb color + drain
ggmap(basemap_bw) +
        geom_polygon(data=ama1.fort, aes(x=long, y=lat, fill=factor(id), group=group),
                     colour="white", alpha=.7) +
        geom_point(data=neighbcoord, aes(x=lon, y=lat), color="black") +
        geom_path(data=sewage, aes(x=long, y=lat, group=group), color="darkblue", size=1) +
        # scale_fill_brewer(palette="Set2", name="Neighb.",
                          # labels=c("A","B","C","D","E", "")) +
        coord_map() +
        theme_bw() +
        theme(legend.position="none") +
        labs(title = "Neighborhoods and Drain Network - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = paste0(getwd(), "/plots_misc/", "map_neighb_drain_mean.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************


# neighb color + drain
ggplot() +
        geom_polygon(data=ama1.fort, aes(x=long, y=lat, fill=factor(color), group=group),
                     colour="white", alpha=.7) +
        # geom_point(data=neighbcoord, aes(x=lon, y=lat), color="grey") +
        geom_path(data=sewage, aes(x=long, y=lat, group=group), color="darkblue", size=1) +
        scale_fill_brewer(palette="Set2", name="Neighb.",
                          labels=c("A","B","C","D","E", "")) +
        coord_map() +
        theme_bw() +
        # theme(legend.position="none") +
        labs(title = "Neighborhoods and Drain Network - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = paste0(getwd(), "/plots_misc/", "map1_neighb_drain.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************


# neighb color + drain + neigh point
ggplot() +
        geom_polygon(data=ama1.fort, aes(x=long, y=lat, fill=factor(color), group=group),
                     colour="black", alpha=.7) +
        geom_point(data=neighbcoord, aes(x=lon, y=lat), color="black") +
        geom_path(data=sewage, aes(x=long, y=lat, group=group), color="darkblue", size=1) +
        scale_fill_brewer(palette="Set2", name="Neighb.",
                          labels=c("A","B","C","D","E", "")) +
        coord_map() +
        theme_bw() +
        # theme(legend.position="none") +
        labs(title = "Neighborhoods and Drain Network - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = paste0(getwd(), "/plots_misc/", "map1_neighb_drain_mean.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************

neighbcoord1 <- neighbcoord %>% filter(!FID %in% c(18,54,64,69,61,22,65,49,56,20,43,63,44,26,47,27))
# neighb color + drain + neigh point + clean
ggplot() +
        geom_polygon(data=ama1.fort, aes(x=long, y=lat, fill=factor(color), group=group),
                     colour="white", alpha=.7) +
        geom_point(data=neighbcoord1, aes(x=lon, y=lat), color="black") +
        geom_path(data=sewage, aes(x=long, y=lat, group=group), color="darkblue", size=1) +
        scale_fill_brewer(palette="Set2", name="Neighb.",
                          labels=c("A","B","C","D","E", "")) +
        coord_map() +
        theme_bw() +
        # theme(legend.position="none") +
        labs(title = "Neighborhoods and Drain Network - Accra, Ghana",
             x = "Longitude", y = "Latitude") +
        theme(plot.title = element_text(hjust = 0.5))


ggsave(filename = paste0(getwd(), "/plots_misc/", "map1_neighb_drain_mean_clean.png"),
       plot = last_plot(), width = 6, height = 6, units = 'in', dpi = 400)
#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************

#population density
neighb <- read.csv(paste0(getwd(), "/data/", "neighborhoods_all_clean.csv"), sep = ",")

ama1.fort$id <- as.numeric(ama1.fort$id)
neighb <- neighb[order(neighb$Region), ]
neighb$id <- 1:71

neighb1 <- select(neighb, id, Neighborhood, waste_liquid_sewage)
ama1.fort11 <- left_join(ama1.fort, neighb1, by="id")


ggplot() +
        geom_polygon(data=ama1.fort11, aes(x=long, y=lat, group=group, fill=(waste_liquid_sewage)),
                     colour="white", alpha=.7)
