#***********************************************************************************************
# Case Numbers
#***********************************************************************************************
#  [1] create 1-0 matrix, indicating infection or no infection per day per station
#***********************************************************************************************

library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

setwd("H:/GitHub/accra_drains/")


# load/ manipulate data
plot.stations <- read.csv(paste0(getwd(), "/data/", "plot_stations_new_all.csv"))
plot.stations1 <- plot.stations
minimum <- 10^7

plot.stations$infect <- as.numeric(plot.stations$value >= minimum)

# tst <- plot.stations[plot.stations$station == "A",]
# tst <- tst[tst$variable <= 200,]
# tst <- tst[tst$infect == 1,]
# 
# ggplot(data=tst, aes(x=variable, y=value)) +
#         geom_line() +
#         geom_point()



# plot.stations$variable <- NULL
# plot.stations1$value <- NULL

#***********************************************************************************************
# positive infection each day fore each station
rows <- nrow(plot.stations1[plot.stations1$station == "A",])

df <- data.frame(matrix(nrow=rows, ncol=5))

df[,1] <- as.numeric(plot.stations1$value[plot.stations1$station == "A"] > minimum)
df[,2] <- as.numeric(plot.stations1$value[plot.stations1$station == "B"] > minimum)
df[,3] <- as.numeric(plot.stations1$value[plot.stations1$station == "C"] > minimum)
df[,4] <- as.numeric(plot.stations1$value[plot.stations1$station == "D"] > minimum)
df[,5] <- as.numeric(plot.stations1$value[plot.stations1$station == "E"] > minimum)

colnames(df) <- c("A", "B", "C", "D", "E")
#***********************************************************************************************
# count how many times within 1000 days there is a positve sample
df$id <- 1:nrow(df)

df1 <- melt(df, id="id")
df1 <- df1 %>% filter(value ==1) %>% group_by(variable) %>% summarise(count=n(),
                                                                      perc=n()/1000)

ggplot(df1, aes(x=variable, y=count)) + 
        geom_bar(aes(fill = variable), alpha=.9, stat="identity") +
        geom_text(aes(label=count), vjust=-.2) +
        # geom_text(aes(label=scales::percent(perc)), vjust=-.2) +
        theme_bw() +
        scale_fill_brewer(palette="Set2", name="Stations") +
        labs(title = "Positive Samples (1000 Days)", 
             x = "Stations", y = "Positive Samples") +
        theme(plot.title=element_text(face="bold",size=25, hjust = 0.5), #title
              strip.text = element_text(size=12), #font size of facet wrap titles
              axis.text=element_text(size=12), #font size of labels
              axis.title=element_text(size=16))

ggsave(filename = paste0(getwd(), "/plots_misc/", "cases_pos_1000.png"),
       plot = last_plot(), width = 8, height = 6, units = 'in', dpi = 400)

#***********************************************************************************************
# determining sample frequency sensitivity
# take sample every nth day and check if sample is positive
df$id <- NULL

sample.intervals <- c(1,7,14,21,30,60)


df.interval <- data.frame(matrix(nrow=200, ncol=7))
colnames(df.interval) <- c("A", "B", "C", "D", "E", "interval", "samples")

for (i in 1:200){
        dat <- df[seq(0, nrow(df), i), ]
        df.interval[i,] <- c(round(colSums(dat) / nrow(dat),2), i, nrow(dat))
}

df.interval <- df.interval %>% filter(interval %in% sample.intervals)


#***********************************************************************************************


quantile(dff$A, c(.90, .95))
quantile(dff$B, c(.90, .95))
quantile(dff$C, c(.90, .95))
quantile(dff$D, c(.90, .95))
quantile(dff$E, c(.90, .95))

