

substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
}

# visualization of endpoint pathogen concentrations
a <- quantile(pathog.endpoint.sum$sum)[1]
b <- quantile(pathog.endpoint.sum$sum)[2]
c <- quantile(pathog.endpoint.sum$sum)[3]
d <- quantile(pathog.endpoint.sum$sum)[4]
e <- quantile(pathog.endpoint.sum$sum)[5]

endpoint <- data.frame("lon" = c(-0.221109), "lat" = c(5.530075))


pathog.endpoint.sum <- data.frame("sum"=colSums(pathog.endpoint.matrix))
pathog.endpoint.sum$color <- "black"
pathog.endpoint.sum$color[pathog.endpoint.sum$sum > a & pathog.endpoint.sum$sum < b] <- "green"
pathog.endpoint.sum$color[pathog.endpoint.sum$sum > b & pathog.endpoint.sum$sum < c] <- "yellow"
pathog.endpoint.sum$color[pathog.endpoint.sum$sum > c & pathog.endpoint.sum$sum < d] <- "orange"
pathog.endpoint.sum$color[pathog.endpoint.sum$sum > d & pathog.endpoint.sum$sum < e] <- "red"

pathog.endpoint.sum$cex[pathog.endpoint.sum$sum > a & pathog.endpoint.sum$sum < b] <- 1
pathog.endpoint.sum$cex[pathog.endpoint.sum$sum > b & pathog.endpoint.sum$sum < c] <- 2
pathog.endpoint.sum$cex[pathog.endpoint.sum$sum > c & pathog.endpoint.sum$sum < d] <- 3
pathog.endpoint.sum$cex[pathog.endpoint.sum$sum > d & pathog.endpoint.sum$sum < e] <- 4

total <- 100
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)

for (i in 1:100){
        png(paste0(getwd(), "/plots_endpoint/", paste0("map_", substrRight(paste0("0000", i),4),".png")) , width = 6, height = 7, units = 'in', res = 300)
        plot(sewage, col = "lightblue", axes=TRUE)
        title(main = "Pathogen Concentration at End Point", sub = paste0("Day ", i))
        points(endpoint, cex=pathog.endpoint.sum$cex[i], pch=16, col=pathog.endpoint.sum$color[i])
        dev.off()
        
        setTxtProgressBar(pb, i)
}
close(pb)


