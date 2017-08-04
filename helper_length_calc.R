# helper functions

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
