# make a new dir called data and copy over the
# step_union_reproj_with_dummy.x files
# you can generate them by going to vectorProximityFromFiles and change
# the stepWrapper to DontRemoveStepDir

library(sf)
library(ggplot2)

zip <- function(...) {
  mapply(list, ..., SIMPLIFY = FALSE)
}

shp <- read_sf("./data/step_union_reproj_with_dummy.shp")

dists <- c(
    1000, 5000#, 3000, 4000, 5000,
    #10000, 20000, 30000, 40000, 50000
)

palette <- rainbow(length(dists))

plotted <- FALSE

plot <- ggplot()

for (x in zip(palette, dists)) {
    color <- x[[1]]
    dist <- x[[2]]
    dist_str <- toString(dist)
    buf <- st_buffer(shp, dist)
    plot <- plot + geom_sf(data=buf, fill=NA, color=color)
}

# ggplot doesn't like plotting legends manually

ggsave('out.png')
