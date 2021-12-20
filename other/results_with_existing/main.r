# Plots the rasters with existing solar farm locations

library(raster)
pdf(NULL)

# These images are from the out dir (not public; adapt path)
a <- '../../out/asakareh/final_clipped.tif'
w <- '../../out/watson/final_clipped.tif'
s <- '../../out/suh/final_clipped.tif'
existing <- read.csv(
    '../existing_solar_farms/existing_solar_farms.csv'
)
x = existing[, 'centroid_x']
y = existing[, 'centroid_y']


r <- raster(x = s)
png("out/suh.png")
plot(r, main="Suh")
points(x=x, y=y, pch='.', col='red', cex=5)

r <- raster(x = a)
png("out/asakereh.png")
plot(r, main="Asakareh")
points(x=x, y=y, pch='.', col='red', cex=5)

r <- raster(x = w)
png("out/watson.png")
plot(r, main='Watson')
points(x=x, y=y, pch='.', col='red', cex=5)

dev.off()
