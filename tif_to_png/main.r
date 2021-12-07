# Plots the rasters with existing solar farm locations

library(raster)
pdf(NULL)

# These images are from the out dir (not public; adapt path)
a <- '../asakareh_final.tif'
w <- '../watson_final.tif'
s <- '../../../suh/final_clipped.tif'
existing <- read.csv(
    '../../../../other/existing_solar_farms/existing_solar_farms.csv'
)
x = existing[, 'centroid_x']
y = existing[, 'centroid_y']


r <- raster(x = s)
png("suh.png")
plot(r, main="Suh")
points(x=x, y=y, pch='.', col='red', cex=5)

r <- raster(x = a)
png("asakereh.png")
plot(r, main="Asakareh")
points(x=x, y=y, pch='.', col='red', cex=5)

r <- raster(x = w)
png("watson.png")
plot(r, main='Watson')
points(x=x, y=y, pch='.', col='red', cex=5)

dev.off()
