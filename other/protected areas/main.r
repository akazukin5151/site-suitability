# make a new dir called data and copy over the
# step_union_reproj_with_dummy.x files
# you can generate them by going to vectorProximityFromFiles and change
# the stepWrapper to DontRemoveStepDir

library(sf)
pdf(NULL)

shp <- read_sf("./data/step_union_reproj_with_dummy.shp")

#buff100 <- st_buffer(shp, 100)
#png("100.png")
#plot(buff100[, 'FID'], main="100m")

#buff500 <- st_buffer(shp, 500)
#png("500.png")
#plot(buff500[, 'FID'], main="500m")

#buff1000 <- st_buffer(shp, 1000)
#png("1000.png")
#plot(buff1000[, 'FID'], main="1000m")

#buff2000 <- st_buffer(shp, 2000)
#png("2000.png")
#plot(buff2000[, 'FID'], main="2000m")

#buff3000 <- st_buffer(shp, 3000)
#png("3000.png")
#plot(buff3000[, 'FID'], main="3000m")

#buff4000 <- st_buffer(shp, 4000)
#png("4000.png")
#plot(buff4000[, 'FID'], main="4000m")

#buff5000 <- st_buffer(shp, 5000)
#png("5000.png")
#plot(buff5000[, 'FID'], main="5000m")

buff10000 <- st_buffer(shp, 10000)
png("10000.png")
plot(buff10000[, 'FID'], main="10000m")

dev.off()
