library(raster)
library(rgdal)
pdf(NULL)

# These images are from the out dir (not public; adapt path if needed)
a <- '../../out/asakareh/final_clipped.tif'
w <- '../../out/watson/final_clipped.tif'
s <- '../../out/suh/final_clipped.tif'

r <- raster(x = s)
#plot(r)
rs <- sample(r, size=10000)
png("out/suh_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Suh)")

r <- raster(x = a)
rs <- sample(r, size=10000)
png("out/a_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Asakereh)")

r <- raster(x = w)
rs <- sample(r, size=10000)
png("out/w_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Watson)")

dev.off()
