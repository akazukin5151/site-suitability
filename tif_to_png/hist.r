library(raster)
library(rgdal)
pdf(NULL)

# These images are from the out dir (not public; adapt path)
a <- '../asakareh_final.tif'
w <- '../watson_final.tif'
s <- '../../../suh/final_clipped.tif'

r <- raster(x = s)
#plot(r)
rs <- sample(r, size=10000)
png("suh_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Suh)")

r <- raster(x = a)
rs <- sample(r, size=10000)
png("a_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Asakereh)")

r <- raster(x = w)
rs <- sample(r, size=10000)
png("w_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Watson)")

dev.off()
