library(raster)
library(rgdal)
pdf(NULL)

# These images are from the out dir (not public; adapt path if needed)
a <- '../../out/asakareh/final_clipped.tif'
w <- '../../out/watson/final_clipped.tif'
s <- '../../out/suh/final_clipped.tif'
sr <- '../../out/suh_range/final_clipped.tif'

r <- raster(x = s)
rs <- sample(r, size=10000)
# replace NA with 0
rs[is.na(rs[])] <- 0
write.csv(rs, 'data/s_rs.csv')
png("out/suh_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Suh)")

r <- raster(x = sr)
rs <- sample(r, size=10000)
# replace NA with 0
rs[is.na(rs[])] <- 0
write.csv(rs, 'data/sr_rs.csv')
png("out/suh_range_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Suh range)")

r <- raster(x = a)
rs <- sample(r, size=10000)
# replace NA with 0
rs[is.na(rs[])] <- 0
write.csv(rs, 'data/a_rs.csv')
png("out/a_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Asakereh)")

r <- raster(x = w)
rs <- sample(r, size=10000)
# replace NA with 0
rs[is.na(rs[])] <- 0
write.csv(rs, 'data/w_rs.csv')
png("out/w_hist.png")
hist(rs[rs != 0], xlab="Suitability score", main="Histogram of suitability scores (Watson)")

dev.off()
