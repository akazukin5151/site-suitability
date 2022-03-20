library(raster)
library(rgdal)
pdf(NULL)

r <- raster(x = '../../out/suh/preprocessed/slope.tif')
rs <- r
rs[is.na(rs[])] <- 0
h <- hist(
    rs[rs != 0],
    xlab="Slope (angular degrees)",
    main="",
    xlim=c(1000, 21600),
    plot=FALSE
)
print(h)
#png('test.png')
#hist(
#    rs[rs != 0],
#    xlab="Slope (angular degrees)",
#    main="",
#    xlim=c(1000, 21600),
#)
#dev.off()
