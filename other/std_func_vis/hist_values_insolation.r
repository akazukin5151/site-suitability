library(raster)
library(rgdal)
pdf(NULL)

r <- raster(x = '../../out/suh/preprocessed/avg insolation.tif')
rs <- r
rs[is.na(rs[])] <- 0
h <- hist(
    rs[rs != 0],
    xlab="Solar radiation (kWh/m²/day)",
    main="",
    xlim=c(1000, 21600),
    plot=FALSE
)
print(h)
print(min(r, na.rm=T))
