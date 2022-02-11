# Plots the rasters with existing solar farm locations

library(raster)
pdf(NULL)

existing <- read.csv(
    '../existing_solar_farms/existing_solar_farms.csv'
)
e_x = existing[, 'centroid_x']
e_y = existing[, 'centroid_y']

# These images are from the out dir (not public; adapt path)
files <- list(
    '../../out/asakareh/final_clipped.tif',
    '../../out/watson/final_clipped.tif',
    '../../out/suh/final_clipped.tif',
    '../../out/suh_range/final_clipped.tif',
    '../../out/suh_range_no_elevation/final_clipped.tif',
    '../../out/suh_range_no_elevation/final_std.tif'
)

titles <- list(
    'asakereh',
    'watson',
    'suh',
    'suh_range',
    'suh_range_no_elevation',
    'suh_range_no_elevation_noclip'
)

f <- function(file, title) {
    outfile <- paste('out/', title, '.png', sep='')
    if (!file.exists(outfile)) {
        print(outfile)
        r <- raster(x = file)
        png(outfile)
        plot(r, main=title)
        points(x=e_x, y=e_y, pch='.', col='red', cex=5)
    }
}

mapply(f, files, titles)

dev.off()
