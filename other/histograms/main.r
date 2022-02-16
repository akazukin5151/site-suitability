library(raster)
library(rgdal)
pdf(NULL)

# These images are from the out dir (not public; adapt path if needed)
files <- list(
    '../../out/asakareh/final_clipped.tif',
    '../../out/asakareh_no_res/final_clipped.tif',
    '../../out/watson/final_clipped.tif',
    '../../out/suh/final_clipped.tif',
    '../../out/suh_improved/final_clipped.tif',
    '../../out/suh_range_no_elevation/final_clipped.tif',
    '../../out/suh_range_no_elevation/final_std.tif'
)

names <- list(
    'asakareh',
    'asakareh_no_res',
    'watson',
    'suh',
    'suh_improved',
    'suh_range_no_elevation',
    'suh_range_no_elevation_noclip'
)

f <- function(file, name) {
    png_name <- paste('out/', name, '.png', sep='')
    csv_name <- paste('data/', name, '.csv', sep='')
    if (!file.exists(png_name) || !file.exists(csv_name)) {
        r <- raster(x = file)
        rs <- sample(r, size=10000)
        # replace NA with 0
        rs[is.na(rs[])] <- 0
        write.csv(rs, csv_name)
        png(paste('out/', name, '.png', sep=''))
        hist(
            rs[rs != 0],
            xlab="Suitability score",
            main="" #paste("Histogram of suitability scores", name)
        )
    }
}

mapply(f, files, names)

dev.off()
