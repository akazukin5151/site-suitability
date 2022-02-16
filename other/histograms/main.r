library(raster)
library(rgdal)
pdf(NULL)

# These images are from the out dir (not public; adapt path if needed)
files <- list(
    '../../out/asakareh/final_clipped.tif',
    '../../out/watson/final_clipped.tif',
    '../../out/suh/final_clipped.tif',
    '../../out/suh_improved/final_clipped.tif'
)

names <- list(
    'asakareh',
    'watson',
    'suh',
    'suh_improved'
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
