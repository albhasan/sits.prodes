library(raster)
for (i in 1:23) {
    r <- NULL
    r <- raster::raster("/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw/HLSL30-RAW_T19LFJ_2016-08-08_evi_STACK_BRICK.tif",
                        band = i)
    print(vapply(c(mean, sd, min, max), function(f, data){f(data, na.rm = TRUE)}, numeric(1), data = r[]))
    #table(r[])
    plot(r, main = i)
}
