# TEST GDALCUBES
# On-Demand Processing of Data Cubes from Satellite Image Collections with the 
# gdalcubes Library by Apgel & Pebesma

# install.packages("gdalcubes")
library(gdalcubes)
library(magrittr)
gdalcubes_set_threads(8)

#---------------------------

# create an image collection from files on disk 
files <- list.files(path = "/home/alber/landsat8/LC08_232_067", 
                    pattern = "LC08_L1TP_232067_20160[7-8][0-9]{2}_[0-9]{08}_[0-9]{2}_[A-Z][0-9]_sr_band[0-9].tif",
                    full.names = TRUE) 
L8SR.col <- gdalcubes::create_image_collection(files, format = "L8_SR")

# create a data cube view for a coarse resolution overview 
v <- gdalcubes::cube_view(srs = "EPSG:3857", extent = L8SR.col, dx = 30, 
                          dt = "P16D", aggregation = "median", 
                          resampling = "bilinear")

# create a true color overview image 
gdalcubes::raster_cube(L8SR.col, v) %>% 
    gdalcubes::select_bands(c("B02", "B03", "B04")) %>% 
    reduce_time("median(B02)", "median(B03)", "median(B04)") %>% 
    plot(rgb = 3:1, zlim = c(0, 1200))

#---------------------------

file_subdatasets <- expand.grid(file = list.files(path = "/net/150.163.2.38/dados1/modisOriginal/MOD13Q1/2014", 
                                                  pattern = "MOD13Q1.A20142[0-9]{2}.h13v14.006.[0-9]{13}.hdf",
                                                  full.names = TRUE), 
                                subdataset = c("NDVI", "EVI", "VI Quality", 
                                               "red reflectance", 
                                               "NIR reflectance", 
                                               "blue reflectance", 
                                               "MIR reflectance", 
                                               "view zenith angle", 
                                               "sun zenith angle", 
                                               "relative azimuth angle", 
                                               "composite day of the year", 
                                               "pixel reliability"), 
                                stringsAsFactors = FALSE)
gdal_datasets <- paste("HDF4_EOS:EOS_GRID:\"", file_subdatasets$file, 
                       "\":MODIS_Grid_16DAY_250m_500m_VI:250m 16 days ", 
                       file_subdatasets$subdataset, sep  = "")

MOD13Q1.col <- create_image_collection(files = gdal_datasets, format = "MxD13Q1")

v <- gdalcubes::cube_view(srs = "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ", 
                          extent = MOD13Q1.col, dx = 231.656358263889, 
                          dt = "P16D", aggregation = "median", 
                          resampling = "bilinear")


gdalcubes::raster_cube(MOD13Q1.col, v) %>% 
    gdalcubes::select_bands(c("NIR", "red", "blue")) %>% 
    reduce_time("median(NIR)", "median(red)", "median(blue)") %>% 
    plot(rgb = 3:1, zlim = c(0, 1200))



