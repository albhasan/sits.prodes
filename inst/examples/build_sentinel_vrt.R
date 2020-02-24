#!/usr/bin/env Rscript

# BUILD GDAL VIRTUAL RASTERS OUT OF SENTINEL2 IMAGES.

# TODO: remove
library(dplyr)
library(sits.starfm)
library(devtools)
devtools::load_all()

#img_dir <- "/home/alber/Documents/temp/clouds/data/fmask4_s2cloudless" %>%
img_dir <- "/home/alber/Documents/temp/clouds/data/maja" %>%
    sits.starfm::build_sentinel_tibble() %>%
    #dplyr::filter(img_date == "2016-08-05") %>%
	dplyr::mutate(composite = purrr::map(files, compose_bands_s2,
                                         out_dir = "/home/alber/Documents/temp/vrt"))




