
# samples to SHP

library(tidyverse)
library(sits)
library(sf)

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
devtools::load_all()


# setup ----
classification_type <- "starfm"
classification_type <- "interpolated"
# ---


shp_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/vector/wrs2_asc_desc/wrs2_asc_desc.shp"

tiles <- shp_path %>% sf::read_sf()

# load samples
if (classification_type == "interpolated") {
    data(list = "prodes_samples_interpolated", package = "sits.prodes")
    prodes_samples <- prodes_samples_interpolated
}else if (classification_type == "starfm") {
    data(list = "prodes_samples_starfm", package = "sits.prodes")
    prodes_samples <- prodes_samples_starfm
}





prodes_samples %>% dplyr::pull(label) %>% unique()
prodes_samples %>% nrow()




prodes_samples
