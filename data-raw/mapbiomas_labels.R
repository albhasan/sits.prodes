#!/usr/bin/Rscript

print("Setting the mapping between PRODES and MAPBIOMAS...")

suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(devtools))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes"
setwd(base_path)

mapbiomas_labels <- tibble::tribble(
        ~label_mb,                                ~id_mb,   ~label_pd,
        #----------------------------------------#--------#----------------
        "1 Forest",                               1L,       "forest",
        "1.1 Natural Forest",                     2L,       "forest",
        "1.1.1 Forest Formation",                 3L,       "forest",
        "1.1.2 Savanna Formation",                4L,       "no forest",
        "1.1.3 Mangrove",                         5L,       "forest",
        "1.2 Forest Plantation",                  9L,       "forest",
        "2 Non Forest Natural Formation",         10L,      "no forest",
        "2.1 Wetland",                            11L,      "no forest",
        "2.2 Grass Formation",                    12L,      "no forest",
        "2.3 Other non forest natural formation", 13L,      "no forest",
        "3 Farming",                              14L,      "deforestation",
        "3.1 Pasture",                            15L,      "deforestation",
        "3.2 Agriculture",                        18L,      "deforestation",
        "3.2.1 Annual and Perennial Crop",        19L,      "deforestation",
        "3.2.2 Semi-Perennial Crop",              20L,      "deforestation",
        "3.3 Mosaic of Agriculture and Pasture",  21L,      "deforestation",
        "4 Non vegerated area",                   22L,      "no forest",
        "4.1 Beach and Dune",                     23L,      "no forest",
        "4.2 Urban Infrastucture",                24L,      "no forest",
        "4.5 Other non vegetated area",           25L,      "no forest",
        "5 Water",                                26L,      "water",
        "6 Non Observed",                         27L,      "no forest",
        "4.3 Rocky outcrop",                      29L,      "no forest",
        "4.4 Mining",                             30L,      "deforestation",
        "5.2 Aquaculture",                        31L,      "water",
        "2.3 Salt Flat",                          32L,      "no forest",
        "5.1 River, Lake and Ocean",              33L,      "water"
    )

usethis::use_data(mapbiomas_labels, overwrite = TRUE)

