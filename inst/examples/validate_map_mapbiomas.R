# validate the result of a classification using map biomas amazonia
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(sits.prodes))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(magrittr))

# TODO: remove
setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
library(devtools)
devtools::load_all()
# - - - 

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
mapbiomas_path <- file.path(base_path, c("data/raster/mapbiomas/amazonia/mapbiomas_tiled"))
mapbiomas_style <- file.path(base_path, "qgis/styles/mapbiomas_amazonia_c3.qml")
prodes_path <- file.path(base_path, "data/vector/prodes/prodes_tiled")

stopifnot(all(dir.exists(c(base_path, mapbiomas_path, prodes_path))))
stopifnot(all(file.exists(mapbiomas_style)))

# insert missing 0s in PRODES' landsat scene
prodes2scene <- function(prodes_scene){gsub('^([0-9]{3})([0-9]+)$', '\\10\\2', prodes_scene)}

# test if a character is made of numbers or dot 
castable2numeric <- function(chr){is.numeric(as.numeric(chr)) & !is.na(chr)}

# get labels
prodes_lbl <- tibble::tribble(
             ~label_pd_pt,     ~label_pd,  ~id_pd,
        #--------------#-----------------#--------
        "DESMATAMENTO",  "deforestation", 1L,
        "RESIDUO",       "deforestation", 1L,
        "FLORESTA",      "forest",        2L,
        "NAO_FLORESTA",  "no forest",     3L,
        "NAO_FLORESTA2", "no forest",     3L,
        "HIDROGRAFIA",   "water",         4L
    ) %>% dplyr::full_join(tibble::tribble(
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
    ), by = "label_pd") %>%
    ensurer::ensure_that(!all(sapply(., is.na)),
                         all.equal(as.vector(table(.$label_pd)), as.vector(table(.$id_pd))))

# re-classification matrix
rcl_mat <- prodes_lbl %>% dplyr::select(id_mb, id_pd) %>% as.matrix()

# get files, reclassify mapbiomas 
mapbiomas_files <- mapbiomas_path %>%
    list.files(full.names = TRUE, pattern = "tif$") %>%
    tibble::enframe(name = NULL, value = "filepath_mb") %>%
    dplyr::mutate(
        tile = filepath_mb %>% basename() %>%
            stringr::str_extract(pattern = "_[0-9]{5}\\.") %>%
            stringr::str_sub(2, -2) %>%
            prodes2scene(),
        year_mb = filepath_mb %>% basename() %>%
            stringr::str_extract(pattern = "_[0-9]{4}_") %>%
            stringr::str_sub(2, -2)
    ) %>% ensurer::ensure_that(all(nchar(.$tile) == 6), all(nchar(.$year_mb) == 4), 
            all(castable2numeric(.$tile)), all(castable2numeric(.$year_mb))) %>%
    dplyr::mutate(mb_reclass = purrr::map_chr(.$filepath_mb, function(file_path){
        reclas_file <- file_path %>% basename() %>%
            tools::file_path_sans_ext() %>%
            stringr::str_c("_prodes.tif")
        out_file <- file.path(tempdir(), reclas_file)
        file_path %>% raster::raster() %>%
            raster::reclassify(rcl = rcl_mat, filename = out_file, overwrite = TRUE)
        return(out_file)
    }))

# get files, rasterize prodes
prodes_files <- prodes_path %>%
    list.files(full.names = TRUE, pattern = "shp$") %>%
    tibble::enframe(name = NULL, value = "filepath_pd") %>%    
    dplyr::mutate(
        tile = filepath_pd %>% basename() %>%
            stringr::str_extract(pattern = "_[0-9]{3}_[0-9]{3}\\.") %>%
            stringr::str_sub(2, -2) %>%
            stringr::str_replace(pattern = '_', '')
    ) %>% ensurer::ensure_that(all(nchar(.$tile) == 6),
                                all(castable2numeric(.$tile))) %>%
    dplyr::right_join(mapbiomas_files, by = "tile") %>%
    dplyr::select(file_pd = filepath_pd, file_mb = mb_reclass, tile, year_mb) %>%
    dplyr::mutate(rasterized = purrr::pmap_chr(., prodes2mapbiomas, prodes_lbl = prodes_lbl))

# compare reclass mapbiomas to prodes

# recode mapbiomas to match PRODES

