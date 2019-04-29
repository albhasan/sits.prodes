#!/usr/bin/Rscript

# COMPUTE THE CONFUSION MATRICES OF MAPBIOMAS USING PRODES AS A REFERENCE

suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(sits.prodes))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(magrittr))


library(devtools)
devtools::load_all()


base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
mapbiomas_path <- file.path(base_path, c("data/raster/mapbiomas/amazonia/mapbiomas_tiled"))
mapbiomas_style <- file.path(base_path, "qgis/styles/mapbiomas_amazonia_c3.qml")
prodes_path <- file.path(base_path, "data/vector/prodes/prodes_tiled")

stopifnot(all(dir.exists(c(base_path, mapbiomas_path, prodes_path))))
stopifnot(all(file.exists(mapbiomas_style)))

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

# reclassify mapbiomas 
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
        out_file <- file.path(base_path, "data",
            "raster", "mapbiomas", "amazonia", "reclas2prodes",
            reclas_file)
        file_path %>% raster::raster() %>%
            raster::reclassify(rcl = rcl_mat, filename = out_file,
                               overwrite = TRUE)
        return(out_file)
    }))

# rasterize prodes
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
    dplyr::select(file_pd = filepath_pd, file_rt = mb_reclass, tile, year_pd = year_mb) %>%
    dplyr::mutate(rasterized = purrr::pmap_chr(., prodes2raster, prodes_lbl = prodes_lbl)) %>%
    # save
    dplyr::mutate(match_pd_mb = file.path(base_path, "data",
        "raster", "prodes", "resampled2mapbiomas",
        paste0("prodes_mb_", tile, '_', year_pd, ".tif"))) %>%
    dplyr::mutate(res = purrr::pmap_lgl(dplyr::select(., rasterized, match_pd_mb),
        function(rasterized, match_pd_mb){file.copy(rasterized, match_pd_mb, overwrite = TRUE)}))

# compute confusion matrices prodes vs mapbiomas
key_tb <- prodes_lbl %>% dplyr::select(label_pd, id_pd) %>%
    dplyr::group_by(label_pd) %>%
    dplyr::slice(1)
key_pd        <- key_tb %>% dplyr::pull(label_pd) %>% as.list()
names(key_pd) <- key_tb %>% dplyr::pull(id_pd)
prodes_files$conmat <- purrr::map2(prodes_files$match_pd_mb,
                                   prodes_files$match_pd_mb, confusion_raster,
                                   key_ls = key_pd)

# save
prodes_mapbiomas <- prodes_files %>% dplyr::select(tile, year_pd, conmat)
devtools::use_data(prodes_mapbiomas)

