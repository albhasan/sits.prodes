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
data("prodes_labels", package = "sits.prodes")
data("mapbiomas_labels", package = "sits.prodes")
stopifnot(exists(c("prodes_labels", "mapbiomas_labels")))

prodes_lbl <- prodes_labels %>% dplyr::full_join(mapbiomas_labels, by = "label_pd") %>%
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
                                   prodes_files$file_rt,
                                   confusion_raster,
                                   key_ls = key_pd)

# save
prodes_mapbiomas <- prodes_files %>% dplyr::select(tile, year_pd, conmat)
devtools::use_data(prodes_mapbiomas, overwrite = TRUE)

