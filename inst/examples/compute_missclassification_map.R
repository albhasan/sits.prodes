
# combine the missclassification maps of each algorithm for the same year
library(dplyr)
library(raster)

in_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify"
alg_dirs <- c("results_dl", "results_rf", "results_svm", "results_vote")
smooth_dir <- c(NA, "smooth_11x11_n10", "smooth_3x3_n10", "smooth_5x5_n10",
                "smooth_7x7_n10", "smooth_9x9_n10")

# Get a tibble of raster files with PRODES-Classification differences.
diff_files <- in_dir %>%
    list.files(pattern = "._masked_refdiff.tif$", full.names = "TRUE", recursive =  TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = basename(file_path)) %>%
    dplyr::mutate(
        scene = file_name %>%
            stringr::str_extract(pattern = "_[0-9]{6}_") %>%
            stringr::str_sub(2, -2),
        pyear = file_name %>%
            stringr::str_extract_all(pattern = "_[0-9]{4}_") %>%
            lapply(., dplyr::last) %>%
            stringr::str_sub(2, -2),
        algorithm = file_path %>%
            stringr::str_extract(pattern = "results_[a-z]+") %>%
            stringr::str_sub(9),
        smooth = file_path %>% dirname() %>%
            stringr::str_extract(pattern = "smooth_[0-9]+x[0-9]+_n[0-9]+") %>%
            ifelse(is.na(.), "no_smooth", .),
        experiment = file_path %>% dirname() %>%
            stringr::str_extract(pattern = "rep_prodes_[0-9]+")
    ) %>%
    dplyr::filter(algorithm != "vote")

# Intersect rasters of the same experiment, smooth, year, and scene.
experiment <- pyear <- scene <- smooth <- NULL
tests <- diff_files %>%
    ensurer::ensure_that(all(c("experiment", "smooth", "pyear", "scene") %in%
                                 colnames(.))) %>%
    dplyr::select(experiment, smooth, pyear, scene) %>%
    dplyr::rename(t_experiment = experiment, t_smooth = smooth, t_pyear = pyear,
                  t_scene = scene) %>%
    lapply(., unique) %>%
    expand.grid(stringsAsFactors = FALSE) %>%
    tibble::as_tibble()

# Helper function for computing the common areas of confusion maps.
compute_confusion <- function(t_experiment, t_smooth, t_pyear, t_scene, data_tb){

    algorithms <- data_tb %>%
        dplyr::pull(algorithm) %>%
        unique() %>%
        ensurer::ensure_that(length(.) > 1, err_desc = "Not enough rasters.")

    # Build a tibble of rasters.
    sub_dt <- data_tb %>% dplyr::filter(experiment == t_experiment,
                                        smooth ==  t_smooth,
                                        pyear == t_pyear,
                                        scene == t_scene) %>%
        ensurer::ensure_that(nrow(.) == length(algorithms),
                             err_desc = "Missmatch between the number of images
                                         and classification algorithms.") %>%
        dplyr::mutate(dif_raster = lapply(.$file_path, raster::raster)) %>%
        ensurer::ensure_that(raster::compareRaster(.$dif_raster,
                                                   stopiffalse = FALSE),
                             err_desc = "Spatial missmatch between rasters.") %>%
        dplyr::mutate(
            is_prodes = purrr::map(.$dif_raster, function(x){return(x == 0)})
        )

    res <-  Reduce(`&`, sub_dt$is_prodes, sub_dt$is_prodes[[1]], accumulate = FALSE)
    return(res)
}

common_confusion <- NULL
tests <- tests %>%
    dplyr::mutate(common_confusion = purrr::pmap(., compute_confusion,
                                                 data_tb = diff_files),
                  filename = file.path(in_dir, "confusion_dl_rf_svm",
                                       paste0(paste(t_experiment, t_scene,
                                                    t_pyear, t_smooth,
                                                    sep = "_"), ".tif")))

tests %>%
    dplyr::select(common_confusion, filename) %>%
    purrr::pmap(., function(common_confusion, filename){
        raster::writeRaster(common_confusion, filename = filename)
    })

print("Done!")
