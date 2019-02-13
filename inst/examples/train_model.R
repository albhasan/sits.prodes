#!/usr/bin/Rscript

# FIND THE BEST HYPERPARAMETERS FOR DEEP LEARNING MODEL

library(tidyverse)
library(sits)
library(keras)

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
devtools::load_all()

# util ----

# build a path to new taining
get_new_train_name <- function(train_path){
    train_path %>% list.dirs(recursive = FALSE) %>%
        stringr::str_match(pattern = "train_[0-9]{2}") %>% .[!is.na(.)] %>%
        stringr::str_extract(pattern = "[0-9]{2}") %>% dplyr::last() %>%
        as.integer() %>% (function(x) x + 1)
}

# setup ----

#brick_type <- "starfm"
#brick_type <- "interpolated"
#brick_type <- "simple"
brick_type <- "mask_cloud"
#experiment_bands <- c("ndvi", "nir", "red", "swir2") # train_40 train_50
#experiment_bands <- c("ndvi", "nir", "red", "swir2", "vegetation", "substrate", "dark") # train_41 train_51
experiment_bands <- c("vegetation", "substrate", "dark") # train_42 train_52

# experiment_labels <- c("forest", "deforestation", "flood") # train_40
experiment_labels <- c("forest", "deforestation") # train_41 train_42 train_50 train_51 train_52

experiment_scenes <- sort(c("225063", "226064", "233067"))
#experiment_scenes <- "225063"
#experiment_scenes <- "226064"
#experiment_scenes <- "233067"

train_path   <- "/home/alber/Documents/data/experiments/prodes_reproduction/02_train_model"
samples_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples"
scene_shp    <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/vector/wrs2_asc_desc/wrs2_asc_desc.shp"

prefix <- stringr::str_c("train_", get_new_train_name(train_path))
train_path <- file.path(train_path, prefix)

if (dir.exists(train_path)) {
    stop("Directory already exits!")
}else{
    dir.create(train_path)
}

log_file <- file(file.path(train_path, paste0(prefix, '_',
                                              R.utils::System$getHostname(), '_',
                                              ".R.log")), open = "wt")
sink(file = log_file, append = TRUE, type = 'message')
message(Sys.time(), ' Initializing...')
message(Sys.time(), ' SITS version ', paste(unlist(packageVersion("sits")), collapse = "."))
message(Sys.time(), ' Clasification type: ', brick_type)
message(Sys.time(), ' Bands  (experiment): ', paste0(experiment_bands, collapse = ", "))
message(Sys.time(), ' Labels (experiment): ', paste0(experiment_labels, collapse = ", "))
message(Sys.time(), ' Scenes (experiment): ', paste0(experiment_scenes, collapse = ", "))
message(Sys.time(), ' Brick type (experiment): ', brick_type)

# load samples
if (brick_type == "interpolated") {
    data(list = "prodes_samples_interpolated", package = "sits.prodes")
    prodes_samples <- prodes_samples_interpolated
}else if (brick_type == "starfm") {
    data(list = "prodes_samples_starfm", package = "sits.prodes")
    prodes_samples <- prodes_samples_starfm
}else if (brick_type == "simple") {
    data(list = "prodes_samples_simple", package = "sits.prodes")
    prodes_samples <- prodes_samples_simple
}else if (brick_type == "mask_cloud") {
    data(list = "prodes_samples_mask_cloud", package = "sits.prodes")
    prodes_samples <- prodes_samples_mask_cloud 
}else{
    stop("Unknown type of brick")
}
message(Sys.time(), ' Bands (samples): ', paste0(sits::sits_bands(prodes_samples), collapse = ", "))

# load scenes
scenes <- scene_shp %>%
    sf::read_sf(quiet = TRUE, stringsAsFactors = TRUE) %>%
    dplyr::filter(PR %in% experiment_scenes) %>%
    dplyr::select(PR) %>%
    sf::st_transform(crs = 4326)

# TODO: use the clustered labels if available
if (all(c("id_neuron", "neuron_label", "id_sample", "label2") %in% colnames(prodes_samples))) {
    prodes_samples <- prodes_samples %>% dplyr::mutate(label = label2) %>%
        dplyr::select(-c(id_neuron, neuron_label, id_sample, label2))
}

# restrain samples to certain bands, labels, and scenes
prodes_samples <- prodes_samples %>%
    sits::sits_select_bands_(bands = experiment_bands) %>%
    dplyr::filter(label %in% experiment_labels) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326,
                 remove = FALSE) %>%
    sf::st_intersection(scenes) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(longitude, latitude, start_date, end_date, label, coverage,
                  time_series)

# build a random hyper-parameter list
param_ls <- list()
n <- 1:20
for (i in seq_along(n)) {
    n_layers <- sample(2:6, size = 1)
    param_ls[[i]] <- list(
        units            = rep(sample(seq(600, 1000, 100), size = 1), n_layers),
        activation       = sample(c('selu', 'sigmoid'), size = 1),
        dropout_rates    = rep(sample(seq(0.3, 0.5, 0.1), size = 1), n_layers),
        optimizer        = keras::optimizer_adam(),
        epochs           = sample(seq(200, 300, 100), size = 1),
        batch_size       = sample(c(64, 256, 320), size = 1),
        validation_split = 0.2,
        model_name       = paste0(prefix, "_model_", i)
    )
}

# train using the hyper-parameters list
counter <- 0
for (p in param_ls) {
    (counter <- counter + 1)
    #options(keras.fit_verbose = 0)
    method <- sits_deeplearning(
        units            = p$units,
        activation       = p$activation,
        dropout_rates    = p$dropout_rates,
        optimizer        = p$optimizer,
        epochs           = p$epochs,
        batch_size       = p$batch_size,
        validation_split = p$validation_split)

    model <- sits::sits_train(prodes_samples, method)

    sits::sits_save_keras(model,
                          hdffile = file.path(train_path, paste0(p$model_name, '.h5')),
                          rdsfile = file.path(train_path, paste0(p$model_name, '.rds')))

    met <- environment(model)$history$metrics
    message('EXPERIMENT')
    message(names(p))
    message('optimizer = ', attributes(p$optimizer)$class[1])
    message(paste0(names(p), " = ", p, '\n'))
    msgs <- paste0(names(met), ' = ', met)
    for (m in msgs) {
        message(m)
    }
    message('---')
    rm(method)
    rm(model)
    rm(met)
}
message('Finished!')
message(Sys.time())
message(' ')
