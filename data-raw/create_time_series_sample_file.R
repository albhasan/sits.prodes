# create binary files of sample time series from PRODES. The time series are
# taken from interpolated and StarFM bricks

library(tidyverse)
library(kohonen)
library(sits)
library(ensurer)



stop("Run on server!")



setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
devtools::load_all()


# setup ----
classification_type <- "interpolated"
classification_type <- "starfm"

samples_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples"
samples_pattern <- c(interpolated = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_starfm.Rdata",
                     starfm       = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_interpolated.Rdata")

# where to store partial results
fp_suffix <- paste0(classification_type, "_", R.utils::System$getHostname(), ".Rdata")
file_samples_koh          <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/tempdir", paste0("file_samples_koh_", fp_suffix))
file_koh_evaluate_samples <- file.path("/home/alber/Documents/data/experiments/prodes_reproduction/tempdir", paste0("file_koh_evaluate_samples_", fp_suffix))
set.seed(666)
# - - - -


# load & filter samples
test_labels <- c("deforestation", "forest")
samples_tb <- samples_path %>%
    list.files(pattern = samples_pattern[classification_type],
               full.names = TRUE) %>% load_samples(sat = NULL) %>%
    dplyr::bind_rows() %>% sits::sits_prune() %>%
    dplyr::filter(label %in% test_labels) %>% ensurer::ensure_that(nrow(.) > 0) %>%
    dplyr::mutate(coverage = stringr::str_c("prodes_amazon_", classification_type))
# samples_tb <- samples_tb %>% sits::sits_linear_interp(n = 23) %>% # fill in the NAs
# samples_tb <- samples_tb %>% sits::sits_sample(n = 1000/length(test_labels))

#Create cluster with Self-organizing maps (kohonen)
xd <- 25
yd <- 25
rl = 100
stopifnot(xd * yd < nrow(samples_tb))
time_series.ts <- samples_tb %>% sits::sits_values(format = "bands_cases_dates")
samples_koh <- sits::sits_kohonen(data.tb = samples_tb,
                                  time_series = time_series.ts,
                                  grid_xdim = xd,
                                  grid_ydim = yd,
                                  rlen = rl,
                                  dist.fcts = "euclidean",
                                  alpha = 1,
                                  neighbourhood.fct = "gaussian")

koh_evaluate_samples <- sits::sits_evaluate_samples(data.tb = samples_tb,
                                                    time_series = time_series.ts,
                                                    grid_xdim = xd,
                                                    grid_ydim = yd,
                                                    rlen = rl,
                                                    distance = "euclidean",
                                                    mode = "pbatch",
                                                    iteration = 100)

# save partial results
save(samples_koh, file = file_samples_koh)
save(koh_evaluate_samples, file = file_koh_evaluate_samples)



stop("Run on desktop!")



# load partial results
if (!exists("samples_koh") && !exists("koh_evaluate_samples")) {
    load(file_samples_koh)
    load(file_koh_evaluate_samples)

    # load("/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_samples_koh_interpolated_esensing-007.Rdata")
    # samples_koh$info_samples <- samples_koh$info_samples %>% dplyr::mutate(coverage = "prodes_amazon_interpolated")
    # load("/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_koh_evaluate_samples_interpolated_esensing-007.Rdata")
    # koh_evaluate_samples$samples.tb <- koh_evaluate_samples$samples.tb %>% dplyr::mutate(coverage = "prodes_amazon_interpolated")
    #
    # load("/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_samples_koh_starfm_esensing-007.Rdata")
    # samples_koh$info_samples <- samples_koh$info_samples %>% dplyr::mutate(coverage = "prodes_amazon_starfm")
    # load("/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_koh_evaluate_samples_starfm_esensing-007.Rdata")
    # koh_evaluate_samples$samples.tb <- koh_evaluate_samples$samples.tb %>% dplyr::mutate(coverage = "prodes_amazon_starfm")
    #
    #rm(samples_koh)
    #rm(koh_evaluate_samples)
}

# check the distribution of the smaples
# sits::sits_plot_kohonen(samples_koh)



# remove confused samples
prodes_samples <- dplyr::left_join(samples_koh$info_samples, koh_evaluate_samples$metrics_by_samples, by = "id_sample") %>%
    dplyr::filter(label == neuron_label.x, percentage > 80) %>%
    dplyr::select(longitude:time_series)


stop("Run on server")



# futher validation of samples through k-folds (see confusion matrix)
prodes_samples %>% sits::sits_kfold_validate(ml_method = sits::sits_svm()) %>%
    sits::sits_conf_matrix()



# TODO: Review lost samples in QGIS. It could have appeared unsampled spots


# # Even more analysis of samples
# #
# #Analyze the mixture between groups and extract informations about confusion matrix
# confusion_by_cluster <- sits::sits_metrics_by_cluster(samples_koh$info_samples)
# confusion_matrix <- confusion_by_cluster$confusion_matrix
# sits::sits_plot_cluster_info(confusion_by_cluster, "Confusion by Cluster")
# confusion_matrix
# #
# #Divide groups according to variations
# subgroups <- sits::sits_subgroup(samples_koh)
# #
# #Get samples tibble with subgroups
# samples_subgroup <- subgroups$samples_subgroup.tb
# #
# #Get neurons and their patterns
# neurons_subgroup <- subgroups$neurons_subgroup.lst
# #
# #Number of subgroups for each class
# number_of_subgroup <- lengths(neurons_subgroup)
# #
# #Plot subgroups
# sits_plot_subgroups(neurons_subgroup)


# save the samples to the package
if (classification_type == "interpolated") {
    prodes_samples_interpolated <- prodes_samples
    devtools::use_data(prodes_samples_interpolated, overwrite = TRUE)
}else if (classification_type == "starfm") {
    prodes_samples_starfm <- prodes_samples
    devtools::use_data(prodes_samples_starfm, overwrite = TRUE)
}

