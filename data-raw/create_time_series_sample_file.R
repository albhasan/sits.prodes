# create binary files of sample time series from PRODES

library(dplyr)
library(kohonen)
library(sits)
library(sits.prodes)
library(ensurer)

# stop("Run on server!")

# setup ----
#classification_type <- "interpolated"
#classification_type <- "starfm"
#classification_type <- "interpolated_few_clouds"
#classification_type <- "starfm_few_clouds"
#classification_type <- "simple"
classification_type <- "mask_cloud"

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

samples_path <- base_path %>% file.path("data", "samples")
samples_pattern <- c(interpolated            = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_interpolated.Rdata",
                     interpolated_few_clouds = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_interpolated_few_clouds.Rdata",
                     starfm                  = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_starfm.Rdata",
                     starfm_few_clouds       = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_starfm_few_clouds.Rdata",
                     simple                  = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_simple.Rdata",
                     mask_cloud              = "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_mask_cloud.Rdata"
)
expected_rows <- c(interpolated            = 23,
                   interpolated_few_clouds = 4,
                   starfm                  = 23,
                   starfm_few_clouds       = 4,
                   simple                  = 4,
                   mask_cloud              = 4)
stopifnot(names(samples_pattern) == names(expected_rows))
stopifnot(dir.exists(samples_path))

# where to store partial results
file_samples_koh          <- tempfile(pattern = paste0("file_samples_koh_",          classification_type, "_"), fileext = ".Rdata")
file_koh_evaluate_samples <- tempfile(pattern = paste0("file_koh_evaluate_samples_", classification_type, "_"), fileext = ".Rdata")

# load & filter samples
samples_tb <- samples_path %>%
    list.files(pattern = samples_pattern[classification_type], full.names = TRUE) %>%
    load_samples(sat = NULL, expected_nrow = expected_rows[classification_type]) %>%
    dplyr::bind_rows() %>%
    sits::sits_prune() %>%
    dplyr::filter(label %in% c("deforestation", "forest")) %>%
    ensurer::ensure_that(nrow(.) > 0) %>%
    dplyr::mutate(coverage = stringr::str_c("prodes_amazon_", classification_type))

# Create cluster with Self-organizing maps (kohonen)
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




# stop("Run on desktop!")






# load partial results
if (!exists("samples_koh") && !exists("koh_evaluate_samples")) {

    # 206
    # file_samples_koh <- "/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_samples_koh_interpolated_esensing-006.Rdata"
    # file_koh_evaluate_samples <- "/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_koh_evaluate_samples_interpolated_esensing-006.Rdata"


    # file_samples_koh <- "/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_samples_koh_simple_esensing-006.Rdata"
    # file_koh_evaluate_samples <- "/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/file_koh_evaluate_samples_simple_esensing-006.Rdata"


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

#stop("Run on server")

# remove confused samples
prodes_samples <- dplyr::left_join(samples_koh$info_samples, koh_evaluate_samples$metrics_by_samples, by = "id_sample") %>%
    dplyr::filter(label == neuron_label.x, percentage > 80) %>%
    dplyr::select(longitude:time_series)

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
    usethis::use_data(prodes_samples_interpolated, overwrite = TRUE)
}else if (classification_type == "interpolated_few_clouds") {
    prodes_samples_interpolated_few_clouds <- prodes_samples
    usethis::use_data(prodes_samples_interpolated_few_clouds, overwrite = TRUE)
}else if (classification_type == "starfm") {
    prodes_samples_starfm <- prodes_samples
    usethis::use_data(prodes_samples_starfm, overwrite = TRUE)
}else if (classification_type == "starfm_few_clouds") {
    prodes_samples_starfm_few_clouds <- prodes_samples
    usethis::use_data(prodes_samples_starfm_few_clouds, overwrite = TRUE)
}else if (classification_type == "simple") {
    prodes_samples_simple <- prodes_samples
    usethis::use_data(prodes_samples_simple, overwrite = TRUE)
}else if (classification_type == "mask_cloud") {
    prodes_samples_mask_cloud <- prodes_samples
    usethis::use_data(prodes_samples_mask_cloud, overwrite = TRUE)
}


