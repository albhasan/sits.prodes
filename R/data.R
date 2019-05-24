#' @title Sample time-series from the PRODES system using a fusion model.
#'
#' @description A dataset containing a tibble with time series sampled on the
#' brazilian Amazon. The time series come from Landsat 8 Collection images. The
#' clouds in the images are filled in using the StarFM image fusion model.
#' StarFM builds a statistical model between MODIS and LANDSAT images at time
#' t1 which is later applied to MODIS images at time t0. The result is a
#' prediction of a Landsat 8 image at t0.
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_starfm
#' @usage data(prodes_samples_starfm)
#'
#' @format A tibble with 9269 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL

#' @title Sample time-series from the PRODES system using a fusion model.
#'
#' @description A dataset containing a tibble with time series sampled on the
#' brazilian Amazon. The time series come from Landsat 8 Collection images. The
#' clouds in the images are filled in using the StarFM image fusion model.
#' StarFM builds a statistical model between MODIS and LANDSAT images at time
#' t1 which is later applied to MODIS images at time t0. The result is a
#' prediction of a Landsat 8 image at t0. These time series are the best four
#' time steps of the samples in prodes_samples_starfm.
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_starfm_few_clouds
#' @usage data(prodes_samples_starfm_few_clouds)
#'
#' @format A tibble with 15978 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL

#' @title Sample time-series from the PRODES system using interpolation.
#'
#' @description A dataset containing a tibble with time series sampled on the
#' brazilian Amazon. The time series come from Landsat 8 Collection images. The
#' clouds in the images are filled in using billinear resampled MODIS images
#' (MOD13Q1 and MYD13Q1) of the closest in terms of place and date.
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_interpolated
#' @usage data(prodes_samples_interpolated)
#'
#' @format A tibble with 16305 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL

#' @title Sample time-series from the PRODES and LANSAT 8 images.
#'
#' @description A dataset containing a tibble with time series sampled on the
#' brazilian Amazon. The time series come from Landsat 8 Collection images.
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_simple
#' @usage data(prodes_samples_simple)
#'
#' @format A tibble with 1 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL


#' @title Sample time-series from the PRODES and cloud-masked LANSAT 8 images.
#'
#' @description A dataset containing a tibble with time series sampled on the
#' brazilian Amazon. The time series come from Landsat 8 Collection images in
#' which the clouds have been replaced with a no-data value (-9999).
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_mask_cloud
#' @usage data(prodes_samples_mask_cloud)
#'
#' @format A tibble with 1 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL


#' @title Sample time-series from the PRODES system using interpolation and
#' LANDSAT 8 images with fewest clouds.
#'
#' @description A dataset containing a tibble with time series sampled on the
#' brazilian Amazon. The time series come from Landsat 8 Collection images
#' with the fewest cluods on each PRODES year.
#' The clouds in the images are filled in using billinear resampled MODIS images
#' (MOD13Q1 and MYD13Q1) of the closest in terms of place and date.
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_interpolated_few_clouds
#' @usage data(prodes_samples_interpolated_few_clouds)
#'
#' @format A tibble with 16305 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL


#' @title Labels of Mapboimas Amazonia.
#'
#' @description Labels for the collection 3 of Mapboimas Amazonia.
#'
#' @docType data
#' @keywords datasets
#' @name mapbiomas_labels
#' @usage data(mapbiomas_labels)
#'
#' @format A tibble with 27 rows and 3 variables: Mapbiomas' labels and ids and
#'         their mapping to PRODES labels.
NULL


#' @title Labels of PRODES Amazonia.
#'
#' @description Labels for the 2017 release of PRODES shapefile.
#'
#' @docType data
#' @keywords datasets
#' @name prodes_labels
#' @usage data(prodes_labels)
#'
#' @format A tibble with 6 rows and 3 variables: PRODES' labels in Portuguesse,
#          English and identifiers for the latter.
NULL

#' @title Similatity between PRODES and MAPBIOMAS.
#'
#' @description Confusion matrices of PRODES (reference map) and MAPBIOMAS. The
#' comparison was made by mapping the MAPBIOMAS labels to PRODES (see data sets
#' prodes_labesl and mapbiomas_labels).
#'
#' @docType data
#' @keywords datasets
#' @name prodes_mapbiomas
#' @usage data(prodes_mapbiomas)
#'
#' @format A tibble with 15 rows and 3 variables: A Landsat Tile, a PRODES year,
#' and a list-colum with the confusion matrix provided by the caret package.
NULL

#' @title Validation samples collected by experts.
#'
#' @description Set of sample points for validating classification results. The
#' samples were selected randomically an classified by hand by an expert
#' (Rodrigo Begotti) using Landsat mosaics from PRODES Digital.
#'
#' @docType data
#' @keywords datasets
#' @name expert_validation
#' @usage data(expert_validation)
#'
#' @format An sf object with 450 rows and 9 colmns inclusing Identification,
#' Landsat tile, and the labels from 2014 to 2017.
NULL


#' @title Description of the Bricks used for the classifications.
#'
#' @description A tibble with the description of the Bricks used for the
#' classifications.
#'
#' @docType data
#' @keywords datasets
#' @name brick_description
#' @usage data(brick_description)
#'
#' @format A tibble of four rows and three columns with the description of the
#' Bricks used for the classifications.
NULL


#' @title Logs of the training of Deep Learning.
#'
#' @description Logs obtained during the training phase of Deep Learning
#' classification.
#'
#' @docType data
#' @keywords datasets
#' @name training_logs
#' @usage data(training_logs)
#'
#' @format A tibble of 14 rows and 4 columns with the experiment, the log's path,
#' the trainig's setup, and the training's partial results.
NULL
