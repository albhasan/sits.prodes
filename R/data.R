#' @title Sample time-series from the PRODES system.
#'
#' @description A dataset containing a tibble with time series sampled for the
#' brazilian Amazon. The time series come from two scenes of Landsat 8
#' Collection images. The clouds in the images were filled in using the StarFM
#' image fusion model. This model builds a statistical model between a MODIS
#' and a LANDSAT image at time t1 which is lated applied to a MODIS image at
#' time t0. The result is a prediction of the Landsat 8 image at t0. 
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_interpolated
#' @usage data(prodes_samples_starfm)
#'
#' @format A tibble with 1 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).


#' @title Sample time-series from the PRODES system.
#'
#' @description A dataset containing a tibble with time series sampled for the
#' brazilian Amazon. The time series come from two scenes of Landsat 8
#' Collection images. The clouds in the images were filled in using billinear
#' resampled MODIS images (MOD13Q1 and MYD13Q1) of approximately the closest 
#' place and date.
#'
#' @docType data
#' @keywords datasets
#' @name prodes_samples_starfm
#' @usage data(prodes_samples_starfm)
#'
#' @format A tibble with 1 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
#'   latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
#'   end_date (final date of the time series), label (the class label associated to the sample),
#'   coverage (the name of the coverage associated with the data),
#'   time_series (list containing a tibble with the values of the time series).
NULL

