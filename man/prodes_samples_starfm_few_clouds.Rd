% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{prodes_samples_starfm_few_clouds}
\alias{prodes_samples_starfm_few_clouds}
\title{Sample time-series from the PRODES system using a fusion model.}
\format{A tibble with 15978 rows and 7 variables: (a) longitude: East-west coordinate of the time series sample (WGS 84);
  latitude (North-south coordinate of the time series sample in WGS 84), start_date (initial date of the time series),
  end_date (final date of the time series), label (the class label associated to the sample),
  coverage (the name of the coverage associated with the data),
  time_series (list containing a tibble with the values of the time series).}
\usage{
data(prodes_samples_starfm_few_clouds)
}
\description{
A dataset containing a tibble with time series sampled on the
brazilian Amazon. The time series come from Landsat 8 Collection images. The
clouds in the images are filled in using the StarFM image fusion model.
StarFM builds a statistical model between MODIS and LANDSAT images at time
t1 which is later applied to MODIS images at time t0. The result is a
prediction of a Landsat 8 image at t0. These time series are the best four
time steps of the samples in prodes_samples_starfm.
}
\keyword{datasets}
