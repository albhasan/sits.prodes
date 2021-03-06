% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{prodes2raster_helper}
\alias{prodes2raster_helper}
\title{Helper for rasterizing a PRODES shapefile.
PRODES's SHP into a TIF.}
\usage{
prodes2raster_helper(file_path_prodes, file_path_res, pyear, prodes_label_key)
}
\arguments{
\item{file_path_prodes}{A path to a PRODES shapefile.}

\item{file_path_res}{A path to the results of a classification.}

\item{pyear}{A PRODES year.}

\item{prodes_label_key}{A tibble with the mapping of the PRODES labels from
Portuguese to English and an ID.}
}
\value{
A path to a raster file.
}
\description{
Rasterize the given PRODES shapfile to match the given
             reference raster. The resulting raster is encoded according to
             the given PRODES label key and the label deforesatation
             includes all the PRODES years until the given PRODES year.
}
\author{
Alber Sanchez, \email{alber.ipia@inpe.br}
}
