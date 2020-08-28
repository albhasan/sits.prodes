#' @title Swap the logical values in a raster mask.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description  Any value greater than one becomes zero and each zero becomes
#'               one.
#'
#' @param in_file  A length-one character. A path to a raster mask.
#' @param out_file A lenght-one character. A path to the file with the results.
#' @return         out_file.
#' @export
invert_mask <- function(in_file,
                        out_file = tempfile(pattern = "invert_mask_",
                                            fileext = ".tif")){
    if (is.na(in_file))
        return(NA)
    in_file %>%
        ensurer::ensure_that(length(.) == 1) %>%
        gdalcmdline::gdal_calc(out_filename = out_file,
                               expression = "numpy.select([A == 0, A > 0], [1, 0])",
                               creation_option = c("COMPRESS=LZW")) %>%
        return()
}


#' @title Tranform a raster into a mask.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Any value greater than one becomes one.
#'
#' @param in_file  A length-one character. A path to a raster mask.
#' @param out_file A lenght-one character. A path to the file with the results.
#' @return         out_file.
#' @export
raster2mask <- function(in_file,
                        out_file = tempfile(pattern = "raster2mask_",
                                            fileext = ".tif")){
    if (is.na(in_file))
        return(NA)
    in_file %>%
        ensurer::ensure_that(length(.) == 1) %>%
        gdalcmdline::gdal_calc(out_filename = out_file,
                                   expression = "numpy.select([A > 0], [1])",
                                   creation_option = c("COMPRESS=LZW")) %>%
        return()
}


#' @title Apply a mask to a raster.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Any value greater than one becomes one.
#'
#' @param in_file   A length-one character. A path to a raster
#' @param mask_file A lenght-one character. A path to a raster mask.
#' @param out_file  A lenght-one character. A path to the file with the results.
#' @return         out_file.
#' @export
mask_with <- function(in_file, mask_file,
                      out_file = tempfile(pattern = "mask_with_",
                                           fileext = ".tif")){

    vrt_file <- gdalUtilities::gdalbuildvrt(gdalfile = c(in_file, mask_file),
                                           output.vrt = tempfile(pattern = "mask_with_",
                                                                 fileext = ".vrt"),
                                           separate = TRUE, vrtnodata = 0)

    gdalcmdline::gdal_calc(input_files = rep(vrt_file, times = 2),
                           out_filename = out_file,
                           band_number = 1:2,
                           expression = "A * B",
                           dstnodata = 0,
                           creation_option = c("COMPRESS=LZW")) %>%
        return()

}


