#
# Compute the number of clouds per pixel per brick.

require(dplyr)
require(purrr)
require(rgdal)
require(stringr)
require(tibble)

gdal_sum <- function(infile, outfile, return_raster=FALSE, overwrite=FALSE) {
  # Be aware that the outfile type will be the same as the infile type
  if(return_raster) require(raster)
  # infile:        The multiband raster file (or a vector of paths to multiple
  #                 raster files) for which to calculate cell sum.
  # outfile:       Path to raster output file.
  # return_raster: (logical) Should the output raster be read back into R?
  # overwrite:     (logical) Should outfile be overwritten if it exists?
  gdal_calc <- Sys.which('gdal_calc.py')
  if(gdal_calc=='') stop('gdal_calc.py not found on system.')
  if(file.exists(outfile) & !overwrite)
    stop("'outfile' already exists. Use overwrite=TRUE to overwrite.",
         call.=FALSE)
  nbands <- sapply(infile, function(x) nrow(attr(GDALinfo(x), 'df')))
  if(length(infile) > 26 || nbands > 26) stop('Maximum number of inputs is 26.')
  if(length(nbands) > 1 & any(nbands > 1))
    warning('One or more rasters have multiple bands. First band used.')

  if(length(infile)==1) {
    inputs <- paste0('-', LETTERS[seq_len(nbands)], ' ', shQuote(infile), ' --',
                     LETTERS[seq_len(nbands)], '_band ', seq_len(nbands), collapse=' ')
    n <- nbands
  } else {
    inputs <- paste0('-', LETTERS[seq_along(nbands)], ' ', shQuote(infile), ' --',
                     LETTERS[seq_along(nbands)], '_band 1', collapse=' ')
    n <- length(infile)
  }

  message('Calculating sum and writing to ', basename(outfile))
  system2('python3',
          args=c(gdal_calc, inputs,
                 sprintf("--outfile=%s", outfile),
                 sprintf('--calc="sum([%s], axis=0)"',
                         paste0(LETTERS[seq_len(n)], collapse=',')),
                 '--co="COMPRESS=LZW"',
                 type='Float32',
                 if(overwrite) '--overwrite'),
          stdout=FALSE
  )
  if(return_raster) raster(outfile) else invisible(NULL)
}

in_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw"
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/cloud_count_hls"

cloud_bricks <- in_dir %>%
    list.files(pattern = "_cloud_", full.names = "TRUE") %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(infile = "value") %>%
    dplyr::mutate(outfile = file.path(out_dir, basename(infile))) %>%
    dplyr::mutate(cloud_count = purrr::map2(infile, outfile, gdal_sum))

