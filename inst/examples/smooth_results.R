#!/usr/bin/env Rscript

# Apply the bayesian smoother
suppressPackageStartupMessages(library(dplyr))
library(optparse)

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")

# Hanlde console input ----
opt_parser <- list(
  make_option("--in_dir", type = "character", default = NULL,
              help = "Path to a directory of SITS probability files",
              metavar = "character"),
  make_option("--win_size", type = "integer", default = 3,
              help = "Size of the smoothing window in pixels [default %default]",
              metavar = "number"),
  make_option("--noise", type = "integer", default = 10,
              help = "Noise to be added to the smoothing [default %default]",
              metavar = "number"),
  make_option("--overwrite", type = "logical", default = FALSE,
              help = "Overwrite old files? [default %default]",
              metavar = "logical")
) %>% OptionParser()
opt <- parse_args(opt_parser)
if (length(opt) != 5 || sum(sapply(opt, is.null)) != 0) {
  print_help(opt_parser)
  stop("Invalid arguments!")
}

in_dir     <- opt$in_dir
win_len    <- opt$win_size   # 3
noise      <- opt$noise      # 10
overwrite  <- opt$overwrite  # TRUE

# setup
out_dir <- file.path(in_dir, paste("smooth", paste(win_len, win_len, sep = 'x'),
                                   paste0('n', noise), sep = '_'))
if (!dir.exists(out_dir))
  dir.create(out_dir)
stopifnot(all(vapply(c(in_dir, out_dir), dir.exists, logical(1))))

# smooth definition
window  <- matrix(1, nrow = win_len, ncol = win_len, byrow = TRUE)

# get probability rasters
prob_files <- in_dir %>%
  list.files(pattern = ".*probs.*\\.tif$", full.names = TRUE) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = value) %>%
  dplyr::mutate(
    pathrow = stringr::str_extract(basename(file_path), "[0-9]{6}"),
    year = stringr::str_sub(stringr::str_extract(basename(file_path),
                                                 "_[0-9]{4}_")[1], 2, 5),
    file_out = file.path(out_dir, stringr::str_replace(basename(file_path),
                                                       "_probs_", '_'))
  )

# processing
for (pr in unique(prob_files$pathrow)) {
    for (y in unique(prob_files$year)) {
        print(sprintf("Processing probability maps of scene-year %s %s ...", pr, y))
        files_in  <- prob_files %>%
          dplyr::filter(pathrow == pr, year == y) %>%
          dplyr::pull(file_path)
        files_out <- prob_files %>%
          dplyr::filter(pathrow == pr, year == y) %>%
          dplyr::pull(file_out)

        cl <- parallel::makeCluster(min(40, length(files_in)))
        smoothed_list <- parallel::clusterApply(cl, seq_along(files_in),
            function(i, files_in, files_out, noise, window, overwrite) {
                raster_prob <- raster::brick(files_in[[i]])
                smooth_raster <- raster::raster(raster_prob[[1]])
                smooth_raster <- tryCatch({
                    raster::writeStart(smooth_raster,
                                       filename  = files_out[[i]],
                                       overwrite = overwrite)
                }, error = function(e) {
                    return(NULL)
                })
                if (is.null(smooth_raster))
                  return(NULL)
                block <- raster::blockSize(raster_prob[[1]])
                for (j in 1:block$n) {
                    band_values <- matrix(NA,
                                          nrow = block$nrows[j] * raster::ncol(raster_prob),
                                          ncol = raster::nlayers(raster_prob))
                    for (k in 1:raster::nlayers(raster_prob)) {
                        if (length(window) > 0) {
                            values <- matrix(raster::getValuesBlock(raster_prob[[k]],
                                                                    row = block$row[j],
                                                                    nrows = block$nrows[j]),
                                             nrow = block$nrows[j],
                                             byrow = TRUE)
                            values[values < 1] <- 1
                            values[values > 9999] <- 9999
                            values <- sits:::smooth_estimator_class(data = values,
                                                                    window = window,
                                                                    noise  = noise)
                        } else {
                          values <- matrix(raster::getValuesBlock(raster_prob[[k]],
                                                                  row = block$row[j],
                                                                  nrows = block$nrows[j]),
                                           nrow  = block$nrows[j],
                                           byrow = FALSE)
                        }
                        band_values[, k] <- values
                    }
                    smooth_raster <- raster::writeValues(smooth_raster,
                                                         apply(band_values, 1,
                                                               which.max),
                                                         block$row[j])
                }
                smooth_raster <- raster::writeStop(smooth_raster)
                return(smooth_raster)
            }, files_in, files_out, noise, window, overwrite)
        parallel::stopCluster(cl)
    }
}

print("Finished!")
