# Apply the bayesian smoother
suppressPackageStartupMessages(library(dplyr))
library(optparse)

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")

# get arguments ----
option_list = list(
  make_option("--experiment", type = "character", default = NULL,  help = "Name of an experiment e.g. 'rep_prodes_40'", metavar="character"),
  make_option("--win_size",   type = "integer",   default = 3,     help = "Size of the smoothing window in pixels [default %default]", metavar="number"),
  make_option("--noise",      type = "integer",   default = 10,    help = "Noise to be added to the smoothing [default %default]", metavar="number"),
  make_option("--overwrite",  type = "logical",   default = FALSE, help = "Overwrite old files? [default %default]", metavar="logical")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# validate arguments ----
if (length(opt) != 5 || sum(sapply(opt, is.null)) != 0){
  print_help(opt_parser)
  stop("Invalid arguments!")
}

# parse arguments ----
experiment <- opt$experiment # "rep_prodes_40"
win_len    <- opt$win_size   # 3
noise      <- opt$noise      # 10
overwrite  <- opt$overwrite  # TRUE

# setup
base_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction"
input_dir <- file.path(base_dir, "03_classify", experiment, "results")
output_dir <- file.path(input_dir, paste("smooth", paste(win_len, win_len, sep = 'x'), paste0('n', noise), sep = '_'))
if (!dir.exists(output_dir)) dir.create(output_dir)
stopifnot(all(vapply(c(base_dir, input_dir, output_dir), dir.exists, logical(1))))

# smooth definition
window  <- matrix(1, nrow = win_len, ncol = win_len, byrow = TRUE)

# get probability rasters
prob_files <- input_dir %>% list.files(pattern = ".*probs.*\\.tif$", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(
        pathrow = stringr::str_extract(basename(file_path), "[0-9]{6}"),
        year = stringr::str_sub(stringr::str_extract(basename(file_path), "_[0-9]{4}_")[1], 2, 5),
        file_out = file.path(output_dir, stringr::str_replace(basename(file_path), "_probs_", '_'))
    )

# processing 
for(pr in unique(prob_files$pathrow)){
    for(y in unique(prob_files$year)){
        print(sprintf("Processing probability maps of scene-year %s %s ...", pr, y))
        files_input  <- prob_files %>% dplyr::filter(pathrow == pr, year == y) %>% dplyr::pull(file_path)
        files_output <- prob_files %>% dplyr::filter(pathrow == pr, year == y) %>% dplyr::pull(file_out)

        cl <- parallel::makeCluster(min(40, length(files_input)))
        smoothed_list <- parallel::clusterApply(cl, seq_along(files_input), 
            function(i, files_input, files_output, noise, window, overwrite) {
                raster_prob <- raster::brick(files_input[[i]])
                smooth_raster <- raster::raster(raster_prob[[1]])
                smooth_raster <- tryCatch({
                    raster::writeStart(smooth_raster, filename  = files_output[[i]], overwrite = overwrite)
                }, error = function(e) {
                    return(NULL)
                })
                if (is.null(smooth_raster)) {
                    return(NULL)
                }
                block <- raster::blockSize(raster_prob[[1]])
                for (j in 1:block$n) {    
                    band_values <- matrix(NA, 
                                          nrow = block$nrows[j] * raster::ncol(raster_prob),
                                          ncol = raster::nlayers(raster_prob))
                    for (k in 1:raster::nlayers(raster_prob)) {
                        if (length(window) > 0) {
                            values <- matrix(raster::getValuesBlock(raster_prob[[k]], row = block$row[j], nrows = block$nrows[j]),
                                             nrow = block$nrows[j], 
                                             byrow = TRUE)
                            values[values < 1] <- 1
                            values[values > 9999] <- 9999
                            values <- sits:::smooth_estimator_class(data = values, window = window, noise  = noise)
                        } else {
                            values <- matrix(raster::getValuesBlock(raster_prob[[k]], row = block$row[j], nrows = block$nrows[j]),
                                             nrow  = block$nrows[j], 
                                             byrow = FALSE)
                        }
                        band_values[, k] <- values
                    }
                    smooth_raster <- raster::writeValues(smooth_raster, apply(band_values, 1, which.max), block$row[j])
                }  
                smooth_raster <- raster::writeStop(smooth_raster)
                return(smooth_raster)
            }, files_input, files_output, noise, window, overwrite)
        parallel::stopCluster(cl)
    }
}

print("Finished!")

