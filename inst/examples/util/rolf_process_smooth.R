# Original script for bayesian smoothing authored by Rolf Simoes. 

# probabilities rasters
#files_input <- sprintf("/vsicurl/%s", s3_list("mod13q1-br-mt-class", ".*probs.*\\.tif$"))
files_input <- list.files("/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results", ".*probs_2016.*\\.tif$", full.names = TRUE) 
# files_input <- list.files("/result/mod13q1_br_cerrado_selectedCerrado/", ".*probs.*\\.tif$", full.names = T)
local_output_folder <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results/smooth"
#local_output_folder <- sprintf("~/result")

# smooth definition
win_len = 3
noise = 10
window = matrix(1, nrow = win_len, ncol = win_len, byrow = TRUE)
if (win_len == 0) { 
  output_name <- "%s/\\2\\4\\5" 
} else { 
  output_name <- "%s/\\2smooth\\4\\5" 
}

# output definition
overwrite = FALSE
files_output <- gsub(
  "^(.+)/(.+)(probs)(.+)(\\.tif)$", 
  sprintf(output_name, local_output_folder), files_input)
if (!dir.exists(local_output_folder)) dir.create(local_output_folder)

# processing ###
cl <- parallel::makeCluster(min(40, length(files_input)))
smoothed_list <- 
  parallel::clusterApply(
    cl, seq_along(files_input), function(i, files_input, files_output, noise, window, overwrite) {
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
# ERROR: i = 1   j = 5   k = 1 ;   NAs 0
#print(sprintf("i %s   j %s   k %s    NAs %s   len_values %s", i, j, k, sum(is.na(values)), length(values) ))
            values[values < 1] <- 1
            values[values > 9999] <- 9999
            values <- sits:::smooth_estimator_class(data   = values, window = window, noise  = noise)
          } else {
            
            values <- matrix(raster::getValuesBlock(raster_prob[[k]], row = block$row[j], nrows = block$nrows[j]),
                             nrow  = block$nrows[j], 
                             byrow = TRUE)
          }
          band_values[, k] <- values
        }
        smooth_raster <- raster::writeValues(smooth_raster, apply(band_values, 1, which.max), block$row[j])
      }
      
      smooth_raster <- raster::writeStop(smooth_raster)
      return(smooth_raster)
    }, files_input, files_output, noise, window, overwrite)
parallel::stopCluster(cl)
