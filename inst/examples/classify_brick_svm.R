#!/usr/bin/Rscript

# Classify sits bricks 
suppressMessages(suppressPackageStartupMessages(library(sits, quietly = TRUE, verbose = FALSE)))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(log4r))
suppressPackageStartupMessages(library(optparse))
library(sits.prodes)

# script setup ----
base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
stopifnot(dir.exists(base_path))
no_data <- -9999 # Landsat no value
path_to_bricks <- c(
   mod13            = file.path(base_path, "data", "raster", "bricks_modis_cropped"),
   l8mod_interp     = file.path(base_path, "data", "raster", "brick_interp"),
   l8mod_starfm     = file.path(base_path, "data", "raster", "brick_starfm"),
   l8_simple        = file.path(base_path, "data", "raster", "brick_simple"),
   l8_mask_cloud    = file.path(base_path, "data", "raster", "brick_mask_cloud")
)
stopifnot(all(vapply(path_to_bricks, dir.exists, logical(1))))

# get arguments ----
option_list = list(
  make_option("--train", type = "character", default = NULL,    help = "Name of a train e.g. 'train_13'", metavar="character"), 
  make_option("--tiles", type = "character", default = NULL,    help = "ID of the tiles to classify e.g. '225063 226064 233067'", metavar="character"), 
  make_option("--btype", type = "character", default = NULL,    help = "Type to brick to use. e.g. 'l8mod_starfm'", metavar="character"), 
  make_option("--bands", type = "character", default = NULL,    help = "Name of the bands to classify e.g. 'ndvi evi nir mir red blue swir2'", metavar="character"), 
  make_option("--years", type = "character", default = NULL,    help = "Years to classify e.g. '2012 2013 2014 2015 2016 2017'", metavar="character"), 
  make_option("--cores", type = "integer",   default = 24,      help = "Number of cores. The default is %default.", metavar = "number"), 
  make_option("--ram",   type = "integer",   default = 96,      help = "Amount of memory to use. The default is %default.", metavar = "number"), 
  make_option("--debug", type = "character", default = "DEBUG", help = "Debug level. The default is %default", metavar="character"), 
  make_option("--log",   type = "character", default = "rep_prodes.log", help = "Path to log file. The default is %default", metavar="character")
) 
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# validate arguments ----
if (length(opt) != 10 || sum(sapply(opt, is.null)) != 0){
  print_help(opt_parser)
  stop("Wrong arguments!")
}
if (!all(opt$btype %in% names(path_to_bricks))){
  print_help(opt_parser)
  stop(sprintf("Invalid brick! The available options are: %s", paste0(names(path_to_bricks), collapse = ", ")))
}
if(parallel::detectCores() < opt$cores){
  print_help(opt_parser)
  stop(sprintf("Not enough cores (The system has %s).", parallel::detectCores()))
}
valid_btypes <- c("l8mod_interp", "l8mod_starfm", "l8_simple", "l8_maskcloud")
if(!(opt$btype %in% valid_btypes)){
  print_help(opt_parser)
  stop(sprintf("Invalid type of brick. The valid options are %s", valid_btypes))
}

# parse arguments ----
train      <- opt$train                                # "train_40"
brick_type <- opt$btype                                # "l8_simple" 
tiles      <- unlist(strsplit(opt$tiles, split = " ")) # c("225063", "226064", "233067")
bands      <- unlist(strsplit(opt$bands, split = " ")) # c("ndvi", "evi", "nir", "mir", "red", "blue", "swir2")
years      <- unlist(strsplit(opt$years, split = " ")) # 2012:2017
multicores <- opt$cores                                # 24
mem        <- opt$ram                                  # 96
cov_timeline <- NULL

# log setup ----
logger                 <- log4r::create.logger()
log4r::logfile(logger) <- file.path(opt$log)
log4r::level(logger)   <- opt$debug
log4r::info(logger, "Initializing...")
log4r::info(logger, "Logging parameters... ")
log4r::info(logger, paste(names(opt), opt, sep = " = "))

# script ----
log4r::info(logger, "Cheking results' directory...")
result_path <- file.path(base_path, "03_classify", stringr::str_replace(train, "train", "rep_prodes"), "results_svm")
if (!dir.exists(result_path)) {
  log4r::info(logger, "Making result's directory...")
  dir.create(result_path, recursive = TRUE)
}
log4r::debug(logger, result_path)

# train
log4r::info(logger, "Loading samples...")
samples_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples"
if (brick_type == "l8mod_interp") {
    data(list = "prodes_samples_interpolated", package = "sits.prodes")
    prodes_samples <- prodes_samples_interpolated
}else if (brick_type == "l8mod_starfm") {
    data(list = "prodes_samples_starfm", package = "sits.prodes")
    prodes_samples <- prodes_samples_starfm
}else if (brick_type == "l8_simple") {
    data(list = "prodes_samples_simple", package = "sits.prodes")
    prodes_samples <- prodes_samples_simple
}else if (brick_type == "l8_maskcloud") {
    data(list = "prodes_samples_mask_cloud", package = "sits.prodes")
    prodes_samples <- prodes_samples_mask_cloud
}else{
    stop("Unknown type of brick")
}
log4r::info(logger, "Training model...")
sits_model <- sits_train(prodes_samples, sits_svm(formula = sits_formula_linear()))
log4r::info(logger, "Classification: support vector machine")
log4r::info(logger, "formula: sits_formula_linear()")

log4r::info(logger, "Saving vector of labels used during training...")
training_labels <- sits_model %>% environment() %>% .[["data.tb"]] %>% 
    dplyr::pull(label) %>% unique() %>% sort()
training_bands <- sits_model %>% environment() %>% .[["data.tb"]] %>% sits::sits_bands()
write.csv(
  matrix(c(training_labels, seq_along(training_labels)), ncol = 2, 
         dimnames = list(NULL, c("Label", "Code"))), 
  file = file.path(result_path, "int_labels.csv"), 
  quote = FALSE, row.names = FALSE)
log4r::info(logger, sprintf("Training label codes saved to %s", file.path(result_path, "int_labels.csv")))

log4r::info(logger, "Gathering bricks' metadata...")
if (brick_type == "mod13") {
  data("timeline_2000_2017", package = "sits")
  cov_timeline <- timeline_2000_2017
  rm(timeline_2000_2017)
}

# brick_tb is the intersection of the bricks available and the user's request
brick_tb <- path_to_bricks[brick_type] %>% 
    ensurer::ensure_that(!is.na(.), err_desc = sprintf("Invalid type of brick: %s", brick_type)) %>%
    list.files(full.names = TRUE, pattern = '*tif$') %>% 
    get_brick_md() %>% dplyr::as_tibble() %>% 
    dplyr::filter(pathrow %in% tiles, year %in% years, band %in% training_bands) %>%
    ensurer::ensure_that(all(.$time_steps == .$time_steps[1]), 
                         err_desc = "Inconsistent number of time-steps among bricks!")

# validate bands
log4r::debug(logger, paste("Bands requested:", paste(bands, collapse = ", ")))
log4r::debug(logger, paste("Bands available:", paste(unique(brick_tb$band), collapse = ", ")))
log4r::debug(logger, paste("Bands training:", paste(training_bands, collapse = ", ")))
if (!identical(sort(unique(brick_tb$band)), sort(training_bands))) {
    msg <- sprintf("Band missmatch between training (%s) and brick (%s)", 
               paste(training_bands, collapse = ", "),
               paste(sort(unique(brick_tb$band)), collapse = ", "))
    log4r::error(logger, msg)
    stop(msg)
}
if (!identical(sort(bands), sort(training_bands))) {
    msg <- sprintf("Band missmatch between request (%s) and brick (%s). Using common bands for Deep Learning classification... ",
               paste(bands, collapse = ", "),
               paste(sort(unique(brick_tb$band)), collapse = ", "))
    log4r::warn(logger, msg)
    warning(msg)
}

# get SITS configuration parameters
sits_conf <- config::get(file = system.file("extdata", "config.yml", package = "sits"))

# classify bricks
img_res <- list()
for(path_row in sort(unique(brick_tb$pathrow))){
  for(y in sort(unique(brick_tb$year))){
    bricks <- brick_tb %>% dplyr::filter(pathrow == path_row, year == y)
    log4r::info(logger, 
                sprintf("Processing path-row %s for year %s for bands %s ...", 
                        path_row, y, paste0(training_bands, collapse = " ")))
    log4r::debug(logger, bricks)
    log4r::info(logger, "Getting coverage metadata...")

    # get MODIS defaults
    scale_factor   <- sits_conf$RASTER_scale_factor$MODIS
    missing_values <- sits_conf$RASTER_missing_value$MODIS
    minimum_values <- sits_conf$RASTER_minimum_value
    maximum_values <- sits_conf$RASTER_maximum_value
    # buid a time line of the same lenght of the brick
    # NOTE: it approximates the dates of the images in the bricks!
    if (stringr::str_detect(brick_type, "^l8.+")) {
        band_names <- c("ndvi", "evi", "blue", "green", "red", "nir", "mir", "swir1", "swir2", "class", "dark", "substrate", "vegetation")
        scale_factor   <- as.list(rep(1/10000, length(band_names)))
        maximum_values <- as.list(rep(10000,   length(band_names)))
        minimum_values <- as.list(rep(0,       length(band_names)))
        missing_values <- rep(no_data, length(band_names))
        names(maximum_values) <- names(minimum_values) <- names(missing_values) <- names(scale_factor) <- band_names
        maximum_values$substrate <- maximum_values$vegetation <- 50000
        t_steps <- as.numeric(unique(bricks$time_steps)[1])
        cov_timeline <- seq(from = as.Date(unique(bricks$start_date)[1]), 
                            by = ceiling(365/t_steps), length.out = t_steps)
    }

    coverage_name <- paste(brick_type, path_row, y, sep = '_')
    scoverage <- sits::sits_coverage(files = bricks$path, name = coverage_name,
                                     bands = bricks$band,
                                     scale_factor = scale_factor, 
                                     maximum_values = maximum_values, 
                                     minimum_values = minimum_values, 
                                     missing_values = missing_values, 
                                     timeline = cov_timeline)
    result_filepath <- file.path(result_path, paste(coverage_name, "svm", sep = "_"))
    param_ls <- list(coverage_name = coverage_name, result_filepath = result_filepath)
    log4r::debug(logger, paste(names(param_ls), param_ls, sep = " = "))
    rm(param_ls)
    sits::sits_classify_raster(file = result_filepath,
                               coverage = scoverage,
                               ml_model = sits_model,
                               memsize = mem,
                               multicores = multicores)
    img_res[[length(img_res) + 1]] <- result_filepath
    log4r::info(logger, paste0("Completed partial bricks classification. The results are stored in ", result_filepath))
  }
}

log4r::info(logger, "Post-processing")
for(f in unlist(img_res)){
    print ("do the bayesian filtering")
}

log4r::info(logger, "Finished!")
