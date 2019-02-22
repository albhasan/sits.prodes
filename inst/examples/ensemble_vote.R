
# ensemble the results of classification
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(optparse))
library(sits.prodes)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

# get arguments ----
option_list = list(
  make_option("--experiment", type = "character", default = NULL, help = "Name of an experiment e.g. 'rep_prodes_40'", metavar="character"),
  make_option("--algorithm",  type = "character", default = NULL, help = "Name of an algorithm e.g. 'dl'", metavar="character"),
  make_option("--smooth_dir", type = "character", default = NULL, help = "Name of a smooth directory e.g. 'smooth_3x3_n10'", metavar="character"),
  make_option("--cores",      type = "integer",   default = 1,    help = "Number of cores for running in parallel [default %default]", metavar="character")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
if (length(opt) != 5 || sum(sapply(opt, is.null)) != 0){
  print_help(opt_parser)
  stop("Wrong arguments!")
}
experiment <- opt$experiment # "rep_prodes_40"
algorithm  <- opt$algorithm  # "dl"
smooth_dir <- opt$smooth_dir # "smooth_3x3_n10"
cores      <- opt$cores      # 8

dl_dir  <- file.path(base_path, "03_classify", experiment, "results_dl",  smooth_dir)
rf_dir  <- file.path(base_path, "03_classify", experiment, "results_rf",  smooth_dir)
svm_dir <- file.path(base_path, "03_classify", experiment, "results_svm", smooth_dir)
stopifnot(all(vapply(c(base_path, dl_dir, rf_dir, svm_dir), dir.exists, logical(1))))

img_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_(dl|rf|svm)_[0-9]{4}_[0-9]_[0-9]{4}_[0-9].tif"

result_tb <- function(path){
    path %>% list.files(pattern = img_pattern, full.names = TRUE) %>%
        ensurer::ensure_that(length(.) > 0, err_desc = sprintf("No classified images found in %s", path)) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename("file_path" = "value") %>%
        dplyr::mutate(experiment = stringr::str_extract(file_path, "rep_prodes_[0-9]+"),
                      algorithm = purrr::map_chr(.$file_path, function(x){
                          unlist(stringr::str_split(stringr::str_extract(x, "results_[a-z]+"), '_'))[2]
                      }),
                      smooth = stringr::str_extract(file_path, "smooth_[0-9]x[0-9]_n[0-9]{2}"),
                      scene = stringr::str_extract(basename(file_path), "[0-9]{6}"),
                      pyear = purrr::map_chr(file_path, function(x){
                          as.integer(dplyr::last(unlist(stringr::str_extract_all(basename(x), "[0-9]{4}"))))
                      })) %>%
        return()
}

raster_tb <- dl_dir %>% result_tb() %>%
    dplyr::rename(dl_file = file_path) %>% 
    dplyr::full_join(result_tb(rf_dir), by = c("experiment", "smooth", "scene", "pyear")) %>%
    dplyr::rename(rf_file = file_path) %>% 
    dplyr::full_join(result_tb(svm_dir), by = c("experiment", "smooth", "scene", "pyear")) %>%
    dplyr::rename(svm_file = file_path) %>% 
    dplyr::select(-tidyselect::starts_with("algorithm")) %>%
    dplyr::select(experiment, smooth, scene, pyear, dl_file, rf_file, svm_file)

stack_ls <- purrr::pmap(
    list(
        as.list(raster_tb$dl_file),
        as.list(raster_tb$rf_file),
        as.list(raster_tb$svm_file)
    ),
    raster::stack
)

# taken from https://stackoverflow.com/questions/26726028/how-to-pick-the-most-frequent-values-mode-from-a-raster-stack 
Mode <- function(x) {
  ux <- unique(x)
  ux <- ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}
Mode_count <- function(x) {
  ux <- unique(x)    
  ux <- ux[!is.na(ux)]
  mode <- ux[which.max(tabulate(match(x, ux)))]
  sum(x == mode, na.rm = T)
}

raster_mode_ls      <- parallel::mclapply(stack_ls, raster::calc, fun = Mode, mc.cores = cores)
raster_mode_freq_ls <- parallel::mclapply(stack_ls, raster::calc, fun = Mode_count, mc.cores = cores)

out_dir <- file.path(dl_dir, "ensemble_vote")
if(!dir.exists(out_dir)) dir.create(out_dir)

for(i in seq_along(raster_mode_ls)){
    fname <- raster_tb %>% dplyr::select(dl_file) %>% dplyr::slice(i) %>% unlist() %>% basename() %>% stringr::str_replace("dl", "dl-rf-svm") %>% stringr::str_replace(".tif", '')
    print(sprintf("Writting result to %s", file.path(out_dir, paste0(fname, "_vote.tif"))))
    raster::writeRaster(raster_mode_ls[[i]], file.path(out_dir, paste0(fname, "_vote.tif")))
    raster::writeRaster(raster_mode_freq_ls[[i]], file.path(out_dir, paste0(fname, "_vote-freq.tif")))
}

print("Finished!")

