#!/usr/bin/Rscript

# validate the result of a classification using PRODES

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ensurer))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(sits.prodes))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

# get arguments ----

#TODO: set to FALSE to use as OS script.
sourced <- TRUE
if (sourced) {
  in_dir     <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/smooth_5x5_n10"
  label_file <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/int_labels.csv"
  out_dir    <- "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/results_dl/smooth_5x5_n10/validation2"
} else {
  opt_parser <- list(
    make_option("--in_dir", type = "character", default = NULL,
                help = "Path to a directory with classification results.",
                metavar = "character"),
    make_option("--label_file", type = "character", default = NULL,
                help = "Path to a csv file detailing the labels in the images.",
                metavar = "character"),
    make_option("--out_dir", type = "character", default = NULL,
                help = "Path to a directory where to store the validation results.",
                metavar = "character")
  ) %>% OptionParser()
  opt <- parse_args(opt_parser)
  if (length(opt) != 4 || sum(sapply(opt, is.null)) != 0) {
    print_help(opt_parser)
    stop("Wrong arguments!")
  }
  in_dir     <- opt$in_dir
  label_file <- opt$label_file
  out_dir    <- opt$out_dir
  if (!file.exists(label_file)) {
    print_help(opt_parser)
    stop("File not found!")
  }
  if (!all(vapply(c(in_dir, base_path), dir.exists, logical(1)))) {
    print_help(opt_parser)
    stop(sprintf("Directories not found %s", paste(c(in_dir), collapse = " ")))
  }
}

data(prodes_labels, package = "sits.prodes")
print(sprintf("Starting to process %s using the labels at %s ", in_dir, label_file))

# Check if gdalinfo is able to open the raster file.
is_raster_valid <- function(file_path){
  if (length(file_path) == 1) {
    res <- TRUE
    tool <- "gdalinfo"
    if (any(stringr::str_detect(system(paste(tool, file_path), intern = TRUE), "error|Error|ERROR|failure|Failure|FAILURE")))
      res <- FALSE
    return(res)
  } else if (length(file_path) > 1) {
    return(vapply(file_path, is_raster_valid, logical(1)))
  }
  return(FALSE)
}

# key for encoding PRODES's SHP into a TIF
prodes_labels_ls <- prodes_labels %>%
  dplyr::pull(label_pd) %>%
  as.list()
names(prodes_labels_ls) <- prodes_labels %>%
  dplyr::pull(label_pd_pt)

# get classification labels
labels_csv <- label_file %>%
  read.csv(stringsAsFactors = FALSE)
int_labels        <- labels_csv$Code
names(int_labels) <- labels_csv$Label
rm(labels_csv)

# when the classes are clustered
if (sum(stringr::str_detect(names(int_labels), "_[0-9]+$")) > 0) {
  names(int_labels) <- names(int_labels) %>%
    stringr::str_replace_all(pattern = "_[0-9]+$", replacement = "")
}

# match reference and results keys
unique_prodes_labels <- prodes_labels_ls %>%
  unlist() %>%
  unique() %>%
  sort()
kv_ref_res <- dplyr::full_join(
  dplyr::tibble(key_ref = seq_along(unique_prodes_labels),
                label = unique_prodes_labels),
  dplyr::tibble(key_res = as.character(int_labels),
                label = names(int_labels)),by = "label") %>%
  ensurer::ensure_that(sum(rowSums(is.na(.)) == 0) > 0,
                       err_desc = "No match between the reference and result labels!")

# create the definitive key
available_keys <- kv_ref_res %>%
  dplyr::select(key_res) %>%
  tidyr::drop_na() %>%
  unlist() %>%
  setdiff(as.character(1:nrow(kv_ref_res)), .)
kv_ref_res <- kv_ref_res %>% dplyr::mutate(key = key_res)
kv_ref_res$key[is.na(kv_ref_res$key)] <- available_keys
kv_ref_res <- kv_ref_res %>% dplyr::arrange(key) %>%
  ensurer::ensure_that(sum(is.na(kv_ref_res$key)) == 0,
                       err_desc = "There are missing keys!")
key_labels <- kv_ref_res %>% dplyr::select(label) %>% unlist() %>% as.list()
names(key_labels) <- kv_ref_res %>% dplyr::select(key) %>% unlist()
key_labels_rev <- key_labels %>% names() %>% as.list()
names(key_labels_rev) <- key_labels %>% unlist() %>% as.vector()
rm(available_keys, kv_ref_res)
stopifnot(all(key_labels == names(key_labels_rev)),
          all(names(key_labels) == key_labels_rev))

# Create a CSV with the confusion keys.
key_tb <- tibble::tibble(label = unlist(key_labels),
                         id_label = as.integer(names(key_labels)))
key_tb %>%
  dplyr::rename(label_ref = "label", id_ref = "id_label") %>%
  tidyr::crossing(.$label_ref) %>%
  dplyr::rename(label_res = `.$label_ref`) %>%
  dplyr::left_join(key_tb, by = c("label_res" = "label")) %>%
  dplyr::rename(id_res = id_label) %>%
  dplyr::mutate(id_confusion = (id_ref * 100) + id_res) %>%
  write.csv(file = file.path(out_dir, "confusion_key.csv"))

# Get data for processing.
water_masks <- base_path %>% 
  file.path("data", "raster", "mascaras", "surface_water-Pekel_et_al_2016", "tiled") %>%
  list.files(pattern = "extent_[0-9]{6}[.]tif$", full.names = TRUE) %>%
  ensurer::ensure_that(all(is_raster_valid(.)), err_desc = "Invalid water masks.") %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(scene = file_path %>% 
                  basename() %>% 
                  stringr::str_extract(pattern = "_[0-9]{6}[.]") %>%
                  stringr::str_sub(2, -2)) %>%
  ensurer::ensure_that(nrow(.) > 0, err_desc = "Water masks not found.")

corner_masks <- base_path %>% 
  file.path("data", "raster", "mask_l8_corner") %>%
  list.files(pattern = "LC08_CORNERMASK_[0-9]{6}.tif", full.names = TRUE) %>%
  ensurer::ensure_that(all(is_raster_valid(.)), err_desc = "Invalid corner masks.") %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(scene = file_path %>%
                  basename() %>%
                  stringr::str_extract(pattern = "_[0-9]{6}[.]") %>%
                  stringr::str_sub(2, -2)) %>%
  ensurer::ensure_that(nrow(.) > 0, err_desc = "Corner masks not found.")

quantile_maps <- base_path %>% 
  file.path("data", "raster", "cloud_count") %>%
  list.files(pattern = "*quantiles.tif", full.names = TRUE) %>%
  ensurer::ensure_that(all(is_raster_valid(.)), err_desc = "Invalid quantile maps.") %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(scene = file_path %>%
                  basename() %>%
                  stringr::str_extract(pattern = "_[0-9]{6}_") %>% 
                  stringr::str_sub(2, -2),
                pyear = file_path %>%
                  basename() %>%
                  stringr::str_extract(pattern = "_[0-9]{4}_") %>% 
                  stringr::str_sub(2, -2)) %>%
  ensurer::ensure_that(nrow(.) > 0, err_desc = "Quantile maps not found.")

prodes_maps <- base_path %>%
  file.path("data", "vector","prodes", "prodes_tiled") %>%
  list.files(pattern = "PDigital2017_AMZ_pol_[0-9]{3}_[0-9]{3}[.]shp", full.names = TRUE) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(scene = file_path %>%
                  basename() %>%
                  stringr::str_extract(pattern = "_[0-9]{3}_[0-9]{3}[.]") %>% 
                  stringr::str_sub(2, -2) %>% 
                  stringr::str_replace('_', '')) %>%
  ensurer::ensure_that(nrow(.) > 0, err_desc = "Prodes maps not found.")

res_map_pattern <- "^l8_(simple|maskcloud)_[0-9]{6}_[0-9]{4}_(dl|rf|svm|dl-rf-svm)_[0-9]{4}_[0-9]_[0-9]{4}_[0-9](.tif|_vote.tif)"

validation_tb <- in_dir %>%
  list.files(pattern = res_map_pattern, full.names = TRUE,
             include.dirs = FALSE) %>%
  ensurer::ensure_that(length(.) > 0, 
                       err_desc = sprintf("No classified images found at %s", 
                                          in_dir)) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(scene = file_path %>%
                  basename() %>%
                  stringr::str_extract(pattern = "_[0-9]{6}_") %>%
                  stringr::str_sub(2, -2),
                pyear = file_path %>% 
                  basename() %>%
                  stringr::str_extract_all(pattern = "_[0-9]{4}_") %>%
                  vapply(., dplyr::last, character(1)) %>%
                  stringr::str_sub(2, -2),
                r_res = purrr::map(file_path, raster::raster)) %>%
  dplyr::left_join(prodes_maps, by = "scene", suffix = c("", "_prodes")) %>%
  dplyr::left_join(water_masks, by = "scene", suffix = c("", "_water")) %>%
  dplyr::left_join(corner_masks, by = "scene", suffix = c("", "_corner")) %>%
  dplyr::left_join(quantile_maps, by = c("scene", "pyear"), suffix = c("_res", "_quantile")) %>%
#------------------------------------
#TODO: remove
#dplyr::slice(3:6) %>%
#------------------------------------
  # Rasterize PRODES to match the result map.
  dplyr::mutate(r_prodes = purrr::pmap(
    dplyr::select(., 
      file_pd = file_path_prodes,
      file_rt = file_path_res,
      tile = scene,
      year_pd = pyear),
    prodes2raster, 
    raster_path = tempfile(pattern = "prodes2raster_", fileext = ".tif"),
    prodes_lbl = prodes_labels %>% dplyr::filter(label_pd %in% names(int_labels)))) %>%
  # Mask the result map using PRODES
  dplyr::mutate(res_masked_prodes = purrr::pmap(dplyr::select(., r_res, r_prodes), function(r_res, r_prodes){
    print("Masking results using PRODES...")
    raster::mask(is.na(r_res), mask = r_prodes, maskvalue = 1) %>%
      raster::writeRaster(filename = tempfile(pattern = "res_masked_prodes_", fileext = ".tif")) %>%
      return()
  })) %>%
  # Project and resample the water masks.
  dplyr::mutate(r_water = purrr::pmap(dplyr::select(., file_path_water, res_masked_prodes), function(file_path_water, res_masked_prodes){
    print("Processing WATER masks...")
    file_path_water %>%
      raster::raster() %>%
      raster::projectRaster(crs = raster::crs(res_masked_prodes, asText = TRUE), method = "ngb") %>%
      raster::resample(y = res_masked_prodes, method = "ngb") %>%
      raster::crop(y = raster::extent(res_masked_prodes)) %>%
      raster::writeRaster(filename = tempfile(pattern = "water_mask_", fileext = ".tif")) %>%
      return()
  })) %>%
  # Mask the results using water maps.
  dplyr::mutate(res_masked_water = purrr::pmap(dplyr::select(., res_masked_prodes, r_water), function(res_masked_prodes, r_water){
    print("Masking results using WATER masks...")
    res_masked_prodes %>%
      raster::mask(mask = r_water, maskvalue = 1) %>%
      raster::writeRaster(filename = tempfile(pattern = "res_masked_water_", fileext = ".tif")) %>%
      return()
  })) %>%
  # NOTE: The corner masks aren't doing a thing. The PRODES mask already removed the corners.
  ## Project and resample the corner masks.
  #dplyr::mutate(r_corner = purrr::pmap(dplyr::select(., file_path_corner, res_masked_water), function(file_path_corner, res_masked_water){
  #  file_path_corner %>%
  #    raster::raster() %>%
  #    raster::projectRaster(crs = raster::crs(res_masked_water, asText = TRUE), method = "ngb") %>%
  #    raster::resample(y = res_masked_water, method = "ngb") %>%
  #    raster::crop(y = raster::extent(res_masked_water)) %>%
  #    raster::writeRaster(filename = tempfile(pattern = "corner_mask_", fileext = ".tif")) %>%
  #    return()
  #})) %>%
  ## Mask the results using the corner masks.
  #dplyr::mutate(res_masked_corner = purrr::pmap(dplyr::select(., res_masked_water, r_corner), function(res_masked_water, r_corner){
  #  res_masked_water %>%
  #    raster::mask(mask = r_corner) %>%
  #    raster::writeRaster(pattern = tempfile(pattern = "res_masked_corner_", fileext = ".tif")) %>%
  #    return()
  #})) %>% 
  # Save results to disk.
  dplyr::mutate(out_res = purrr::pmap(dplyr::select(., res_masked_water, file_path_res), function(res_masked_water, file_path_res, out_dir){
    print("Saving masked results to disc...")
    raster::writeRaster(res_masked_water, 
                        filename = file.path(out_dir, 
                                             stringr::str_replace(basename(file_path_res), 
                                             ".tif", "_masked.tif")), 
                        overwrite = TRUE)
  }, out_dir = out_dir)) %>%
  # NOTE: Deprecated. Use r_confusion instead.
  ## Compute the difference between the reference and results rasters.
  #dplyr::mutate(ref_res_diff = purrr::pmap(dplyr::select(., r_prodes, out_res), function(r_prodes, out_res, out_dir){
  #  r <- r_prodes - out_res
  #  r %>% raster::writeRaster(filename = file.path(out_dir, 
  #                                                 stringr::str_replace(basename(out_res@file@name),
  #                                                                      ".tif",
  #                                                                      "_masked_refdiff.tif")),
  #                            overwrite = TRUE)
  #}, out_dir = out_dir)) %>%
  # Computing a confusion raster ranged from 0100 t0 9999. The most significant 
  # digits (one or two) are the referece id while the two least significant are 
  # the result id.
  dplyr::mutate(r_confusion = purrr::pmap(dplyr::select(., r_prodes, out_res), function(r_prodes, out_res, out_dir){
    r <- (r_prodes * 100) + out_res
    r %>% raster::writeRaster(filename = file.path(out_dir, 
                                                   stringr::str_replace(basename(out_res@file@name), 
                                                                        ".tif", 
                                                                        "_confusion.tif")),
                              datatype = "INT2S",
                              overwrite = TRUE)
  }, out_dir = out_dir)) %>%
  # Project and resample the quantile maps.
  dplyr::mutate(r_quantile = purrr::pmap(dplyr::select(., file_path_quantile, out_res), function(file_path_quantile, out_res, out_dir){
    file_path_quantile %>%
      raster::raster() %>%
      raster::projectRaster(crs = raster::crs(out_res, asText = TRUE), method = "ngb") %>%
      raster::resample(y = out_res, method = "ngb") %>%
      raster::crop(y = raster::extent(out_res)) %>%
      raster::writeRaster(filename = tempfile(pattern = "quantile_", fileext = ".tif")) %>%
      return()
  }, out_dir = out_dir)) %>%
  # Compute acuracy by cloud-quantile.
  dplyr::mutate(validation_quantile_data = purrr::pmap(dplyr::select(., r_confusion, r_quantile), function(r_confusion, r_quantile, key_labels){
    q_tb <- raster::crosstab(r_confusion, r_quantile, long = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::rename("confusion" = 1, "quantile" = 2) %>%
      dplyr::filter(quantile > 0, quantile < 5) %>%
      dplyr::mutate(ref = floor(confusion / 100), 
                    res = confusion - (ref * 100)) %>%
      dplyr::mutate(lab_ref = dplyr::recode(ref, !!!key_labels),
                    lab_res = dplyr::recode(res, !!!key_labels)) %>%
      dplyr::select(lab_ref, lab_res, quantile, Freq)
    quantile_accuracy <- lapply(1:4, function(x, q_tb){
      q_tb %>% 
        dplyr::filter(quantile == x) %>%
        dplyr::select(-quantile) %>%
        stats::xtabs(Freq ~ lab_res + lab_ref, data = .) %>%
        as.matrix() %>%
        asses_accuracy_simple() %>%
        return()
    }, q_tb = q_tb)
    names(quantile_accuracy) <- paste0("quantile_", seq_along(quantile_accuracy))
    return(quantile_accuracy)
  }, key_labels = key_labels)) %>%
  # Build a reference-result data.frame.
  dplyr::mutate(validation_data = purrr::pmap(dplyr::select(., r_prodes, out_res), function(r_prodes, out_res, key_labels, prodes_labels_ls){
    ref_res_df <- raster::stack(r_prodes, out_res, quick = FALSE)[] %>%
      as.data.frame() %>%
      tidyr::drop_na() %>%
      dplyr::rename("lab_ref_num" = 1, "lab_res_num" = 2) %>%
      dplyr::mutate(lab_ref = dplyr::recode(lab_ref_num, !!!key_labels),
                    lab_res = dplyr::recode(lab_res_num, !!!key_labels)) %>%
      dplyr::filter(lab_res %in% unlist(prodes_labels_ls))
    flevels <- ref_res_df %>%
      dplyr::select(lab_ref, lab_res) %>%
      unlist() %>%
      unique()
    con_mat <- caret::confusionMatrix(data = factor(ref_res_df[["lab_res"]],
                                                    flevels),
                                      reference = factor(ref_res_df[["lab_ref"]],
                                                         flevels))
    # computing reference areas
    lab_ref <- NULL
    ref_areas <- cov_get_areas(r_prodes, label = lab_ref) %>%
      dplyr::mutate(lab_ref = dplyr::recode(label, !!!key_labels)) %>%
      tidyr::drop_na()
    # asses accuracy
    class_areas <- as.vector(ref_areas$area)
    names(class_areas) <- ref_areas$lab_ref
    asses_accuracy_area(error_matrix = as.matrix(as.data.frame.matrix(con_mat$table)),
                        class_areas) %>%
    return()
  }, key_labels = key_labels, prodes_labels_ls = prodes_labels_ls))

# Store accuracy results.
res_file <- file.path(out_dir, "validation_tb.rds")
print(sprintf("Saving results to %s", res_file))
validation_tb %>% 
  dplyr::mutate(r_res             = purrr::map_chr(r_res,             function(x){x@file@name}),
                r_prodes          = purrr::map_chr(r_prodes,          function(x){x@file@name}),
                res_masked_prodes = purrr::map_chr(res_masked_prodes, function(x){x@file@name}),
                r_water           = purrr::map_chr(r_water,           function(x){x@file@name}),
                res_masked_water  = purrr::map_chr(res_masked_water,  function(x){x@file@name}),
                out_res           = purrr::map_chr(out_res,           function(x){x@file@name}),
                r_confusion       = purrr::map_chr(r_confusion,       function(x){x@file@name}),
                r_quantile        = purrr::map_chr(r_quantile,        function(x){x@file@name})) %>%
  saveRDS(file = res_file)

print("Finished!")

