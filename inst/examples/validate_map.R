#!/usr/bin/env Rscript

# validate the result of a classification using PRODES

# get arguments ----
suppressMessages(library(optparse))
option_list <- list(
  make_option("--in_dir", type = "character", default = NULL,
              help = "Path to a directory with classification results.",
              metavar = "character"),
  make_option("--label_file", type = "character", default = NULL,
              help = "Path to a csv file detailing the labels in the images.",
              metavar = "character"),
  make_option("--out_dir", type = "character", default = NULL,
              help = "Path to a directory where to store the validation results.",
              metavar = "character")
)

arguments <- parse_args(OptionParser(option_list = option_list))
if (!dir.exists(arguments$in_dir))
  stop(sprintf("Directory not found: %s", arguments$in_dir))
if (!file.exists(arguments$label_file))
  stop(sprintf("File not found: %s", arguments$label_file))
if (!dir.exists(arguments$out_dir)) {
  warning(sprintf("Output directory not found. Attempting to create it: %s", arguments$out_dir))
  dir.create(arguments$out_dir)
}

in_dir     <- arguments$in_dir
label_file <- arguments$label_file
out_dir    <- arguments$out_dir
rm(option_list, arguments)
detach(package:optparse)

# script ----
suppressMessages(library(dplyr))
suppressMessages(library(ensurer))
#suppressMessages(library(sits.prodes))
#-----------------

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction" %>%
  ensurer::ensure_that(dir.exists(.), err_desc = sprintf("Directory not found: %s", .))

data(prodes_labels, package = "sits.prodes")
print(sprintf("Starting to process %s using the labels at %s ", in_dir, label_file))

# key for encoding PRODES's SHP into a TIF
prodes_labels_ls <- prodes_labels %>%
  dplyr::pull(label_pd) %>%
  as.list() %>%
  magrittr::set_names(prodes_labels$label_pd_pt)

# get classification labels
labels_csv <- label_file %>%
  read.csv(stringsAsFactors = FALSE)
int_labels        <- labels_csv %>%
  dplyr::pull(Code) %>%
  magrittr::set_names(labels_csv$Label)
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
                label = names(int_labels)), by = "label") %>%
  ensurer::ensure_that(sum(rowSums(is.na(.)) == 0) > 0,
                       err_desc = "No match between the reference and result labels!")

# create the definitive key
available_keys <- kv_ref_res %>%
  dplyr::select(key_res) %>%
  tidyr::drop_na() %>%
  unlist() %>%
  setdiff(as.character(1:nrow(kv_ref_res)), .)
kv_ref_res <- kv_ref_res %>%
  dplyr::mutate(key = key_res)
kv_ref_res$key[is.na(kv_ref_res$key)] <- available_keys
kv_ref_res <- kv_ref_res %>%
  dplyr::arrange(key) %>%
  ensurer::ensure_that(sum(is.na(kv_ref_res$key)) == 0,
                       err_desc = "There are missing keys!")
key_labels <- kv_ref_res %>%
  dplyr::select(label) %>%
  unlist() %>%
  as.list() %>%
  magrittr::set_names(dplyr::pull(kv_ref_res, key))
key_labels_rev <- key_labels %>%
  names() %>%
  as.list() %>%
  magrittr::set_names(as.vector(unlist(key_labels)))
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
  file.path("data", "raster", "mascaras", "surface_water-Pekel_et_al_2016",
            "tiled") %>%
  list.files(pattern = "extent_[0-9]{6}[.]tif$", full.names = TRUE) %>%
  ensurer::ensure_that(all(is_raster_valid(.)),
                       err_desc = "Invalid water masks.") %>%
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
  ensurer::ensure_that(all(is_raster_valid(.)),
                       err_desc = "Invalid corner masks.") %>%
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
  file.path("data", "vector","prodes", "tiled") %>%
  list.files(pattern = "PDigital2017_AMZ_pol_[0-9]{3}_[0-9]{3}[.]shp",
             full.names = TRUE) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(scene = file_path %>%
                  basename() %>%
                  stringr::str_extract(pattern = "_[0-9]{3}_[0-9]{3}[.]") %>%
                  stringr::str_sub(2, -2) %>%
                  stringr::str_replace('_', '')) %>%
  ensurer::ensure_that(nrow(.) > 0, err_desc = "Prodes maps not found.")

deter_maps <- base_path %>%
  file.path("data", "vector", "deter", "tiled") %>%
  list.files(pattern = "deter_[0-9]+[.]shp$", full.names = TRUE) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(deter_file_path = "value") %>%
  dplyr::mutate(file_name = tools::file_path_sans_ext(basename(deter_file_path))) %>%
  tidyr::separate(col = file_name, into = c(NA, "scene"))

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
                  stringr::str_sub(2, -2)) %>%
  dplyr::left_join(prodes_maps, by = "scene", suffix = c("", "_prodes")) %>%
  dplyr::left_join(water_masks, by = "scene", suffix = c("", "_water")) %>%
  dplyr::left_join(corner_masks, by = "scene", suffix = c("", "_corner")) %>%
  dplyr::left_join(quantile_maps, by = c("scene", "pyear"), suffix = c("_res", "_quantile")) %>%
  dplyr::left_join(deter_maps, by = "scene", suffix = c("_res", "_quantile")) %>%
  # Rasterize PRODES to match the result map.
  dplyr::mutate(prodes_rasterized = purrr::pmap_chr(
    dplyr::select(., file_pd = file_path_prodes, file_rt = file_path_res,
                  tile = scene, year_pd = pyear),
    function(file_pd, file_rt, tile, year_pd, prodes_lbl){
      raster_path = tempfile(pattern = "prodes2raster_", fileext = ".tif")
      prodes2raster(file_pd = file_pd, file_rt = file_rt,
                    raster_path = raster_path, tile = tile, year_pd = year_pd,
                    prodes_lbl = prodes_lbl)
    },
    prodes_lbl = prodes_labels %>%
      dplyr::filter(label_pd %in% names(int_labels)))) %>%
  # Mask the result map using PRODES
  dplyr::mutate(res_masked_prodes = purrr::pmap_chr(dplyr::select(., file_path_res, prodes_rasterized), function(file_path_res, prodes_rasterized){
    print(sprintf("Masking results using PRODES %s %s", file_path_res, prodes_rasterized))
    if (any(vapply(c(file_path_res, prodes_rasterized), is.na, logical(1))))
        return(NA)
    r_res <- file_path_res %>%
      raster::raster()
    r_prodes <- prodes_rasterized %>%
      raster::raster() %>%
      raster::projectRaster(crs = raster::crs(r_res, asText = TRUE), method = "ngb") %>%
      raster::resample(y = r_res, method = "ngb") %>%
      raster::crop(y = raster::extent(r_res))
    out_file <- tempfile(pattern = "res_masked_prodes_", fileext = ".tif")
    r_res %>%
      raster::mask(mask = is.na(r_prodes), maskvalue = 1) %>%
      raster::writeRaster(filename = out_file)
    rm(r_res, r_prodes)
    return(out_file)
  })) %>%
  # Mask the results using water maps.
  dplyr::mutate(res_masked_water = purrr::pmap_chr(dplyr::select(., res_masked_prodes, file_path_water), function(res_masked_prodes, file_path_water){
    print("Masking results using WATER masks...")
    if (any(vapply(c(res_masked_prodes, file_path_water), is.na, logical(1))))
      return(NA)
    r_res <- res_masked_prodes %>%
      raster::raster()
    r_water <- file_path_water %>%
      raster::raster() %>%
      raster::projectRaster(crs = raster::crs(r_res, asText = TRUE), method = "ngb") %>%
      raster::resample(y = r_res, method = "ngb") %>%
      raster::crop(y = raster::extent(r_res))
    out_file <- tempfile(pattern = "res_masked_water_", fileext = ".tif")
    r_res %>%
      raster::mask(mask = r_water, maskvalue = 1) %>%
      raster::writeRaster(filename = out_file)
    rm(r_res, r_water)
    return(out_file)
  })) %>%



  # Save results to disk.
  dplyr::mutate(out_res = purrr::pmap_chr(dplyr::select(., res_masked_water, file_path_res), function(res_masked_water, file_path_res, out_dir){
    print("Saving masked results to disk...")
    if (any(vapply(c(res_masked_water, file_path_res), is.na, logical(1))))
      return(NA)
    out_file <- file.path(out_dir, stringr::str_replace(basename(file_path_res), ".tif", "_masked.tif"))
    file.copy(from = res_masked_water, to = out_file, overwrite = TRUE)
    return(out_file)
  }, out_dir = out_dir)) %>%
  # Computing a confusion raster ranged from 0100 t0 9999. The most significant
  # digits (one or two) are the referece id while the two least significant are
  # the result id.
  dplyr::mutate(res_confusion = purrr::pmap_chr(dplyr::select(., prodes_rasterized, out_res), function(prodes_rasterized, out_res, out_dir){
    if (any(vapply(c(prodes_rasterized, out_res), is.na, logical(1))))
      return(NA)
    r_res <- out_res %>%
      raster::raster()
    r_prodes <- prodes_rasterized %>%
      raster::raster() %>%
      raster::projectRaster(crs = raster::crs(r_res, asText = TRUE), method = "ngb") %>%
      raster::resample(y = r_res, method = "ngb") %>%
      raster::crop(y = raster::extent(r_res))
    r <- (r_prodes * 100) + r_res
    out_file <- file.path(out_dir, stringr::str_replace(basename(out_res), ".tif", "_confusion.tif"))
    r %>% raster::writeRaster(filename = out_file,
                              datatype = "INT2S",
                              overwrite = TRUE)
    rm(r_prodes, r_res, r)
    return(out_file)
  }, out_dir = out_dir)) %>%
  # Compute acuracy by cloud-quantile.
  dplyr::mutate(r_quantile = purrr::pmap(dplyr::select(., out_res, res_confusion, file_path_quantile), function(out_res, res_confusion, file_path_quantile, key_labels, out_dir){
    if(any(vapply(c(out_res, res_confusion, file_path_quantile), is.na, logical(1))))
      return(NA)
    r_res <- out_res %>%
      raster::raster()
    r_confusion <- res_confusion %>%
      raster::raster()
    r_quantile <- file_path_quantile %>%
      raster::raster() %>%
      raster::projectRaster(crs = raster::crs(r_res, asText = TRUE), method = "ngb") %>%
      raster::resample(y = r_res, method = "ngb") %>%
      raster::crop(y = raster::extent(r_res))
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
    rm(r_res, r_confusion, r_quantile, q_tb)
    return(quantile_accuracy)
  }, key_labels = key_labels, out_dir = out_dir)) %>%
  # Build a reference-result data.frame.
  dplyr::mutate(validation_data = purrr::pmap(dplyr::select(., prodes_rasterized, out_res), function(prodes_rasterized, out_res, key_labels, prodes_labels_ls){
    if (any(vapply(c(prodes_rasterized, out_res), is.na, logical(1))))
      return(NA)
    r_res <- out_res %>%
      raster::raster()
    r_prodes <- prodes_rasterized %>%
      raster::raster() %>%
      raster::projectRaster(crs = raster::crs(r_res, asText = TRUE), method = "ngb") %>%
      raster::resample(y = r_res, method = "ngb") %>%
      raster::crop(y = raster::extent(r_res))
    ref_res_df <- raster::stack(r_prodes, r_res, quick = FALSE)[] %>%
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
    if (length(flevels) < 2) {
      warning("Found less than 2 labels!")
      return(NA)
    }
    con_mat <- caret::confusionMatrix(data = factor(ref_res_df[["lab_res"]],
                                                    flevels),
                                      reference = factor(ref_res_df[["lab_ref"]],
                                                         flevels))
    # computing reference areas
    lab_ref <- NULL
    ref_areas <- cov_get_areas(r_prodes, label = lab_ref) %>%
      dplyr::mutate(lab_ref = dplyr::recode(label, !!!key_labels)) %>%
      tidyr::drop_na()
    rm(r_res, r_prodes, ref_res_df)
    # asses accuracy
    class_areas <- as.vector(ref_areas$area)
    names(class_areas) <- ref_areas$lab_ref
    asses_accuracy_area(error_matrix = as.matrix(as.data.frame.matrix(con_mat$table)),
                        class_areas) %>%
    return()
  }, key_labels = key_labels, prodes_labels_ls = prodes_labels_ls))

# Store accuracy results.
out_file <- file.path(out_dir, "validation_tb.rds")
print(sprintf("Saving results to %s", out_file))
validation_tb %>%
  saveRDS(file = out_file)
print("Finished!")

