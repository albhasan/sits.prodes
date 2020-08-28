#!/usr/bin/env Rscript

# LIST THE IMAGES AVAILABLE FOR BUILDING BRICKS.

library(dplyr)

# list the downloaded landsat images
landsat_targz_tb <- "/disks/d7/LANDSAT" %>%
    list.files(pattern = "[A-Z0-9|-][.]tar[.]gz$", full.names = TRUE, recursive = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = file_path %>%
                      basename() %>%
                      tools::file_path_sans_ext()) %>%
    tidyr::separate(col = file_name,
                    into = c("type", "pathrow", "date", "collection",
                             "category", NA, "download", NA),
                    sep = c(4, 10, 18, 20, 22, 25, 33)) %>%
    dplyr::mutate(date =     lubridate::as_date(date),
                  download = lubridate::as_date(download)) %>%
    dplyr::arrange(type, pathrow, date, collection, date, category, download)


# report images which don't belong here
landsat_targz_tb %>%
    dplyr::filter(type != "LC08"|| collection != "01") %>%
    ensurer::ensure_that(nrow(.) == 0,
                          err_desc = "Some images aren't L8 or collection 1")

# Build the expected time lines
first_tb <- landsat_targz_tb %>%
    dplyr::select(pathrow, date) %>%
    dplyr::group_by(pathrow) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(first_date = date)
last_tb <- landsat_targz_tb %>%
    dplyr::select(pathrow, date) %>%
    dplyr::group_by(pathrow) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(last_date = date)
time_line_tb <- first_tb %>%
    dplyr::left_join(last_tb, by = "pathrow") %>%
    dplyr::mutate(date = purrr::map2(first_date, last_date,
                                          seq, by = 16)) %>%
    tidyr::unnest(cols = c(date))
rm(first_tb, last_tb)

# Check if the time series match the available images.
# NOTE: Remember there are uncompressed images
landsat_targz_tb <- landsat_targz_tb %>%
    dplyr::full_join(time_line_tb, by = c("pathrow", "date"))
landsat_targz_tb %>%
    dplyr::filter(is.na(file_path)) %>%
    dplyr::arrange(type, pathrow, collection, date) %>%
    print(n = Inf)

# List the Sentinel2 images.
sentinel_tb <- c("/disks/d5/sen2agri/archive/maccs_def",
                 "/disks/d1/sen2agri_2/archive/maccs_def") %>%
    list.dirs(full.names = TRUE, recursive = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(dir_path = value) %>%
    dplyr::filter(stringr::str_detect(dir_path, pattern = "[.]SAFE$")) %>%
    dplyr::mutate(dir_name = dir_path %>%
                      basename() %>%
                      tools::file_path_sans_ext()) %>%
    tidyr::separate(col = dir_name, into = c("mission", "level", "date_time",
                                             "baseline", "rel_orbit", "tile",
                                             "discriminator"), sep = '_') %>%
    dplyr::mutate(img_date = date_time %>%
                      stringr::str_sub(1, 8) %>%
                      lubridate::as_date(),
                  img_month = lubridate::month(img_date),
                  img_year  = lubridate::year(img_date)) %>%
    dplyr::arrange(mission, level, tile, img_date) %>%
    dplyr::mutate(time_resolution = img_date - dplyr::lag(img_date, lag = 1)) %>%
    # Count the number of consequtive occurrences of date with the same temporal resolution.
    dplyr::mutate(n_consecutive = sequence(rle(as.character(time_resolution))$lengths)) %>%
    # Count the number of files in each Sentinel image's directory.

# TODO:
# - Select the best among images of the same date.
# - Find the longest sequence of time resolution of 10 days.

# Longest time series of images with temporal resolution of 10 days and more
# than 35 images in July.
brick_tile <- "T22MCA"
brick_pyear <- 2019
complete_brick <- sentinel_tb %>%
    # Dismiss other time resolutions and recompute.
    dplyr::filter(time_resolution == 10) %>%
    dplyr::mutate(time_resolution = img_date - dplyr::lag(img_date, lag = 1)) %>%
    dplyr::mutate(n_consecutive = sequence(rle(as.character(time_resolution))$lengths)) %>%
    # Select the last image in series longer than 350 days that finish in July.
    dplyr::filter(time_resolution == 10,
                  img_month == 7,
                  n_consecutive >= 35) %>%
    dplyr::group_by(tile, img_year, img_month) %>%
    dplyr::arrange(tile, img_year, n_consecutive, .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(tile == brick_tile,
                  img_year == brick_pyear) %>%
    dplyr::select(tile, img_date) %>%
    dplyr::mutate(brick = paste(brick_tile, brick_pyear, sep = '_')) %>%
    ensurer::ensure_that(nrow(.) == 1,
                         err_desc = "Found more than one record!") %>%
    dplyr::mutate(time_line = purrr::map(img_date, function(to_date){
                                             seq(from = to_date, by = -10,
                                                 length.out = 36)})) %>%
    tidyr::unnest(time_line)


# NOTE: It requires permissions over user scidb's directory.
sentinel_tb %>%
    dplyr::right_join(complete_brick, by = c("tile", "img_date")) %>%
    dplyr::arrange(tile, img_date) %>%
    (function(x){
         print(x, n = Inf)
         invisible(x)
    }) %>%
    # Copy the images to another location
    dplyr::pull(dir_path) # %>% file.copy(to = "/home/scidb/docker_fmask4/data", recursive = TRUE)



