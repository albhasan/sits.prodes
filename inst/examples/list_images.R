#!/usr/bin/env Rscript

# LIST THE IMAGES AVAILABLE FOR BUILDING BRICKS.

library(dplyr)

# list the downloaded landsat images
landsat_targz_tb <- "/disks/d7/LANDSAT" %>%
    list.files(pattern = "tar[.]gz", full.names = TRUE, recursive = TRUE) %>%
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
    dplyr::mutate(date = date_time %>%
                  stringr::str_sub(1, 8) %>%
                  lubridate::as_date()) %>%
    dplyr::arrange(mission, level, tile, date)


