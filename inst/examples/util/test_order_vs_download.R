#!/usr/bin/Rscript
###############################################################################
# COMPARE DOWNLOADED FILES AGAINST LIST
#------------------------------------------------------------------------------
# Last update 2018-05-21
###############################################################################

suppressMessages(library(dplyr))

base_path <- '/disks/d7/LANDSAT'

stopifnot(dir.exists(base_path))

# list the downloaded files
images_tb <- base_path %>%
    file.path("orders") %>%
    ensurer::ensure_that(file.exists(.), err_desc = "Directory not found!") %>%
    list.files(pattern = "[.]tar[.]gz$", recursive = TRUE, full.names = TRUE) %>%
    ensurer::ensure_that(length(.) > 0, err_desc = "No files found!") %>%
    tibble::enframe(name = NULL)  %>%
    dplyr::rename(image_path = value) %>%
    dplyr::mutate(file_name = basename(image_path)) %>%
    tidyr::separate(file_name, into = c("satellite", "path_row", 
                                        "img_acquisition", "collection", "tier",
                                         "stuff"), sep = c(4, 10, 18, 20, 22 )) %>%
    dplyr::select(-stuff)

# list the expected images.
orders_tb <- base_path %>%
    file.path("order_list") %>%
    ensurer::ensure_that(dir.exists(.), err_desc = "Directory not found!") %>%
    list.files(pattern = "[.]txt$", full.names = TRUE) %>%
    ensurer::ensure_that(length(.) > 0, err_desc = "No files found!") %>%
    tibble::enframe(name = NULL)  %>%
    dplyr::rename(order_path = value) %>%
    dplyr::mutate(order_id = tools::file_path_sans_ext(basename(order_path)),
                  image_id = purrr::map(order_path, readr::read_csv, 
                                        col_names = "image_id", col_types = "c")) %>%
    tidyr::unnest() %>%
    tidyr::separate(image_id, into = c("satellite", "level", "path_row", 
                                       "img_acquisition", "img_processing", 
                                       "collection", "tier")) %>%
    dplyr::left_join(images_tb, by = c("satellite", "path_row", 
                                       "img_acquisition", "collection", "tier"))

print("Missing images from orders: ")
orders_tb %>%
    dplyr::filter(is.na(image_path)) %>%
    dplyr::pull(order_id) %>%
    unique()

