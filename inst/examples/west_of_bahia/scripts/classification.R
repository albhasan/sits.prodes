#!/usr/bin/env Rscript 

# GET TIME SERIES FROM A POING SHAPEFILE AND A SITS BRICK.

#---- Configuration ----

require(dplyr)
#require(sits)
devtools::load_all()
require(sf)

shp_path   <- "/home/alber/Documents/ghProjects/sits.prodes/inst/examples/west_of_bahia/samples/samples.shp"
brick_path <- "/home/alber/Documents/ghProjects/sits.prodes/inst/examples/west_of_bahia/bricks"

#---- Util functions ----

# Get the number of bands in a file.
get_number_of_bands <- function(filepath) {
	stopifnot(is.atomic(filepath))
	if (is.na(filepath) || length(filepath) < 1) return(NA)
	if (length(filepath) == 1) {
		system2("gdalinfo", filepath, stdout = TRUE) %>%
			stringr::str_subset("Band") %>% dplyr::last() %>%
			stringr::str_split(" ") %>% unlist() %>% dplyr::nth(2) %>%
			as.integer() %>%
			return()
	} else {
		return(vapply(filepath, get_number_of_bands, integer(1)))
	}
}

#---- Script ----

# Get Bricks' metadata
brick_tb <- brick_path %>%
    list.files(pattern = "*.tif$", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = basename(file_path)) %>%
    tidyr::separate(file_name, sep = '_',
                    into = c("type", "tile", "start_date", "band", NA, NA)) %>%
    dplyr::mutate(start_date = as.Date(start_date),
                  n_bands = get_number_of_bands(file_path)) %>%
    # NOTE: Only allowed bricks of one year of monthly data.
    dplyr::filter(n_bands == 12) %>%
    dplyr::mutate(time_line = purrr::map2(start_date, n_bands, 
											function(x, y){
                                                seq(from = x, by = "month", length.out = y)
											}),
                  scale_factor = 1/10000,
                  missing_value = -9999,
                  minimum_value = 0,
                  maximum_value = 10000) %>%
    ensurer::ensure_that(nrow(.) > 0, err_desc = "No bricks found!")

# Read the shp, drop the geometry, fix the column names, and save as CSV.
shp <- shp_path  %>%
    ensurer::ensure_that(file.exists(.), err_desc = "File not found!") %>%
	sf::st_read(quiet = TRUE, stringsAsFactors = FALSE) %>%
    sf::st_transform(crs = 4326)
coords <- shp %>%
	sf::st_coordinates() %>%
	dplyr::as_tibble()
csv_path <- paste0(tools::file_path_sans_ext(shp_path), ".csv")
shp %>%
	dplyr::mutate(longitude = coords$X, latitude = coords$Y) %>%
	sf::st_set_geometry(value = NULL) %>%
	dplyr::as_tibble() %>%
    dplyr::mutate(start_date = as.character(dplyr::first(brick_tb$time_line[[1]])),
                  end_date   = as.character(dplyr::last (brick_tb$time_line[[1]])),
                  label = Classe_0,
                  coverage = "west_of_bahia",
                  time_series = NA,
                  cube = NA,
                  id = 1:dplyr::n()) %>%
    dplyr::select(c("id", "longitude", "latitude", "start_date", 
                             "end_date", "label", "cube", "time_series")) %>%
    utils::write.csv(file = csv_path, quote = FALSE, row.names = FALSE)

# Get a sits coverage.
time_line <- as.character(brick_tb$time_line[[1]])
raster_cube <- sits::sits_cube( service = "BRICK",
                                name     = "west_bahia",
                                satellite = "CBERS4",
                                sensor = "WFI",
                                #tiles_names = NULL,
                                #geom = NULL,
                                from = dplyr::first(time_line),
                                to = dplyr::last(time_line),
                                timeline = time_line, 
                                bands    = brick_tb$band, 
                                files    = brick_tb$file_path)

samples_file <- "/home/alber/Documents/ghProjects/sits.prodes/inst/examples/west_of_bahia/data/samples_tb.rds"
if(file.exists(samples_file)) {
    samples_tb <- readRDS(samples_file)
} else {
    samples_tb <- sits::sits_get_data(raster_cube, file = csv_path) %>%
        sits::sits_prune() %>%
        ensurer::ensure_that(nrow(.) > 1, err_desc = "No valid samples found!") %>%
        saveRDS(file = samples_file)
}

# NOTE: SVM erorrs in original sits
#ml_model <- sits::sits_train(samples_tb)
#Error in na.fail.default(list(`factor(reference)` = c(1L, 1L, 1L, 1L,  :
#  missing values in object
#ml_model <- sits::sits_train(samples_tb, sits::sits_rfor(num_trees = 3000))
#Error: Missing data in columns: ndvi2, ndvi6.

# NOTE: This only runs with the modifyed sits on alber@esensing-6
ml_model <- sits::sits_train(samples_tb, sits::sits_svm())

# NOTE: not working after changes
#ml_model <- sits::sits_train(samples_tb, sits::sits_rfor(num_trees = 500))

ml_probs <- sits::sits_classify(data = raster_cube, ml_model = ml_model)

# label the classified image
wbahia_label <- sits::sits_label_classification(ml_probs)

# smooth the result with a bayesian filter
wbahia_bayes <- sits_label_classification(ml_probs, smoothing = "bayesian")

