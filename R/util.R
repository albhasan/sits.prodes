#' @title Test if a character can be casted to numeric.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description  Test if a character can be casted to numeric.
#'
#' @param chr A length-one character.
#' @return    A logical.
#' @export
castable2numeric <- function(chr){
    is.numeric(as.numeric(chr)) & !is.na(chr)
}

#' @title Util function for computing similarity parameters between two time series
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description    Compute comparison parameters between two time series
#'
#' @param x           A numeric. A row number in sam_l8m_mod
#' @param band        A length-one character. The name of a band present in both L8-MOD and MODIS time-series.
#' @param sam_l8m_mod A sits tibble with two list-columns for the L8-MOD (ts_l8mod) and MODIS (ts_modis) time series.
#' @return            A numeric vector.
#' @export
compare_ts <- function(x, band, sam_l8m_mod){
    ts_l8mod <- sam_l8m_mod$ts_l8mod[[x]] %>%
        dplyr::select(l8mod = band) %>%
        unlist() %>%
        as.vector()
    ts_modis <- sam_l8m_mod$ts_modis[[x]] %>%
        dplyr::select(modis = band) %>%
        unlist() %>%
        as.vector()
    #
    m <- matrix(c(ts_l8mod, ts_modis), nrow = 2, ncol = length(ts_l8mod))
    dtw_dist <- m %>%
        dtw::dtwDist() %>%
        proxy::as.dist() %>%
        as.vector()
    ts_lm <- stats::lm(formula = ts_l8mod ~ ts_modis)
    res <- c(ts_lm$coefficients, summary(ts_lm)$r.squared, dtw_dist)
    names(res) <- c("intercept", "slope", "r.squared", "dtw_dist")
    return(res)
}


#' @title Compute indexes
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description   Compute vegetation indexes
#'
#' @param x       A tibble.
#' @param sat     A Length-one character. The satellite or sensor name in c("Landsat8", "MOD13").
#' @return        A tibble with additional variables (prefixed with "c_").
#' @export
compute_indexes <- function(x, sat){
    a <- 0.0001 # avoid division by 0
    if (sat == "Landsat8") {
        nir <- red <- swir2 <- NULL
        res <- x %>%
            ensurer::ensure_that(c("nir", "red", "swir2") %in% colnames(.)) %>%
            dplyr::mutate(
                # USGS. (2017). LANDSAT surface reflectance-derived spectral indices. Retrieved from https://landsat.usgs.gov/sites/default/files/documents/si_product_guide.pdf (page 14)
                #c_evi   = 2.5 * ((nir - red)/(nir + 6.0 * red - 7.5 * blue + 1.0 + a)),
                c_msavi = (2 * nir + 1.0 - sqrt((2.0 * nir + 1.0)^2.0 - 8.0 * (nir - red))) / 2.0,
                c_nbr   = (nir - swir2) / (nir + swir2 + a),
                #c_nbr2   = (swir1 - swir2) / (swir1 + swir2 + a),
                #c_ndmi  = (swir - swir2) / (swir + swir2 + a),

                c_ndvi  = (nir - red)/(nir + red + a),
                # geraVI.py rasterndvi = (10000 * (nir - red) / (nir + red + 0.0001)).astype(numpy.int16)

                c_savi  = ((nir - red) / (nir + red + 0.5 + a)) * (1.5),

                # evi2 is NOT documented for Landsat!!!
                c_evi2 = 2.5 * (nir - red)/(nir + 2.4 * red + 1.0 + a)
            )
    } else if (sat == "MOD13") {
        # Didan, K., Barreto Munoz, A., Solano, R., & Huete, A. (2015). MODIS Vegetation Index User’s Guide (MOD13 Series), 2015(June). Retrieved from http://vip.arizona.edu (page 3)
        blue <- NULL
        res <- x %>%
            ensurer::ensure_that("blue" %in% colnames(.)) %>%
            dplyr::mutate(
                c_evi  = 2.5 * (nir - red)/(nir + 6.0 * red - 7.5 * blue + 1.0 + a),
                # geraVI.py rasterevi = (10000 * 2.5 * (nir - red)/(nir + 6. * red - 7.5 * blue + 1)).astype(numpy.int16)
                c_evi2 = 2.5 * (nir - red)/(nir + 2.4 * red + 1.0 + a),
                c_ndvi = (nir - red)/(nir + red + a)
            )
    }
    return(res)
}


#' @title Get metadata from a Brick filename
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get metadata from a Brick's file name.
#'
#' @param brick_paths A character. Path to brick files.
#' @return            A character of path, pathrow, start_date, band, year (NA for MODIS).
#' @export
get_brick_md <- function(brick_paths){
    # @title Get the number of bands in a file.
    # @author Alber Sanchez, \email{albequietr.ipia@@inpe.br}
    # @description Get the number of bands in a file.
    #
    # @param filepath A character. Path to a file.
    # @return         A numeric.
    # Adapted from sits.starfm
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
    #
    brick_df <- lapply(brick_paths, function(x){
        fn <- substr(basename(x), 1, nchar(basename(x)) - 4)
        fn_md <- strsplit(fn, split = '_')[[1]]
        res <- NULL
        if (stringr::str_detect(fn_md[[1]], "^LC8SR.+")) {
            res <- c(
                path = x,
                pathrow = fn_md[2],
                start_date = fn_md[3],
                band = fn_md[4],
                year = format(as.Date(fn_md[[3]]), '%Y'),
                time_steps = get_number_of_bands(x)
            )
        } else if (fn_md[[1]] == "MOD13Q1") {
            res <- c(
                path = x,
                pathrow = tolower(fn_md[2]),
                start_date = "2000-01-01",
                band = tolower(fn_md[7]),
                year = 2000,
                time_steps = get_number_of_bands(x)
            )
        } else if (stringr::str_detect(fn_md[[1]], "^HLS[L|S][0-9]{2}.+")) {
            res <- c(
                path = x,
                pathrow = fn_md[2],
                start_date = fn_md[3],
                band = fn_md[4],
                year = format(as.Date(fn_md[[3]]), '%Y'),
                time_steps = get_number_of_bands(x)
            )
        }else{
            stop("Cannot identify the bricks' type!")
        }
        return(res)
    })
    return(as.data.frame(do.call(rbind, brick_df), stringsAsFactors = FALSE))
}


#' @title Get metadata from an image.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description   Get metadata from a image's path.
#'
#' @param img_path A character. Path to a file.
#' @return         A list of character.
#' export
get_img_md <- function(img_path){
    img_raster <- img_path %>% raster::raster()
    img_extent <- img_raster %>% raster::extent()
    param_crs <- paste0("'", raster::projection(img_raster), "'")
    param_extent_output <- c(img_extent@xmin, img_extent@ymin,
                             img_extent@xmax, img_extent@ymax)
    param_ncol <- ncol(img_raster)
    param_nrow <- nrow(img_raster)
    param_img_size <- c(param_ncol, param_nrow)
    param_pixel_size_x <- raster::xres(img_raster)
    param_pixel_size_y <- raster::yres(img_raster)

    return(list(crs = param_crs,
                extent_output = param_extent_output,
                ncol = param_ncol, nrow = param_nrow, img_size = param_img_size,
                pixel_size_x = param_pixel_size_x,
                pixel_size_y = param_pixel_size_y))
}


#' @title Identify bricks missing in directory.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description   Given sets of years, scenes, bands, check if thre are bricks available for them.
#'
#' @param in_dir             A length-one character. A path to a directory of brick files.
#' @param expected_scenes    A character. Expected path and rows.
#' @param expected_years     A character. Expected years.
#' @param expected_bands     A character. Expected bands.
#' @param brick_file_pattern A length-one character· A regular expression to match brick files.
#' @return                   A tibble.
#' @export
identify_missing_bricks <- function(in_dir, expected_scenes, expected_years,
                                    expected_bands,
                                    brick_file_pattern = "^LC8SR-(MASKCLOUD|MOD13Q1-MYD13Q1|MOD13Q1-STARFM|RAW|SIMPLE)_[0-9]{6}_[0-9]{4}-[0-9]{2}-[0-9]{2}_[a-z0-9]{+}_STACK_BRICK.tif$"){
    found <- pathrow <- NULL
    # Get bricks' metadata.
    brick_md <- in_dir %>%
        list.files(pattern = brick_file_pattern, full.names = TRUE) %>%
        ensurer::ensure_that(length(.) > 0,
                             err_desc = sprintf("No brick files found at %s",
                                                in_dir)) %>%
        get_brick_md() %>%
        tibble::as_tibble()

    # Build a tibble of expected bricks.
    test_tb <- expand.grid(expected_scenes, expected_years,
                           expected_bands, stringsAsFactors = FALSE) %>%
        dplyr::as_tibble()
    colnames(test_tb) <- c("e_scene", "e_year", "e_band")

    # Find the missing bricks.
    test_tb %>%
        dplyr::mutate(found = purrr::pmap_lgl(., function(e_scene, e_year, e_band, brick_md){
            pathrow <- year <- band <- NULL
            brick_md %>%
                ensurer::ensure_that(c("pathrow", "year", "band") %in% colnames(.)) %>%
                dplyr::filter(pathrow == e_scene,
                              year == e_year,
                              band == e_band) %>%
                nrow(.) == 1 %>%
                return()
        }, brick_md = brick_md)) %>%
        dplyr::filter(found == FALSE) %>%
        dplyr::select(-found) %>%
        return()
}


#' @title Mask a raster.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Mask a raster using either a vector or another raster.
#'
#' @param file_path  A character. Path to a raster file.
#' @param mask       Either an sf or raster object. A mask to be applied to the input.
#' @param band       An integer. The band identifier in the input file.
#' @param resampling A character. A resamplig method.
#' @return           A raster object.
mask_raster <- function(file_path, mask, band = 1, resampling = "ngb"){
  if (is.null(mask) || (is.atomic(mask) && is.na(mask)))
    return(NA)
  r <- file_path %>%
    raster::raster(band = band)

  if (sum(class(mask) %in% c("sf")) > 0) {
    my_mask <- mask %>%
      sf::st_transform(crs = raster::crs(r)) %>%
      sf::as_Spatial()
  } else if (sum(class(mask) %in% c("RasterLayer")) > 0) {
    my_mask <- mask %>%
      raster::projectRaster(to = r, method = resampling)
  } else {
    stop(sprintf("Unknown type of mask: %s", class(mask)))
  }

  r %>%
    raster::mask(mask = my_mask, filename = tempfile(pattern = "mask_raster_",
                                                fileext = ".tif")) %>%
    return()
}


#' @title Replace column values with random numbers.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description   Replace the values of the columns (from the second one on) of a tibble uniform random numbers between 0 and 1
#'
#' @param x       A tibble.
#' @return        A numeric vector.
#' @export
replace_bands_with_random <- function(x){
    for (n in seq_along(x)) {
        if (n > 1) {
            x[[n]] <- stats::runif(length(x[[n]]), 0, 1)
        }
    }
    return(x)
}


#' @title Split a vector at the given positions.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Split a vector at the given positions.
#'
#' @param x   An atomic vector.
#' @param pos A vector os positions
#' @return   A list.
#' @export
splitAt <- function(x, pos){
    stopifnot(is.atomic(x))
    f <- cut(1:length(x), pos)
    split(x, f)
}


#' @title Rasterize vectors
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Rasterize a sf object to match the given raster
#'
#' @param vector_sf    A sf object.
#' @param raster_r     A raster object. This raster is the reference for coordinate sytem, spatial resolution, and dimensions.
#' @param vector_field A length-one character. The name of an integer column in vector_sf.
#' @param raster_path  A length-one character. An optional file path to store the resulting raster.
#' @return             A raster layer.
#' @export
vector2raster <- function(vector_sf, raster_r, vector_field,
                          raster_path = tempfile(pattern = "vector2raster", fileext = ".tif")){
    #vector_sf = sf::st_read("/home/alber/Documents/data/experiments/prodes_reproduction/data/vector/prodes/prodes_tiled", "PDigital2017_AMZ_pol_225_063") %>% dplyr::mutate(label_id = 1:n())
    #raster_r = raster::raster("/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/mapbiomas/amazonia/reclas2prodes/Classification_2013_22563_prodes.tif")
    #vector_field = "label_id"
    #no_data = -9999
    #raster_path = tempfile(pattern = "vector2raster", fileext = ".tif")

    stopifnot(lapply(vector_sf, class)[[vector_field]] == "integer")

    raster::writeRaster(raster::setValues(raster_r, NA),
                        filename = raster_path,
                        overwrite = TRUE,
                        options = c("BIGTIFF=YES"))
    tmp_vector_path <- tempfile(pattern = "vector2raster", fileext = ".shp")
    suppressWarnings(
        vector_sf %>%
            sf::st_transform(crs = raster::projection(raster_r, asText = TRUE)) %>%
            sf::st_write(dsn = tmp_vector_path,
                         delete_dsn = TRUE,
                         quiet = TRUE,
                         delete_layer = TRUE)
    )
    gdalUtils::gdal_rasterize(src_datasource = tmp_vector_path,
                              dst_filename = raster_path,
                              a = vector_field,
                              l = tools::file_path_sans_ext(basename(tmp_vector_path)),
                              output_Raster = TRUE) %>%
        .[[1]] %>% # Cast to raster layer, why? Because fuck you R! That's why
        return()
}
