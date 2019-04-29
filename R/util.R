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
    ts_l8mod <- sam_l8m_mod$ts_l8mod[[x]] %>% dplyr::select(l8mod = band) %>% unlist() %>% as.vector()
    ts_modis <- sam_l8m_mod$ts_modis[[x]] %>% dplyr::select(modis = band) %>% unlist() %>% as.vector()
    #
    m <- matrix(c(ts_l8mod, ts_modis), nrow = 2, ncol = length(ts_l8mod))
    dtw_dist <- m %>% dtw::dtwDist() %>% proxy::as.dist() %>% as.vector()
    ts_lm <- lm(formula = ts_l8mod ~ ts_modis)
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
        res <- x %>% dplyr::mutate(
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
        res <- x %>% dplyr::mutate(
            c_evi  = 2.5 * (nir - red)/(nir + 6.0 * red - 7.5 * blue + 1.0 + a),
            # geraVI.py rasterevi = (10000 * 2.5 * (nir - red)/(nir + 6. * red - 7.5 * blue + 1)).astype(numpy.int16)
            c_evi2 = 2.5 * (nir - red)/(nir + 2.4 * red + 1.0 + a),
            c_ndvi = (nir - red)/(nir + red + a)
        )
    }
    return(res)
}


#' @title Compute the confusion matrix between two raster files.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description  Compute the confusion matrix between two raster files.
#'
#' @param r1_path A length-one character. Path to a raster file.
#' @param r2_path A length-one character. Path to a raster file.
#' @param key_ls  A named list used to recode the integer values of r1_path and r2_path. The list is made of labels and its names are the numbers (as character) in the rasters.
#' @return            A list as explained in caret::confusionMatrix
#' export
confusion_raster <- function(r1_path, r2_path, key_ls){
    stopifnot(!tibble::is_tibble(key_ls))
    lev <- names(key_ls)
    lab <- unlist(key_ls)
    stopifnot(length(lev) > 0)
    stopifnot(length(lev) == length(lab))
    stopifnot(all(is.atomic(lev), is.atomic(lab)))
    data_df <- raster::stack(r1_path, r2_path, quick = FALSE)[] %>%
        as.data.frame() %>%
        tidyr::drop_na() %>%
        dplyr::rename("lab_ref_num" = !!names(.[1]),  # reference labels as integers
                      "lab_pred_num" = !!names(.[2])) # predicted labels as integers
    caret::confusionMatrix(data      = factor(data_df$lab_pred_num, levels = lev, labels = lab),
                           reference = factor(data_df$lab_ref_num,  levels = lev, labels = lab)) %>%
        return()
}


#' @title Get metadata from a Brick filename
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description   Get metadata from a Brick filename
#'
#' @param brick_paths A character. Path to dbrick files
#' @return            A character of path, pathrow, start_date, band, year (NA for MODIS)
#' export
get_brick_md <- function(brick_paths){
    # Get the number of bands in a file
    # @param filepath A length-one character. A path to a file
    # @return A length-one numeric. The number of bands
    get_number_of_bands <- function(filepath) {
        system2("gdalinfo", filepath, stdout = TRUE) %>%
        stringr::str_subset("Band") %>% dplyr::last() %>%
        stringr::str_split(" ") %>% unlist() %>% dplyr::nth(2) %>%
        as.numeric() %>% return()
    }
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
        }else if(fn_md[[1]] == "MOD13Q1"){
            res <- c(
                path = x,
                pathrow = tolower(fn_md[2]),
                start_date = "2000-01-01",
                band = tolower(fn_md[7]),
                year = 2000,
                time_steps = get_number_of_bands(x)
            )
        }else{
            stop("Cannot identify the bricks' type!")
        }
        return(res)
    })
    return(as.data.frame(do.call(rbind, brick_df), stringsAsFactors = FALSE))
}


#' @title Replace column values with random numbers
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description   Replace the values of the columns (from the second one on) of a tibble uniform random numbers between 0 and 1
#'
#' @param x       A tibble.
#' @return        A numeric vector.
#' @export
replace_bands_with_random <- function(x){
    for (n in seq_along(x)) {
        if (n > 1) {
            x[[n]] <- runif(length(x[[n]]), 0, 1)
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
#' @return             A raster layer. 
#' @export
vector2raster <- function(vector_sf, raster_r, vector_field){
    stopifnot(lapply(vector_sf, class)[[vector_field]] == "integer")

    tmp_raster_path <- tempfile(pattern = "vector2raster", fileext = ".tif")
    tmp_vector_path <- tempfile(pattern = "vector2raster", fileext = ".shp")
    raster::writeRaster(raster::setValues(raster_r, NA),
                        filename = tmp_raster_path, overwrite = TRUE)
    suppressWarnings(
        vector_sf %>%
        sf::st_transform(crs = raster::projection(raster_r, asText = TRUE)) %>%
        sf::st_write(dsn = tmp_vector_path, delete_dsn = TRUE,
                     quiet = TRUE, delete_layer = TRUE)
    )
    gdalUtils::gdal_rasterize(src_datasource = tmp_vector_path,
                              dst_filename = tmp_raster_path, a = vector_field,
                              l = tools::file_path_sans_ext(basename(tmp_vector_path)),
                              output_Raster = TRUE) %>%
        .[[1]] %>% # Cast to raster layer
        return()
}







# TODO: Review functions -----






#' @title Compute the pixel-wise standard deviation.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute the pixel-wise standard deviation of a set of raster
#' with of same name
#'
#' @param img      A length-one character. A raster name.
#' @param base_dir A length-one character. The path to the rasters.
#' @param cores    A length-one numeric. The available number of cores.
compute_sd_raster <- function(img, base_dir, cores = 1L){
    # stack the rasters
    raster_st <- base_dir %>% list.files(pattern = paste0(img, '$'),
                                         full.names = TRUE, recursive = TRUE) %>%
        raster::stack()
    # compute SD in parallel
    raster::beginCluster(cores, type = 'SOCK')
    raster_sd <- raster::calc(raster_st, fun = stats::sd)
    raster::endCluster()
    return(raster_sd)
}


# Taken from https://gist.github.com/johnbaums/61c8062938e05a4c6b92
#' @title Compute the pixel-wise standard deviation using GDAL.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute the pixel-wise standard deviation of a set of raster
#'
#' @param infile  The multiband raster file (or a vector of paths to multiple
#' raster files) for which to calculate cell standard deviations.
#' @param outfile Path to raster output file.
#' @param quiet   Logical. Should gdal_calc.py output be silenced?
compute_sd_raster_gdal <- function(infile, outfile, quiet=TRUE) {
    gdal_calc <- Sys.which('gdal_calc.py')
    if (gdal_calc == '') stop('gdal_calc.py not found on system.')
    if (file.exists(outfile)) stop('outfile already exists.')
    nbands <- sapply(infile, function(x) nrow(attr(GDALinfo(x), 'df')))
    if (length(infile) > 26 || nbands > 26) stop('Maximum number of inputs is 26.')
    if (length(nbands) > 1 & any(nbands > 1))
        warning('One or more rasters have multiple bands. First band used.')
    if (length(infile) == 1) {
        inputs <- paste0('-', LETTERS[seq_len(nbands)], ' ', infile, ' --',
                         LETTERS[seq_len(nbands)], '_band ', seq_len(nbands),
                         collapse = ' ')
        n <- nbands
    } else {
        inputs <- paste0('-', LETTERS[seq_along(nbands)], ' ', infile, ' --',
                         LETTERS[seq_along(nbands)], '_band 1', collapse = ' ')
        n <- length(infile)
    }

    message('Calculating standard deviation and writing to ', basename(outfile))
    cmd <- 'python %s %s --outfile=%s --calc="std([%s], 0, ddof=1)" --co="COMPRESS=LZW"'
    out <- system(
        sprintf(cmd, gdal_calc, inputs, outfile,
                paste0(LETTERS[seq_len(n)], collapse=',')),
        show.output.on.console=!quiet, intern=TRUE
    )
    if(any(grepl('Error', out))) stop(out, call.=FALSE) else NULL
}





#' @title Compute a confusion matrix
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description        Compute a confusion matrix
#'
#' @param cov_res A results coverage. It must have a variable "label_res".
#' @param cov_ref A reference coverage. It must have a variable "label_ref".
#' @param n       A numeric. The number of samples.
#' @param cores   A numeric. The number of cores to use for parallel processing.
#' @return        A matrix
get_confusion_matrix <- function(cov_res, cov_ref, n, cores = 1L){
    stop("Deprecated. Use asses_accuracy_area instead")
    label_vec <- cov_ref %>% dplyr::select(label_ref) %>%
        sf::st_set_geometry(NULL) %>% unique()
    if( length(label_vec) < 2) warning("length(label_vec) < 2")
    pts_ref <- cov_ref %>% sample_stratified(n * nrow(label_vec), "label_ref", cores)
    pts_res <- cov_res %>% raster::extract(methods::as(pts_ref, "Spatial"), sp = TRUE) %>%
        sf::st_as_sf() %>%
        dplyr::select(label_id = dplyr::contains("mixl8")) %>%
        dplyr::mutate(label_res = dplyr::recode(label_id,
                                                "0" = "Unknown",
                                                "1" = "Deforestation",
                                                "2" = "Forest",
                                                "3" = "No forest",
                                                "4" = "Water")) %>%
        dplyr::select(label_res) %>%
        sf::st_set_geometry(NULL)
    pts_val <- pts_ref %>% sf::st_set_geometry(NULL) %>%
        dplyr::bind_cols(pts_res) %>% stats::na.omit()

    # check if all the result labels are in the reference values
    stopifnot(unique(pts_val[["label_res"]]) %in% unique(pts_val[["label_ref"]]))

    flevels <- pts_val %>% dplyr::select(label_ref, label_res) %>%
        unlist() %>% unique()
    con_mat <- caret::confusionMatrix(factor(pts_val[["label_ref"]], flevels),
                                      factor(pts_val[["label_res"]], flevels))
    return(con_mat)
}


# Given file paths to images, get the scene to which they belong.
#
# @param file_path A charater. Paths to image files.
# @return          A character. The images' Landsat scenes or NA. 
# @export
get_scene <- function(file_path){
    file_path %>% basename() %>%
        stringr::str_extract(pattern = "_[0-9]{6}_") %>%
        stringr::str_sub(2, 7) %>%
        unique() %>%
        return()
}


# Given a list of data.frames, this function adds the list names as columns on each of the data.frames
#
# @param df.list  A list of data.frames
# @return         A list of data frames
listname2data.frame <- function(df.list, colname){
    res <- parallel::mclapply(1:length(df.list),
                              function(x, df.list){
                                  df.list[[x]][colname] <- names(df.list)[x]
                                  return(df.list[[x]])
                              },
                              df.list = df.list
    )
    return(res)
}




#' @title Obtain random samples from polygons
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get random samples from a polygon sf object. By default, this
#' function returns approximately the same number of samples for each unique
#' label. If is_density is true
#'
#' @param sf_shape      A POLYGON sf object.
#' @param label         A length-one character. The name of a variable in sf_shape
#' @param n_samples     A length-one numeric. The number of samples requested. If is_density = TRUE, n_samples is the number of samples per unit of area.
#' @param min_area      A length-one numeric. The minimum area of a sampled polygon.
#' @param min_dist      A length-one numeric. The minimum disatnces between samples.
#' @param border_offset A length-one numeric. The minimum distance from the samples to the polygon_s borders.
#' @param is_density    A length-one logical. Is this a density sampling? The dafault is FALSE
#' @return sf_samples   A point sf object with the structure of sf_shape, plues the X and Y coordinates. The number of rows is approximately n_samples.
sample_polygons <- function(sf_shape, label, n_samples, min_area,
                            min_dist, border_offset,
                            is_density = FALSE){
    stop("Deprecated. Use cov_sample_random or cov_sample_stratified instead") # Use functions in coverage.R
    # filter out invalid geometries
    # sf_shape <- sf_shape[sf::st_is_valid(sf_shape),]
    # buffer to ensure no samples near the polygons_ borders
    if(border_offset != 0){
        suppressMessages(suppressWarnings(
            sf_shape <- sf::st_buffer(sf_shape, dist = base::sqrt(border_offset^2) * (-1))
        ))
    }
    # add temporal variables
    sf_shape["tmp_label"] <- sf::st_set_geometry(sf_shape[label], NULL)
    sf_shape["tmp_area"] <- sf::st_area(sf_shape)
    # filter out small areas and empty geometries
    units(min_area) <- units::as_units(units(sf_shape$tmp_area))
    sf_shape <- sf_shape[sf_shape$tmp_area > min_area,]
    sf_shape <- sf_shape[!sf::st_is_empty(sf_shape),]
    #
    if(nrow(sf_shape) == 0){return(sf_shape)}
    #
    if(is_density){
        # get samples
        n <- round(sum(units::drop_units(sf_shape$tmp_area)) * n_samples,
                   digits = 0)
        suppressMessages(
            samples_sfc <- sf::st_sample(sf_shape, size = n)
        )
        # cast to sf
        sf_samples <- data.frame(id = 1:length(samples_sfc))
        sf::st_geometry(sf_samples) <- samples_sfc
    }else{
        label_vec <- unique(sf_shape$tmp_label)
        samples_per_class <- round(n_samples / length(label_vec), digits = 0)
        # get samples
        samples_sf_ls <- lapply(label_vec, FUN = function(x, sf_obj, n){
            #sf_obj <- sf_obj %>% dplyr::filter(tmp_label == x)
            sf_obj <- sf_obj[sf_obj$tmp_label == x, ]
            suppressMessages(
                samples_sfc <- sf::st_sample(sf_obj, size = n)
            )
            sf_samples <- data.frame(id = seq(along.with = samples_sfc))
            sf::st_geometry(sf_samples) <- samples_sfc
            return(sf_samples)
        }, sf_obj = sf_shape, n = samples_per_class)
        # cast to sf
        sf_samples <- do.call(rbind, samples_sf_ls)
    }
    # check minimum distance between points
    if(min_dist > 0){
        dist_mt <- sf::st_distance(sf_samples)
        units(min_dist) <- units::as_units(units(dist_mt))
        dist_lg <- dist_mt > min_dist
        dist_lg[upper.tri(dist_lg, diag = TRUE)] <- NA
        selected_samples <- apply(dist_lg, MARGIN = 1, all, na.rm = TRUE)
        sf_samples <- sf_samples[selected_samples, ]
    }
    # add coords as attributes
    sf_samples <- cbind(sf_samples, sf::st_coordinates(sf_samples))
    # spatial-join to the original attributes
    suppressMessages(
        sf_samples <- sf::st_join(sf_samples, sf_shape)
    )
    # prepare
    sf_samples["tmp_label"] <- NULL
    sf_samples["tmp_area"] <- NULL
    return(sf_samples)
}


#' @title Get random sample points
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description    Get approximately n samples out of the given coverage
#'
#' @param coverage Either a vector or raster coverage.
#' @param n        Number of samples.
#' @return         A simple feature object of type point.
sample_random <- function(coverage, n){
    stop("Deprecated. Use cov_sample_random instead") # Use cov_sample_random in coverage.R
    sample_points <- NULL
    if("sf" %in% class(coverage)){
        suppressMessages(
            samples_sfc <- sf::st_sample(coverage, size = n)
        )
        # cast back to sf
        sf_samples <- data.frame(id = 1:length(samples_sfc))
        sf::st_geometry(sf_samples) <- samples_sfc
        suppressMessages(
            sample_points <- sf::st_join(sf_samples, coverage)
        )
    }else if("RasterLayer" %in% class(coverage)){
        s_pts <- coverage %>% raster::sampleRandom(size = n, sp = TRUE)
        sample_points <- coverage %>% raster::extract(s_pts, sp = TRUE) %>%
            sf::st_as_sf()
    }else{
        stop("Unknown object!")
    }
    return(sample_points)
}


#' @title Get random samples points
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description    Get approximately n samples out of the given coverage. Each label gets approximately the same number of samples
#'
#' @param coverage Either a vector or raster coverage.
#' @param n        Number of samples.
#' @param label    A length-one character. A label for the samples.
#' @param cores    A length-one numeric. Number of cores for parallel processing.
#' @return         A simple feature object of type point.
sample_stratified <- function(coverage, n, label, cores = 2L){
    stop("Deprecated. Use cov_sample_stratified instead") # Use cov_sample_stratified in coverage.R
    sample_points <- NULL
    if("sf" %in% class(coverage)){
        coverage["tmp_label"] <- sf::st_set_geometry(coverage[label], NULL)
        label_vec <- unique(coverage$tmp_label)
        samples_per_class <- round(n / length(label_vec), digits = 0)
        samples_sf_ls <- parallel::mclapply(label_vec,
                                            FUN = function(x, coverage, samples_per_class){
                                                sf_obj <- coverage %>% dplyr::filter(tmp_label == x) %>%
                                                    sample_random(n = samples_per_class)
                                                return(sf_obj)
                                            }, coverage = coverage, samples_per_class = samples_per_class,
                                            mc.cores = cores)
        # cast to sf
        sample_points <- do.call(rbind, samples_sf_ls)
        sample_points <- sample_points %>% dplyr::select(-tmp_label)
    }else if("RasterLayer" %in% class(coverage)){
        # TODO:
        stop("Not implemented!")
    }else{
        stop("Unknown object!")
    }
    return(sample_points)
}








#' @title Simulate a sits_tibble made of random observations
#' @name sim_sits_tibble
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function simulates a sits_tibble using random data. The
#' time series are simulated using a cosine function.
#'
#' @param n_samples     A length-one numeric. The number of samples per label.
#' @param label         A character. The labels for returned tibble. The default is "label_A".
#' @param lon_mean      A length-one numeric. The central longitude of the random samples. The default is 65 degrees est.
#' @param lon_sd        A length-one numeric. The standard deviation of the longitude of the random samples. The default is 1.
#' @param lat_mean      A length-one numeric. The central latitude of the random samples. The default is 5 degrees south.
#' @param lat_sd        A length-one numeric. The standard deviation of the latitude of the random samples. The default is 1.
#' @param date_start    A length-one character.The start date. The default is "2000/01/01".
#' @param date_end      A length-one character.The start date. The default is "2000/12/31".
#' @param obs_freq      A length-one numeric. The number of observations in the period. The default are 23.
#' @param n_vi          A length-one numeric. The number of vegetation indexes on each sample. The default is 1.
#' @return random_st    A sits_tibble
#'
#' @examples
#' library(sits.prodes)
#' library(sits)
#' my_st <- sim_sits_tibble(10, label = c("A", "B"))
#' sits_plot(my_st)
#'
#' @export
sim_sits_tibble <- function(n_samples, label = "label_A", lon_mean = -65,
                            lon_sd = 1, lat_mean = -5, lat_sd = 1,
                            date_start = "2000/01/01",
                            date_end = "2000/12/31",
                            obs_freq = 23,
                            n_vi = 1){
    # number if samples
    n <- n_samples * length(label)

    # simulate the time series
    index = seq(from = as.Date(date_start), to = as.Date(date_end), length.out = obs_freq)
    ts.lst <- lapply(1:n, FUN = function(x, obs_freq, index, n_vi){
        tmp.lst <- list()
        tmp.lst[["Index"]] <- index
        for(i in 1:n_vi){
            tmp.lst[[paste0("vi", i)]] <- simulate_ts(t_vector = 1:obs_freq)
        }
        return(tibble::as_tibble(tmp.lst))
    }, obs_freq = obs_freq, index = index, n_vi = n_vi)

    # build the sits_tibble
    random_st <- tibble::tibble(longitude   = stats::rnorm(n, lon_mean, lon_sd),
                                latitude    = stats::rnorm(n, lat_mean, lat_sd),
                                start_date  = rep(as.Date(date_start), times = n),
                                end_date    = rep(as.Date(date_end), times = n),
                                label       = rep(label, each = n_samples),
                                coverage    = rep("random_coverage", times = n),
                                time_series = ts.lst)
    class(random_st) <- class(sits::sits_tibble())
    #sits:::.sits_test_tibble(random_st)
    return(random_st)
}



#' @title Simulate a time series of a Vegetation Index (VI). Alber Sanchez
#' @name simulate_ts
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function simulates a time series of vegetation indexes using a cosine function.
#'
#' @param t_vector    Numeric. A vector of time indexes.
#' @param m           A length-one numeric. The mean of the VI.
#' @param amplitude   A length-one numeric. The amplitude of the VI wave.
#' @param freq        A length-one numeric. The frequency of the VI wave.
#' @param phase_shift A length-one numeric. The phase shift of the VI wave.
#' @param noise_mean  A length-one numeric. The mean of the noise.
#' @param noise_sd    A length-one numeric. The stabndard deviation of the noise.
#' @return            A numeric vector
#'
simulate_ts <- function(t_vector, m = 0.5, amplitude = 0.15,
                        freq = 16/365, phase_shift = 0,
                        noise_mean = 0, noise_sd = 0.2){
    angular_freq <- 2 * pi * freq
    noise <- stats::rnorm(length(t_vector), noise_mean, noise_sd)
    return(m + amplitude * cos((angular_freq * t_vector) + phase_shift) + noise)
}







# TODO: Document
# Split a vector at the given positions
# Taken from Calimo at https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
splitAt2 <- function(x, pos) {
    out <- list()
    pos2 <- c(1, pos, length(x)+1)
    for (i in seq_along(pos2[-1])) {
        out[[i]] <- x[pos2[i]:(pos2[i+1]-1)]
    }
    return(out)
}



#' @title Validate a coverage
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compare two coverages.
#' @param cov_res A coverage.
#' @param cov_ref A coverage.
#' @param lab_res A character. The name of the label field in the results coverage.
#' @param lab_ref A character. The name of the label field in the referece coverage.
#' @param n_samples A length-one integer. The number of samples to use during validation. The default is 1000.
#' @param cores     A length-one integer. The number cores to use. The default is 1
validate_sampling <- function(cov_res, cov_ref, lab_res, lab_ref,
                              n_samples = 10000, cores = 1){
    # get sample points
    s_points <- cov_ref %>%
        cov_sample_stratified(n = n_samples,
                              label = lab_ref,
                              cores = cores) %>%
        dplyr::transmute(sample_id = 1:nrow(.))
    # get reference sample points
    ref_points <- cov_ref %>% cov_get_values(s_points)
    # get values at sample points
    res_points <- cov_res %>% cov_get_values(s_points) %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::select(sample_id, label_id = dplyr::contains("mixl8")) %>%
        dplyr::mutate(lab_res = dplyr::recode(label_id,
                                              "0" = "Unknown",
                                              "1" = "Deforestation",
                                              "2" = "Forest",
                                              "3" = "No forest",
                                              "4" = "Water")) %>%
        dplyr::select(sample_id, lab_res)
    # control
    if (nrow(ref_points) != nrow(res_points))
        warning("nrow(ref_points) != nrow(res_points)")
    # Join the data, select and rename label fields
    val_tb <- ref_points %>% sf::st_set_geometry(NULL) %>%
        dplyr::left_join(res_points, by = "sample_id") %>%
        dplyr::select("lab_ref" = lab_ref, "lab_res" = lab_res)
    # validate
    if (sum(stats::complete.cases(val_tb)) != nrow(val_tb)) {
        warning(paste0("Some samples are labeled as NAs: ",
                       sum(stats::complete.cases(val_tb)), "/", nrow(val_tb)))
        val_tb <- stats::na.omit(val_tb)
    }
    # control - check if all the result labels are in the reference values
    stopifnot(unique(val_tb[["lab_res"]]) %in% unique(val_tb[["lab_ref"]]))
    # Compute confusion matrix
    flevels <- val_tb %>% dplyr::select(lab_ref, lab_res) %>%
        unlist() %>% unique()
    con_mat <- caret::confusionMatrix(factor(val_tb[["lab_ref"]], flevels),
                                      factor(val_tb[["lab_res"]], flevels))
    # Compute areas
    cov_areas <- cov_get_areas(cov_ref, label = lab_ref)
    # Asses accuracy
    class_areas <- as.vector(cov_areas$area)
    names(class_areas) <- cov_areas$label
    acc <- asses_accuracy_area(
        error_matrix = as.matrix(as.data.frame.matrix(con_mat$table)),
        class_areas)
    # NOTE:
    # The function sits::sits_accuracy_area() internally calls dtwSat:::.twdtwAssess()
    # dtwSat:::.twdtwAssess(x = error_matrix, mapped_area = class_areas, conf.int = 0.95, rm.nosample = TRUE)
    return(acc)
}
