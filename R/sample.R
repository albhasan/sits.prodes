# FUNCTIONS FOR SAMPLE PROCESSING
# alber sanchez alber.ipia@inpe.br
# Last update 2018-09-08
# TODO:
# NOTES:


#' @title Load sample time series from Rdata files
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Load (and filter) sample time series from Rdata files.
#'
#' @param x             A character. A path to a file of samples time series.
#' @param sat           A Length-one character. The satellite or sensor name in c("Landsat8", "MOD13").
#' @param expected_nrow A lengh-one inetger. Keep the samples with this number of time steps.
#' @return              A tibble or a list of tibbles.
load_samples <- function(x, sat, expected_nrow){
    stopifnot(is.character(x))

    # util functions
    # test if a vector elements are the same. Take from https://stackoverflow.com/questions/4752275/test-for-equality-among-all-elements-of-a-single-vector
    zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
        if (length(x) == 1) return(TRUE)
        x <- range(x) / mean(x)
        isTRUE(all.equal(x[1], x[2], tolerance = tol))
    }
    # validate the tme sereis of a SITS sample
    is_ts_valid <- function(ts_tb, expected_nrow){
        if (!dplyr::is.tbl(ts_tb)) return(FALSE)
        if (nrow(ts_tb) != expected_nrow || ncol(ts_tb) < 2) return(FALSE)
        if (!any(vapply(dplyr::select(ts_tb, -1), is.numeric, logical(1)))) return(FALSE)
        # is there any NA?
        if (any(vapply(ts_tb, function(x) any(is.na(unlist(ts_tb))), logical(1))))
            return(FALSE)
        # are all the observations the same? This could be NAs, etc
        if (any(vapply(dplyr::select(ts_tb, -1), zero_range, logical(1)))) return(FALSE)
        return(TRUE)
    }
    # body
    if (is.na(x) || length(x) == 0) {
        return(NA)
    }else if (length(x) == 1) {
        samples.tb <- NULL
        load(x)
        if (is.null(samples.tb)) return(NA)
        res <- samples.tb %>%
            dplyr::arrange(longitude, latitude, start_date, end_date, label) %>%
            dplyr::mutate(valid = purrr::map_lgl(time_series, is_ts_valid, expected_nrow)) %>%
            dplyr::filter(valid == TRUE) %>%
            dplyr::select(-valid)
        if (!is.null(sat))
            res <- res %>%
            dplyr::mutate(time_series = purrr::map(time_series, compute_indexes,
                                                   sat = sat))
        return(res)
    } else {
        return(lapply(x, load_samples, sat, expected_nrow))
    }
}


#' @title Process a shapefile of validated samples.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get validated PRODES samples and split them by label in year
#' into a table (csv). These processed samples are ready for other script to
#' retrieve time-series
#'
#' @param shp_path A length-one character. Path to a shapefile.
#' @param out_dir  A length-one character. Path to a folder.
#' @return         A vector of paths to CSV files
process_valid_shp <- function(shp_path, out_dir){
    # read shp, drop geometry, fix column names
    shp <- shp_path  %>% sf::st_read(quiet = TRUE, stringsAsFactors = FALSE)
    coords <- shp %>% sf::st_coordinates() %>% dplyr::as_tibble()
    csv <- shp %>% dplyr::mutate(longitude = coords$X, latitude = coords$Y) %>%
        dplyr::rename(time_series = time_serie) %>%
        sf::st_set_geometry(NULL) %>% dplyr::as_tibble()

    # check for a valid id field
    id_col <- csv %>% dplyr::select(tidyselect::starts_with('tmp_id'))
    if (nrow(id_col) > 0 && ncol(id_col) > 0) {
        id_col <- as.integer(as.vector(unlist(id_col[, ncol(id_col)]))) # take the last tmp_id used
        if (length(id_col) != length(unique(id_col)))
            id_col <- 1:nrow(csv) # recompute id if it is not valid
    }else{
        id_col <- 1:nrow(csv)
    }
    csv <- csv %>% dplyr::mutate(id = id_col) %>%
        dplyr::select(-tidyselect::starts_with("tmp_id")) %>%
        dplyr::select(id, tidyselect::everything())



    # update date ranges
    start_md <- "-08-01" # "-08-20"
    end_md   <- "-08-31" # "-08-19"

    res <- csv[0,]
    for (y in 2013:2017) {
        label_all_years <- c("FLORESTA", "NAO_FLORESTA", "NAO_FLORESTA2", "HIDROGRAFIA", paste0('d', y))
        res <- csv %>% dplyr::filter(label %in% label_all_years) %>%
            dplyr::mutate(start_date = paste0(y - 1, start_md)) %>%
            dplyr::mutate(end_date   = paste0(y,     end_md)) %>%
            dplyr::bind_rows(res)
    }
    for (y in 2014:2017) {
        label_some_years <- c("flood")
        res <- csv %>% dplyr::filter(label %in% label_some_years) %>%
            dplyr::mutate(start_date = paste0(y - 1, start_md)) %>%
            dplyr::mutate(end_date   = paste0(y,     end_md)) %>%
            dplyr::bind_rows(res)
    }

    # re-label
    res <- res %>%  dplyr::mutate(label = dplyr::recode(label,
                                                        "FLORESTA" = "forest" ,
                                                        "NAO_FLORESTA" = "no forest",
                                                        "NAO_FLORESTA2" = "no forest 2",
                                                        "HIDROGRAFIA" = "water",
                                                        "d2012" = 'deforestation',
                                                        "d2013" = 'deforestation',
                                                        "d2014" = 'deforestation',
                                                        "d2015" = 'deforestation',
                                                        "d2016" = 'deforestation',
                                                        "d2017" = 'deforestation',
                                                        "d2018" = 'deforestation',
                                                        "flood" = "flood"
    ))

    # write a file per year
    csv_files <- character()
    for (d in unique(res$start_date)) {
        csv_d <- res %>% dplyr::filter(start_date == d)
        fname <- file.path(out_dir, paste0(substr(basename(shp_path), 1, nchar(basename(shp_path)) - 4), '_', d, '.csv'))
        csv_files <- append(csv_files, fname)
        print(paste0("Writing ", nrow(csv_d), " samples to file ", fname))
        print(table(csv_d$label))
        utils::write.csv(csv_d, file = fname, quote = FALSE, row.names = FALSE)
    }
    return(csv_files)
}



#' @title Get time series for validates sample points.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get validated PRODES samples (from a CSV file) and retrieve time
#' series for each one
#'
#' @param cpath        A length-one character. Path to a CSV file.
#' @param path_bricks  A character. Path to a directory with Landsdat8-MODIS bricks.
#' @param brick_prefix A character. Prefix to the name of the Landsdat8-MODIS bricks.
#' @param class_bands  A character. The name of the bands to process.
#' @param scale_factor   A list. Scale factorof the image band files.
#' @param missing_values A list. Values representing missing pixels for each band.
#' @param minimum_values A list. Minimum values of each band.
#' @param maximum_values A list. Maximum values of each band.
#' @param suffix         A length-one character. A suffix to append to the filename of the results
#' @param max_time_diff  A length-one numeric. The maximum difference (in days) between the fist day of the bricks and the first day of the CSV sample file.
#' @param cov_name     A length-one character. Name for the sits coverage to create.
#' @param time_len     A length-one integer. The lenght of the brick's time line.
#' @param time_by      A length-one inetger. The number of days between observations in the time line.
#' @return             A vector of paths to Rdata files
get_timeseries <- function(cpath, path_bricks, brick_prefix, class_bands,
                           scale_factor = NULL, missing_values = NULL, 
                           minimum_values = NULL, maximum_values = NULL,
                           suffix = "", max_time_diff = 30, 
                           cov_name = "SITS coverage", time_len = 23,
                           time_by = 16){
    stopifnot(length(cpath) == 1)

    # SITS default parameteres
    sits_conf <- config::get(file = system.file("extdata", "config.yml", package = "sits"))
    if (is.null(scale_factor))
        scale_factor   <- sits_conf$RASTER_scale_factor$LANDSAT
    if (is.null(missing_values))
        missing_values <- sits_conf$RASTER_missing_value$LANDSAT
    if (is.null(minimum_values))
        minimum_values <- sits_conf$RASTER_minimum_value
    if (is.null(maximum_values))
        maximum_values <- sits_conf$RASTER_maximum_value

    # get metadata from brick's name
    pathrow <- cpath %>% basename() %>% stringr::str_extract("_[0-9]{3}_[0-9]{3}_") %>%
        strsplit(split = '_', fixed = TRUE) %>% unlist() %>%
        paste(collapse = '')
    start_date <- cpath %>% basename() %>% stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
    if (class(try(as.Date(start_date))) == "try-error" || is.na(try(as.Date(start_date))))
        stop("Invalid start_date in file name ", cpath)

    # build a data.frame of bricks' metadata
    brick_tb <- path_bricks %>% 
        list.files(pattern = paste0(brick_prefix, pathrow, '_*'), 
                   full.names = TRUE) %>% 
        tibble::enframe(name = NULL) %>%
        dplyr::rename(path = value) %>%
        dplyr::mutate(
            pathrow = stringr::str_extract(basename(path), "[0-9]{6}"),
            year = stringr::str_extract(basename(path), "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
            band = stringr::str_sub(stringr::str_extract(basename(path), "_[a-z]+[0-9]?_"), 2, -2),
            dif_time = abs(as.numeric(difftime(as.Date(year), as.Date(start_date), units = 'days')))
        ) %>%
        dplyr::filter(pathrow == pathrow, dif_time < max_time_diff, band %in% class_bands) %>%
        ensurer::ensure_that(all(as.numeric(pathrow) > 200000 && as.numeric(pathrow < 300000)))

    if (nrow(brick_tb) == 0) {
        warning("No L8MOD brick found for scene ", pathrow, " and date ", start_date)
        return(NA)
    }

    # A brick should contain one year worth of images  of a single path/row of a single band
    time_line <- brick_tb %>% dplyr::pull(year) %>% unique() %>%
        ensurer::ensure_that(length(.) == 1) %>% as.Date() %>%
        seq(by = time_by, length.out = time_len)

    # get a sits coverage
    raster_cov <- sits::sits_coverage(
        service = "RASTER",
        name = cov_name,
        bands = brick_tb$band,
        scale_factor = scale_factor,
        missing_values = missing_values,
        minimum_values = minimum_values,
        maximum_values = maximum_values,
        timeline = time_line,
        files = brick_tb$path
    )

    # get samples
    samples.tb <- sits::sits_get_data(file = cpath, coverage = raster_cov)

    fp <- file.path(dirname(cpath),
                    paste0(basename(tools::file_path_sans_ext(cpath)), suffix,
                           ".Rdata"))

    save(samples.tb, file = fp)
    csv_paths <- character()
    csv_paths <- append(csv_paths, fp)
    return(csv_paths)
}


#' @title Get MODIS time series for validates sample points.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get validated PRODES samples (from a CSV file) and retrieve time
#' series for each one from WTSS server.
#'
#' @param cpath        A length-one character. Path to a CSV file.
#' @return             A length-one character. Path to the result file.
get_timeseries_modis <- function(cpath){
    wtss_coverage <- sits::sits_coverage(service = "WTSS-INPE", name = "MOD13Q1")
    samples.tb <- sits::sits_get_data(coverage = wtss_coverage, file = cpath)
    rd_file <- file.path(paste0(substr(cpath, 1, nchar(cpath) - 4), '_mod13q1.Rdata'))
    save(samples.tb, file = rd_file)
    return(rd_file)
}



#' @title Validate CSV files of samples.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Validate SITS sample CSV files.
#'
#' @param csv_paths    A character. Paths to a CSV file.
#' @return             A character.
validate_csv <- function(csv_paths){
    res <- lapply(csv_paths, function(x){
        r <- character()
        if (!file.exists(x)) {
            warning(paste0("File ", x, " does not exist"))
            return(paste0("File ", x, " does not exist"))
        }
        csv <- utils::read.csv(x)
        valid_cn <- c("id", "longitude", "latitude", "start_date", "end_date", "label")
        miss_f <- setdiff(valid_cn, colnames(csv))
        if (length(miss_f) > 0) {
            warning(paste0("There are missing fields in ", basename(x)))
            r <- paste0("There are missing fields in ", basename(x))
        }
        return(r)
    })
    unlist(res)
}
