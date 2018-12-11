# FUNCTIONS FOR SAMPLE PROCESSING
# alber sanchez alber.ipia@inpe.br
# Last update 2018-09-08
# TODO:
# NOTES:


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
    id_col <- csv %>% dplyr::select(starts_with('tmp_id'))
    if (nrow(id_col) > 0 && ncol(id_col) > 0) {
        id_col <- as.integer(as.vector(unlist(id_col[, ncol(id_col)]))) # take the last tmp_id used
        if (length(id_col) != length(unique(id_col)))
            id_col <- 1:nrow(csv) # recompute id if it is not valid
    }else{
        id_col <- 1:nrow(csv)
    }
    csv <- csv %>% dplyr::mutate(id = id_col) %>%
        dplyr::select(-starts_with("tmp_id")) %>%
        dplyr::select(id, everything())



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
        write.csv(csv_d, file = fname, quote = FALSE, row.names = FALSE)
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
#' @param suffix       A length-one character. A suffix to append to the filename of the results
#' @param max_time_diff A length-one numeric. The maximum difference (in days) between the fist day of the bricks and the first day of the CSV sample file.
#' @return             A vector of paths to Rdata files
get_timeseries <- function(cpath, path_bricks, brick_prefix, class_bands,
                           suffix = "", max_time_diff = 30){

    # get time series from bricks
    pathrow <- cpath %>% basename() %>% stringr::str_extract("_[0-9]{3}_[0-9]{3}_") %>%
        strsplit(split = '_', fixed = TRUE) %>% unlist() %>%
        paste(collapse = '')

    start_date <- cpath %>% basename() %>% stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}")
    if (class(try(as.Date(start_date))) == "try-error" || is.na(try(as.Date(start_date))))
        stop("Invalid start_date in file name ", cpath)

    # build a data.frame of bricks' metadata
    brick_tb <- path_bricks %>% list.files(
        pattern = paste0(brick_prefix, pathrow, '_*'),
        full.names =  TRUE) %>% dplyr::as_tibble() %>%
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

    # A brick should contain one year (23 images) of a single path/row of a single band
    time_line <- brick_tb %>% dplyr::pull(year) %>% unique() %>%
        ensurer::ensure_that(length(.) == 1) %>% as.Date() %>%
        seq(by = 16, length.out = 23)

    # get a sits coverage
    raster_cov <- sits::sits_coverage(
        service = "RASTER",
        name = "Sinop-crop",
        timeline = time_line,
        bands = brick_tb$band,
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
    samples.tb <- sits::sits_getdata(coverage = wtss_coverage, file = cpath)
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
        csv <- read.csv(x)
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
