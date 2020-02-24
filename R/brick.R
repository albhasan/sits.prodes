# functions about bricks

#' @title Get metadata from a Brick filename
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get metadata from a Brick's file name.
#'
#' @param brick_paths A character. Path to brick files.
#' @return            A character of path, pathrow, start_date, band, year (NA for MODIS).
#' @export
get_brick_md <- function(brick_paths){
    .Deprecated("get_brick_metadata")
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


#' @title Create a tibble with metadata of the bricks in a directory.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Create a tibble with metadata of the bricks in a directory.
#'
#' @param brick_dir A length-one character. Path to a directory of bricks.
#' @return          A tibble.
#' @export
get_brick_metadata <- function(brick_dir){
    brick_tb <- brick_dir %>%
        list.files(pattern = "*.tif$", full.names = TRUE) %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(file_name = basename(file_path)) %>%
        tidyr::separate(file_name, sep = '_',
                        into = c("type", "tile", "start_date", "band", NA, NA)) %>%
        dplyr::mutate(start_date = as.Date(start_date),
                      n_bands = get_number_of_bands(file_path)) %>%
        return()
}

