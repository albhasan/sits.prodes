#' @title Parse the log file of a training.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description  Parse the log file of a training.
#'
#' @param log_path  A length-one character. Path to the log file of a training.
#' @return          A list of two: A tibble and a list of tibble
#' @export
parse_training_log <- function(log_path){
    # TODO: remove
    #log_path <- "/home/alber/Documents/data/experiments/prodes_reproduction/02_train_model/train_20/train_20_esensing-006_.R.log"
    #setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
    # - - - - 
    # read the log
    stopifnot(file.exists(log_path))
    con <- file(log_path, open = "r")
    txt <- readLines(con) 
    close(con)
    txt_ls <- txt %>% splitAt(pos = c(1, which(txt %in% "EXPERIMENT"), length(txt)))

    # parse the file
    header <- txt_ls[[1]] %>% 
                  stringr::str_sub(21) %>%
                  stringr::str_trim() %>%
                  stringr::str_subset(':') %>%
                  dplyr::as_tibble() %>% 
                  tidyr::separate(col = value, into = c("key", "value"), sep = ":")
    experiments <- lapply(txt_ls[2:length(txt_ls)], function(x){
            x_copy <- x %>% .[!stringr::str_detect(., "Error")]
            # join splitted vectors
            guide <- cumsum(as.integer(stringr::str_detect(x_copy, "[(]")) - as.integer(stringr::str_detect(x_copy, "[)]")))
            x <- character(0)
            x_tmp <- character(0)
            for(i in 1:length(x_copy)){
                if (guide[i] == 0) {
                    x <- append(x, paste(x_tmp, x_copy[i]))
                    x_tmp <- character(0)
                }else if (guide[i] == 1) {
                    x_tmp <- paste(x_tmp, x_copy[i])
                }else{
                    stop("Unknown state")
                }
            }
            # 
            x %>%  stringr::str_subset('=') %>% 
                dplyr::as_tibble() %>% 
                tidyr::separate(col = value, into = c("key", "value"), sep = "=") %>%
                lapply(stringr::str_trim) %>% dplyr::as_tibble()})
    return(list(header = header, experiments = experiments))
}

