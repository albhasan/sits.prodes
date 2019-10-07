# export HLS samples from shp to sits_tibble

# TODO : Remove. Use get_timeseries.R instead.
stop("Use get_timeseries.R instead.")

require(dplyr)
require(sf)

in_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples/hls"
brick_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_hls_raw"

# Get file of samples.
shp_tb <- in_dir %>%
    list.files(pattern = "shp$", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = "value") %>%
    dplyr::mutate(file_name = basename(file_path)) %>%
    tidyr::separate(file_name, into = c("v1", "tile", "pyear"), sep = '_') %>%
    dplyr::mutate(pyear = stringr::str_sub(pyear, 2,5), 
                  sf = purrr::map(file_path, sf::st_read, quiet = TRUE, 
                                  stringsAsFactors = FALSE)) 

# Get bricks' metadata.



get_coord <- function(x, coord){
    if (coord == "x"){
        idx <- 1
    } else if (coord == "y"){
        idx = 2
    }else{
        return(NA)
    }
    return(sf::st_coordinates(x)[,idx])
}

samples_tb <- do.call(rbind, dplyr::pull(shp_tb, sf)) %>% 
    dplyr::mutate(longitude = purrr::map_dbl(geometry, get_coord, coord = "x"), 
                  latitude  = purrr::map_dbl(geometry, get_coord, coord = "y"), 
                  time_series = NA, 
                  coverage    = "brick_hls_raw") %>%
    sf::st_set_geometry(value = NULL) %>%
    tibble::as_tibble() %>%
    dplyr::select(longitude, latitude, start_date, end_date, 
                  label, coverage, time_series) %>%
    dplyr::mutate(label = dplyr::recode(label,
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
                                        "DESMATAMENTO" = "deforestation",
                                        "flood" = "flood")) %>%
    

    
