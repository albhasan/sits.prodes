# prepare sen2agri samples

library(dplyr)

#---- configuration ----
set.seed(666)
base_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction"
sentinel_shp <- base_dir %>%
    file.path() %>%
    ensurer::ensure_that(dir.exists(.))
sentinel_tiles <- c("19LGK")
out_dir <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/vector/sen2agri/samples"
prodes_years <- 2017:2019

#----- utilitary functions ----

# Helper for reading a PRODES shapefile and map its classes to Sen2agri.
read_prodes <- function(x){

    # Sen2agri legend.
    legend_tb <- "/home/alber/Documents/ghProjects/sits.prodes/inst/extdata/esa_cci_lc_legend.csv" %>%
        readr::read_csv() %>%
        dplyr::mutate(label = ifelse(is.na(Global_label), Regional_label, 
                                     Global_label),
                      value = ifelse(is.na(Global_value), Regional_value, 
                                     Global_value)) %>%
        dplyr::select(sen2agri_label = label, sen2agri_id = value) %>%
        dplyr::mutate(CROP = as.integer(stringr::str_detect(sen2agri_label, 
                                                            "Cropland")),
                      LC = stringr::str_sub(sen2agri_label, 1, 70),
                      CODE = as.integer(sen2agri_id),
                      IRRIGATION = 0L) %>%
        ensurer::ensure_that(!any(sapply(., is.na)), 
                             err_desc = "Inconsistent legend.")

    # Map PRODES' classes to Sen2agri. 
    data(prodes_labels, package = "sits.prodes")
    prodes_labels <- prodes_labels %>%
        dplyr::mutate(sen2agri_id = c(40, 40, 50, 200, 200, 210)) %>%
        ensurer::ensure_that(!any(is.na(.$sen2agri_id)), 
                             err_desc = "Missing sen2agri legend codes.") %>%
        dplyr::left_join(legend_tb, by = "sen2agri_id")

    prodes <- x %>%
        sf::read_sf(quiet = TRUE) %>%
        sf::st_transform(crs = 4326) %>%
        dplyr::left_join(prodes_labels, by = c("mainclass" = "label_pd_pt")) %>%
        return()
}

# Helper for getting the extent of a shapefile.
get_bbox <- function(x){
    x %>%
        sf::st_bbox() %>%
        sf::st_as_sfc() %>%
        return()
}

# Helper for finding the prodes shps that intesect Sentinel2' tiles.
intersect_prodes <- function(x, prodes_tb){
        # Intersect x with PRODES shapefiles' bounding boxes.
	do_bbox_intersect <- prodes_tb %>%
	    dplyr::pull(bounding_box) %>%
	    purrr::map(sf::st_intersects, y = x) %>%
	    purrr::map(as.matrix) %>%
	    unlist()
        # List of sf objects that intersect with x.
	intersect_tb <- prodes_tb %>%
	    dplyr::filter(do_bbox_intersect) %>%
            dplyr::pull(sf)
        # Merge prodes shps and intersect again with x.
        prodes_merged_tb <- do.call(rbind, intersect_tb)
	do_intersect <-  prodes_merged_tb %>%
            dplyr::pull(geometry) %>%
            purrr::map(sf::st_intersects, y = x) %>%
	    purrr::map(as.matrix) %>%
            unlist()
        # Only return the PRODES features that intersect x.
	prodes_merged_tb %>%
            dplyr::filter(do_intersect) %>%
            return()
}

#---- script ----

# List the PRODES shapefiles and their metadata.
prodes_tb <- base_dir %>%
    file.path("data/vector/prodes/tiled") %>%
    list.files(pattern = "*[.]shp$", full.names = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = basename(tools::file_path_sans_ext(file_path))) %>%
    tidyr::separate(file_name, into = c(NA, NA, NA, "path", "row"), sep = '_') %>%
    dplyr::mutate(sf = purrr::map(.$file_path, read_prodes),
                  bounding_box = purrr::map(sf, get_bbox)) %>%
    dplyr::select(-file_path)

# PRODES's spatial reference system.
prodes_crs <- prodes_tb %>%
    dplyr::pull(sf) %>%
    dplyr::first() %>%
    sf::st_crs()

# List the SENTINEL shapefiles and their overlapping PRODES features.
sentinel_tb <- sentinel_tiles %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(tile = value) %>%
    dplyr::mutate(sf = purrr::map(tile, function(x, crs = prodes_crs){
        base_dir %>%
            file.path("data/vector/sentinel2/sentinel_2_index_shapefile.shp") %>%
            sf::read_sf() %>%
            sf::st_transform(crs = crs) %>%
            dplyr::filter(Name %in% x) %>%
            return()
    })) %>%
    dplyr::mutate(bounding_box = purrr::map(sf, get_bbox),
                  prodes = purrr::map(bounding_box, intersect_prodes, prodes_tb),
                  out_shp = file.path(out_dir, stringr::str_c("BR_", tile , 
                                                              "_LC_II_YEARMONTH.shp"))) %>%
    # Format the PRODES data to fit the Sen2agri's sample schema.
    dplyr::mutate(samples_sf = purrr::pmap(dplyr::select(., prodes, bounding_box, out_shp), 
                                           function(prodes, bounding_box, out_shp, prodes_years){
        #is_within_bb <- function(x, bounding_box){
        #    sf::st_within(x, bounding_box, sparse = FALSE)[1,]
        #}
        bb_wgs84 <- bounding_box %>%
            sf::st_transform(crs = 4326) %>%
            sf::st_buffer(-0.1)
        for(pyear in prodes_years){
            s2a_shp <- stringr::str_replace(out_shp, pattern = "_YEARMONTH", 
                                            paste0('_', pyear, "11"))
            prodes %>%
                dplyr::mutate(ID = dplyr::row_number()) %>%
                dplyr::filter(class_name %in% c("FLORESTA", "NAO_FLORESTA", 
                                                "NAO_FLORESTA2", 
                                                paste0('d', pyear)),
                              areameters > 100000) %>%
                #dplyr::filter(is_within_bb(., bb_wgs84)) %>%
                sf::st_intersection(y = bb_wgs84) %>%
                dplyr::select(ID, CROP, LC, CODE, IRRIGATION) %>%
                dplyr::group_by(CODE) %>%
                dplyr::sample_n(20) %>%
                dplyr::ungroup() %>%
                sf::st_write(s2a_shp)
        }
    }, prodes_years))

