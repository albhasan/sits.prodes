#!/usr/bin/Rscript
# prepare the samples acquired by experts
library(dplyr)
library(sf)
library(ensurer)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

###############
# Rodrigo's shp
###############
prodes_lbl <- tibble::tribble(
             ~label_pd_pt,     ~label_pd,  ~id_pd,
        #--------------#-----------------#--------
        "DESMATAMENTO",  "deforestation", 1L,
        "RESIDUO",       "deforestation", 1L,
        "FLORESTA",      "forest",        2L,
        "NAO_FLORESTA",  "no forest",     3L,
        "NAO_FLORESTA2", "no forest",     3L,
        "HIDROGRAFIA",   "water",         4L
    )

# read shapefile
shp_path <- file.path(base_path, "data/samples/expert_validated_samples/samples") %>%
    list.files(pattern = "shp$", full.names = TRUE)
shp <- lapply(shp_path, function(x){
    sf::st_read(dsn = dirname(x),
                layer = tools::file_path_sans_ext(basename(x)), quiet = TRUE,
                stringsAsFactors = FALSE) %>%
        dplyr::mutate(tile = stringr::str_extract(basename(x), pattern = "[0-9]{6}")) %>%
        sf::st_transform(4326) %>%
        return()
})

# make sure the shps have the same fields
shp_fields <- shp %>% sapply(colnames) %>% unlist() %>% unique() 
shp_proc <- shp %>% lapply(function(x, shp_fields){
                          missing_names <- shp_fields[!(shp_fields %in% colnames(x))] 
                          for(mn in missing_names){
                              x <- x %>% dplyr::mutate(!!mn := NA)   
                          }
                          fn_sort <- sort(colnames(x)[!(colnames(x) %in% "geometry")])
                          x %>% dplyr::select_(.dots = fn_sort) %>%
                              return()
                        }, shp_fields)

# merge shp into a single sf object
validation_experts <- shp_proc[[1]]
for (i in 2:length(shp_proc)) {
    validation_experts <- rbind(validation_experts, shp_proc[[i]])
}

# check the labels
recode_ls <- list(
    "Cloud Oover"   = "cloud",
    "Cloud Cover"   = "cloud",
    "Coud Cover"    = "cloud",
    "Forest"        = "forest",
    "Deforestation" = "deforestation",
    "Degadation"    = "degradation",
    "Degradation"   = "degradation",
    "Non Forest"    = "no forest"
)

validation_experts <- validation_experts %>%
    dplyr::mutate(
        Label2013 = dplyr::recode(.$Label2013, !!!recode_ls),
        Label2014 = dplyr::recode(.$Label2014, !!!recode_ls),
        Label2015 = dplyr::recode(.$Label2015, !!!recode_ls),
        Label2016 = dplyr::recode(.$Label2016, !!!recode_ls),
        Label2017 = dplyr::recode(.$Label2017, !!!recode_ls)
    ) 

validation_experts %>%
    dplyr::select(Label2013, Label2014, Label2015, Label2016, Label2017) %>%
    sf::st_set_geometry(NULL) %>%
    unlist() %>%
    unique() %>%
    .[!is.na(.)] %>%
	    ensurer::ensure_that(all(. %in% c("cloud", "degradation", prodes_lbl$label_pd)), err_desc = "Unknow labels found!") 

# save
devtools::use_data(validation_experts, overwrite = TRUE)

#    %>% dplyr::select(Label2013, Label2014, Label2015, Label2016, Label2017) %>% sf::st_set_geometry(NULL) %>% unlist() %>% unique() %>% print()
 
