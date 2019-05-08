#!/usr/bin/Rscript
# prepare the samples acquired by experts
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ensurer))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
setwd(base_path)

###############
# Rodrigo's shp
###############
data("prodes_labels", package = "sits.prodes")
prodes_lbl <- prodes_labels

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
                          x %>% dplyr::select(.dots = !!!fn_sort) %>%
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

validation_experts <- validation_experts %>%
    dplyr::mutate(to_remove = all(is.na(Label2013), is.na(Label2014),
                  is.na(Label2015), is.na(Label2016), is.na(Label2017))) %>%
    tidyr::drop_na(to_remove) %>%
    dplyr::select(-to_remove)

# save
setwd(file.path(base_path, "Rpackage", "sits.prodes"))
usethis::use_data(validation_experts, overwrite = TRUE) 

