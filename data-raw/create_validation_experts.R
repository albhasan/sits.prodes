# prepare the samples acquired by experts
library(dplyr)
library(sf)

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"

###############
# Rodrigo's shp
###############

# read shapefile
shp_path <- file.path(base_path, "data/samples/expert_validated_samples/samples") %>%
    list.files(pattern = "shp$", full.names = TRUE)
shp <- lapply(shp_path, function(x){
    sf::st_read(dsn = dirname(x),
                layer = tools::file_path_sans_ext(basename(x)), quiet = TRUE) %>%
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
    print(i)
    validation_experts <- rbind(validation_experts, shp_proc[[i]])
}

# save
devtools::use_data(validation_experts)

