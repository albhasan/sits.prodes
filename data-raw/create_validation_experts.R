#!/usr/bin/Rscript

# PREPARE THE SAMPLES VALIDATED BY EXPERTS
# VALIDATE CLASSIFICATION RESULTS USING THE SAMPLES PROVIDED BY THE EXPERTS

suppressMessages(library(caret))
suppressMessages(library(dplyr))
suppressMessages(library(ensurer))
suppressMessages(library(purrr))
suppressMessages(library(raster))
suppressMessages(library(sf))
suppressMessages(library(sits.prodes))
suppressMessages(library(stringr))
suppressMessages(library(tibble))
suppressMessages(library(tidyr))

base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
setwd(base_path)

data("prodes_labels", package = "sits.prodes")

#------------------------------------------------------------------------------
# prepare the shapefile
#------------------------------------------------------------------------------

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
	    ensurer::ensure_that(all(. %in% c("cloud", "degradation", prodes_labels$label_pd)), err_desc = "Unknow labels found!") 

validation_experts <- validation_experts %>%
    dplyr::mutate(to_remove = all(is.na(Label2013), is.na(Label2014),
                  is.na(Label2015), is.na(Label2016), is.na(Label2017))) %>%
    tidyr::drop_na(to_remove) %>%
    dplyr::select(-to_remove)


#------------------------------------------------------------------------------
# do the validation
#------------------------------------------------------------------------------

experiments <- c("rep_prodes_40", "rep_prodes_41", "rep_prodes_42", "rep_prodes_50", "rep_prodes_51", "rep_prodes_52" )
results     <- c("results_dl", "results_rf", "results_svm", "results_vote")
smooths     <- c("smooth_3x3_n10", "smooth_5x5_n10", "smooth_7x7_n10")

# get a tibble of raster files
expert_validation <- expand.grid(experiment = experiments, algorithm = results, smooth = smooths, stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(file_path = purrr::pmap(., function(experiment, algorithm, smooth){
        file.path(base_path, "03_classify", experiment, algorithm, smooth) %>%
            list.files(pattern = "*_masked.tif$", recursive = TRUE, full.names = TRUE)
    })) %>%
    tidyr::unnest() %>%
    dplyr::mutate(fname = basename(file_path)) %>%
    dplyr::mutate(scene = purrr::map_chr(.$fname, function(x){
                        x %>% stringr::str_extract(pattern = '_[]0-9]{6}_') %>%
                            stringr::str_sub(start = 2, end = -2)
                    }), 
                  pyear = purrr::map_chr(.$fname, function(x){
                        x %>% stringr::str_extract_all(pattern = '_[]0-9]{4}_') %>%
                            unlist() %>% tail(n = 1) %>%
                            stringr::str_sub(start = 2, end = -2)
                    }))

# get the values
expert_validation$match_ref_res <- expert_validation%>% dplyr::select(file_path, scene) %>%
    purrr::pmap(function(file_path, scene){
        # build recode table
        lab_tb <- prodes_labels %>% dplyr::select(label_pd, id_pd) %>%
            dplyr::distinct() %>% dplyr::arrange(id_pd)
        lab <- lab_tb %>% dplyr::pull(label_pd) %>% as.list()
        names(lab) <- lab_tb %>% dplyr::pull(id_pd)
        # 
        r <- file_path %>% raster::raster()
        pts <- validation_experts %>% dplyr::filter(tile == scene) %>%
            sf::st_transform(crs = r@crs@projargs)
        pts$raster_val <- r %>% raster::extract(y = as(pts, "Spatial"))
        pts %>% dplyr::mutate(label_res = dplyr::recode(.$raster_val, !!!lab)) %>%
            return()
    })

# compute confusion matrixes
expert_validation$confusion_matrix <- lapply(expert_validation$match_ref_res, function(x){
        label_tb <- x %>% sf::st_set_geometry(NULL) %>%
            tibble::as_tibble() %>%
            dplyr::select(tidyselect::starts_with("Label"))
        ys <- label_tb %>%
            dplyr::select(tidyselect::starts_with("Label", ignore.case = FALSE)) %>%
            colnames() %>% stringr::str_extract("[0-9]{4}") %>% sort()
        res <- list()
        for (y in ys) {
            con_mat <- NA
            cname <- paste("Label", y, sep = '')
            cm_dat <- label_tb %>%
                dplyr::select(c(cname, "label_res")) %>%
                tidyr::drop_na() %>%
                dplyr::filter(.data[[cname]] %in% unique(dplyr::pull(prodes_labels, label_pd)))
            # compute confusion matrix
            if(nrow(cm_dat) > 0) {
                flevels <- prodes_labels %>% dplyr::pull(label_pd) %>% unlist() %>% unique()
                data_f <- factor(label_tb[[cname]], flevels)
                ref_f  <- factor(label_tb[["label_res"]], flevels)
                con_mat <- caret::confusionMatrix(data = data_f,
                                                  reference = ref_f)
            }
            res[[y]] <- con_mat
        }
        return(res)
    }
)

# save
setwd(file.path(base_path, "Rpackage", "sits.prodes"))
usethis::use_data(expert_validation, overwrite = TRUE)

