#!/usr/bin/env Rscript

print("Parsing the logs of the classifications' trainings...")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sits.prodes))

base_path <- "~/Documents/data/experiments/prodes_reproduction"
log_pattern <- "^train_[0-9]{2}_esensing"

parse_header <- function(x){
    x[["header"]] %>%
        dplyr::mutate(key = stringr::str_replace_all(key, c("\\(" = "", "\\)" = "", " " = "_"))) %>%
        tidyr::spread(key, value) %>%
        return()
}

parse_experiment <- function(x){
    x[["experiments"]] %>%
        lapply(function(y){
            y %>% dplyr::filter(value != "<environment>") %>%
                tidyr::spread(key, value) %>%
                return()
        }) %>%
        dplyr::bind_rows() %>%
        return()
}

training_logs <- base_path %>%
    file.path("02_train_model") %>%
    list.files(pattern = log_pattern, full.names = TRUE, recursive = TRUE) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::rename(path = value) %>%
    dplyr::mutate(experiment = basename(path)) %>%
    dplyr::select(experiment, path) %>%
    dplyr::mutate(parsed_log = purrr::map(.$path, parse_training_log)) %>%
    dplyr::mutate(setup = purrr::map(.$parsed_log, parse_header),
                  trains = purrr::map(.$parsed_log, parse_experiment)) %>%
    dplyr::select(-parsed_log)

setwd("~/Documents/ghProjects/sits.prodes")
usethis::use_data(training_logs, overwrite = TRUE)
