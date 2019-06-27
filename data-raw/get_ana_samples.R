#!/usr/bin/env Rscript

# GET ANNA RORATO'S SAMPLES OF THE AMAZON FOREST.

library(dplyr)

file_url <- "https://mod13q1-classifications.s3.amazonaws.com/mod13q1/br/amazonia/DL_4Bands_11Classes/scripts/model.rds"
dest_file <- file.path("..", "data", basename(file_url))

if (!file.exists(dest_file)) {
    warning("Origin file not found. Trying to download it again...")
    download.file(file_url, destfile = dest_file)
    if (!file.exists(dest_file)) {
        Warning("File not found. Aborting...")
        quit(save = "no")
    }
}

expert_samples <- dest_file %>% readRDS() %>%
    environment() %>%
    as.list() %>%
    magrittr::extract("data.tb") %>%
    .[[1]]

usethis::use_data(expert_samples, overwrite = TRUE)

