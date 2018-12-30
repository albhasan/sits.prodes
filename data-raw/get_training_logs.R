#!/usr/bin/env Rscript

# Copy the logs of training Deep Learning models to the package

suppressPackageStartupMessages(library(dplyr))

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
log_pattern <- "^train_[0-9]{2}_esensing"

old_logs <- "./inst/extdata" %>%
    list.files(pattern = log_pattern, full.names = TRUE, recursive = FALSE)
print(sprintf("Removing old logs: %s", old_logs))
old_logs %>% file.remove()

new_logs_from <- "/home/alber/Documents/data/experiments/prodes_reproduction/02_train_model" %>%
    list.files(pattern = log_pattern, full.names = TRUE, recursive = TRUE)
log_files <- file.path("./inst/extdata", basename(new_logs_from))
print(sprintf("Copying new logs: %s", log_files))
new_logs_from %>% file.copy(to = log_files)

print("Saving file names to log_files.Rdata...")
file.remove("./inst/extdata/log_files.Rdata")
log_files <- basename(log_files)
save(log_files, file = "./inst/extdata/log_files.Rdata")

print("Done!")

