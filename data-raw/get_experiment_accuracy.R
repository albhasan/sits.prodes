#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(dplyr))

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
base_path <- "/home/alber/Documents/data/experiments/prodes_reproduction"
stopifnot(dir.exists(base_path))

# collect accuracy data
accuracy_ls <- list()
accuracy_files <- base_path %>% 
    list.files(pattern = "accuracy.rds", recursive = TRUE, full.names = TRUE)
for (f in accuracy_files) {
    accuracy_ls <- append(accuracy_ls, readRDS(f))
}

fname <- file.path(getwd(), "inst", "extdata", "accuracy_ls.Rdata")
if (file.exists(fname)) {
    file.remove(fname)
}
print(sprintf("Saving new accuracy file to: %s", fname))
save(accuracy_ls, file = fname)
print("Finished!")

