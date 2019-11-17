#!/usr/bin/Rscript

print("Creating a table of descriptions of each brick...")

brick_description <- tibble::tribble(
    ~Classification_type, ~Obs_year, ~Description,
    #--------------------|----------|--------------------|
    "interpolated",       0,         "TODO",
    "starfm",             0,         "TODO",
    "simple",             0,         "TODO",
    "mask_clouds",        0,         "TODO"
)

setwd("~/Documents/ghProjects/sits.prodes")
usethis::use_data(brick_description, overwrite = TRUE)

