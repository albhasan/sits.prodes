#!/usr/bin/Rscript

print("Setting the PRODES' mapping between portuguese and english labels...")

suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(devtools))

base_path <- "~/Documents/ghProjects/sits.prodes"
setwd(base_path)

prodes_labels <- tibble::tribble(
             ~label_pd_pt,     ~label_pd,  ~id_pd,
        #--------------#-----------------#--------
        "DESMATAMENTO",  "deforestation", 1L,
        "RESIDUO",       "deforestation", 1L,
        "FLORESTA",      "forest",        2L,
        "NAO_FLORESTA",  "no forest",     3L,
        "NAO_FLORESTA2", "no forest",     3L,
        "HIDROGRAFIA",   "water",         4L
    )

usethis::use_data(prodes_labels, overwrite = TRUE)

