#!/usr/bin/Rscript

# SET THE PRODES LABELS AND THEIR MAPPINGS BETWEEN PORTUGUESE AND ENGLISH
library(tibble)
library(devtools)

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

devtools::use_data(prodes_labels, overwrite = TRUE)

