#!/bin/bash

parallel -j 4 Rscript smooth_results.R --experiment {1} --win_size {2} --noise 10 --overwrite TRUE ::: rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: 3 5 7 

