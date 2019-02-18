#!/bin/bash
parallel -j 8 Rscript validate_map.R --experiment {1} --smooth_dir {2} ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: smooth_3x3_n10 smooth_5x5_n10 smooth_7x7_n10

