#!/bin/bash
#parallel -j 8 Rscript validate_map.R --experiment {1} --algorithm {2} --smooth_dir {3} ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: dl svm rf  ::: smooth_3x3_n10 smooth_5x5_n10 smooth_7x7_n10
parallel -j 8 Rscript validate_map.R --experiment {1} --algorithm {2} --smooth_dir {3} ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: svm rf  ::: smooth_3x3_n10 smooth_5x5_n10 smooth_7x7_n10

echo "Finished"

