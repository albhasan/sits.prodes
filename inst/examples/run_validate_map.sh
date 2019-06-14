#!/bin/bash

# Compute accuraries for smoothed classifications.
parallel -j 8 --no-notice Rscript validate_map.R --in_dir {1}/rep_prodes_{2}/results_{3}/smooth_{4} --label_file {1}/rep_prodes_{2}/results_dl/int_labels.csv --out_dir {1}/rep_prodes_{2}/results_{3}/smooth_{4}/validation ::: /home/alber/Documents/data/experiments/prodes_reproduction/03_classify ::: 40 41 42 50 51 52 ::: dl rf svm vote ::: 3x3_n10 5x5_n10 7x7_n10 9x9_n10 11x11_n10

# Compute accuraries for non-smoothed classifications.
parallel -j 8 --no-notice Rscript validate_map.R --in_dir {1}/rep_prodes_{2}/results_{3} --label_file {1}/rep_prodes_{2}/results_dl/int_labels.csv --out_dir {1}/rep_prodes_{2}/results_{3}/validation ::: /home/alber/Documents/data/experiments/prodes_reproduction/03_classify ::: 40 41 42 50 51 52 ::: dl rf svm

echo "Finished"

