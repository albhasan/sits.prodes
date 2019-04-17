#!/bin/bash
# parallel -j 8 --no-notice Rscript validate_map.R  --in_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_vote/{2} --label_file /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_dl/int_labels.csv --out_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_vote/{2}/validation ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: smooth_3x3_n10 smooth_5x5_n10 smooth_7x7_n10

parallel -j 8 --no-notice Rscript validate_map.R --in_dir     {1}/rep_prodes_{2}/results_{3}/smooth_{4}/prodes_reproduction --label_file {1}/rep_prodes_{2}/results_dl/int_labels.csv --out_dir    {1}/rep_prodes_{2}/results_{3}/smooth_{4}/prodes_reproduction/validation ::: /home/alber/Documents/data/experiments/prodes_reproduction/03_classify ::: 40 41 42 50 51 52 ::: dl rf svm vote ::: 3x3_n10 5x5_n10 7x7_n10 

echo "Finished"

