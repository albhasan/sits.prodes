#!/bin/bash
parallel -j 8 Rscript validate_map.R  --in_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_vote/{2} --label_file /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_dl/int_labels.csv --out_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_vote/{2}/validation ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: smooth_3x3_n10 smooth_5x5_n10 smooth_7x7_n10
echo "Finished"

