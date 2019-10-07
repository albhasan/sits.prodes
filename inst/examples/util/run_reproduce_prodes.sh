#!/bin/bash
# Call the R script which accumulates deforestation in a PRODES-like fashion
if [ "$#" -gt 1 ]; then
    echo "Wrong parameters!"
    exit 1
fi
if [ "$1" = "dry-run" ]; then
    echo "WARNING: Running in dry-run mode"
    dry_run="--dry-run"
fi

parallel "$dry_run" Rscript reproduce_prodes.R --in_dir {1}_{2}/results_{3}/smooth_{4}_n10 --label_file {1}_{2}/results_dl/int_labels.csv --out_dir {1}_{2}/results_{3}/smooth_{4}_n10/prodes_reproduction  ::: /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes ::: 40 41 42 50 51 52 ::: dl rf svm vote ::: 3x3 5x5 7x7

