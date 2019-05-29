#!/bin/bash
parallel -j 4 Rscript smooth_results.R --in_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_{1}/results_dl   --win_size {2} --noise 10 --overwrite TRUE ::: 40 41 42 50 51 52 ::: 3 5 7 9 11
parallel -j 4 Rscript smooth_results.R --in_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_{1}/results_rf   --win_size {2} --noise 10 --overwrite TRUE ::: 40 41 42 50 51 52 ::: 3 5 7 9 11
parallel -j 4 Rscript smooth_results.R --in_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_{1}/results_svm  --win_size {2} --noise 10 --overwrite TRUE ::: 40 41 42 50 51 52 ::: 3 5 7 9 11
# NOTE: model ensembling (vote) runs using run_ensemble_vote.sh

