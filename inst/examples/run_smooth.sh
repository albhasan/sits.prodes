#!/bin/bash

# parallel -j 4 Rscript smooth_results.R --experiment {1} --win_size {2} --noise 10 --overwrite TRUE ::: rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: 3 5 7 
# parallel -j 4 Rscript smooth_results.R --experiment {1} --win_size {2} --noise 10 --overwrite TRUE ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 ::: 3 5 7 
# parallel -j 4 Rscript smooth_results.R --in_dir /net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_{1}/results_svm --win_size {2} --noise 10 --overwrite TRUE ::: 40 41 42 ::: 3 5 7 
parallel -j 4 Rscript smooth_results.R --in_dir /net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_{1}/results_svm --win_size {2} --noise 10 --overwrite TRUE ::: 50 51 52 ::: 3 5 7 
parallel -j 4 Rscript smooth_results.R --in_dir /net/150.163.2.206/disks/d6/shared/alber/prodes_reproduction/03_classify/rep_prodes_{1}/results_rf --win_size {2} --noise 10 --overwrite TRUE ::: 40 41 42 50 51 52 ::: 3 5 7 

