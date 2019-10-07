#!/bin/bash

# parallel --dry-run -j 4 Rscript ensemble_vote.R --experiment {1} --out_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_vote/{2} --smooth_dir {2} --cores 6 ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 rep_prodes_50 rep_prodes_51 rep_prodes_52 ::: smooth_3x3_n10 smooth_5x5_n10 smooth_7x7_n10 smooth_9x9_n10 smooth_11x11_n10 ""


# delete this line
parallel -j 4 Rscript ensemble_vote.R --experiment {1} --out_dir /home/alber/Documents/data/experiments/prodes_reproduction/03_classify/{1}/results_vote/ --smooth_dir \"\"  --cores 6 ::: rep_prodes_40 rep_prodes_41 rep_prodes_42 rep_prodes_50 rep_prodes_51 rep_prodes_52 

echo "Finished!"

