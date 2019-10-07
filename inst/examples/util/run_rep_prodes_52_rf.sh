#!/bin/bash

# run the classification

./classify_brick_rf.R --train "train_52" --btype "l8_mask_cloud" --tiles "225063 226064 233067" --bands "vegetation substrate dark" --years "2013 2014 2015 2016" --cores "16" --ram "8" --debug "DEBUG" --log "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_52/rep_prodes_rf_209.log"

