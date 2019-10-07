#!/bin/bash

# run the classification

./classify_brick_rf.R --train "train_42" --btype "l8_simple" --tiles "225063 226064 233067" --bands "vegetation substrate dark" --years "2013 2014 2015 2016" --cores "16" --ram "8" --debug "DEBUG" --log "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_42/rep_prodes_rf_209.log"

