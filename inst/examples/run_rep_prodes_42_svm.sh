#!/bin/bash

# run the classification rep_prodes_42 using SVM

./classify_brick_svm.R --train "train_42" --btype "l8_simple" --tiles "225063 226064 233067" --bands "vegetation substrate dark" --years "2013 2014 2015 2016" --cores "24" --ram "96" --debug "DEBUG" --log "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_42/rep_prodes_svm_208.log"


