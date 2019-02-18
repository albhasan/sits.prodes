#!/bin/bash

./classify_brick_svm.R --train "train_51" --btype "l8_mask_cloud" --tiles "225063 226064 233067" --bands "ndvi nir red swir2 vegetation substrate dark" --years "2013 2014 2015 2016" --cores "24" --ram "96" --debug "DEBUG" --log "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_51/rep_prodes_svn_210.log"

