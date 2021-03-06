#!/bin/bash

# run the classification rep_prodes_41

./classify_brick.R --train "train_41" --model "train_41_model_2" --btype "l8mod_simple" --tiles "225063 226064 233067" --bands "ndvi nir red swir2 vegetation substrate dark" --years "2013 2014 2015 2016" --cores "24" --ram "96" --debug "DEBUG" --log "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_41/rep_prodes_207.log"

