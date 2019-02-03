#!/bin/bash

# run the classification rep_prodes_40

./classify_brick.R --train "train_40" --model "train_40_model_17" --btype "l8mod_simple" --tiles "225063 226064 233067" --bands "ndvi nir red blue swir2" --years "2013 2014 2015 2016" --cores "24" --ram "96" --debug "DEBUG" --log "/home/alber/Documents/data/experiments/prodes_reproduction/03_classify/rep_prodes_40/rep_prodes_207.log"

