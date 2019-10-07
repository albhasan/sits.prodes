#!/bin/bash

# Download Harmonized Landsat8-Sentinel2 images of the Bolivian-Brazilian Amazon.
# NOTE: It depends on the HLS download script available at 
# - https://hls.gsfc.nasa.gov/wp-content/themes/landsat-2012/bash_file/download.hls.sh
# - https://hls.gsfc.nasa.gov/documents/ 

parallel --delay 300 -j 1 /home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes/inst/examples/hls_scripts/./download.hls.sh -t {1} -y {2} /home/alber/Documents/data/experiments/prodes_reproduction/data/raster/harmonized_landsat_sentinel2/data/hls ::: 19LFJ 19LGK 19LGJ 19LDJ 19LDH 19LDL 19LDK 19LEJ 19LEH 19LEL 19LEK 19LFK 21LWF 21LWG 21LWH 21LXF 21LXG 21LXH 21LYF 21LYG 21LYH ::: 2013 2014 2015 2016 2017 2018 2019

echo "Done!"

