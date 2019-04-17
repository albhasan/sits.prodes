#!/bin/bash
###############################################################################
# SPLIT MAPBIOMAS AMAZONIA DATA BY LANDSAT SCENE 
#------------------------------------------------------------------------------
# Last update 2019-04-17
###############################################################################
BASE_PATH="/home/alber/Documents/data/experiments/prodes_reproduction"
WD="$BASE_PATH"/data/raster/mapbiomas/amazonia
PRODES_TAR="$BASE_PATH"/data/vector/prodes/prodes_2017.tar.gz
PRODES_SHP="$WD"/prodes_2017/PDigital2017_AMZ_pol.shp

mkdir "$WD"/mapbiomas_tiled

# extract files
tar -xzf $PRODES_TAR --directory "$WD"

# cut TIFs using a shapefile
parallel -j 6 gdalwarp -r near -dstnodata -9999 -crop_to_cutline -cutline $PRODES_SHP -cwhere \"pathrow=\'{2}\'\" {1} "$WD"/mapbiomas_tiled/{1/.}_{2}.tif ::: $(find $WD -maxdepth 1 -type f -name "*.tif") ::: 22563 23367 22664

# clean
rm -rf "$WD"/prodes_2017 

