#!/bin/bash
###############################################################################
# SPLIT DETER DATA BY LANDSAT SCENE 
#------------------------------------------------------------------------------
# Last update 2020-02-20
###############################################################################
echo "It takes too long. Use QGIS instead".
exit 1

BASE_PATH="/home/alber/Documents/data/experiments/prodes_reproduction"
WD="$BASE_PATH"/data/vector/deter
DETER_TAR="$WD"/deter.tar.gz
DETER_SHP="$WD"/deter_public.shp
WRS2_SHP="$BASE_PATH/data/vector/wrs2_asc_desc/wrs2_asc_desc.shp"

mkdir "$WD"/tiled

tar -xzf "$DETER_TAR" --directory "$WD"

#ogr2ogr "$WD"/tiled/deter_233066.shp "$DETER_SHP" -clipsrcwhere "MODE='D' AND PR=1066"  -clipsrc "$WRS2_SHP" 

parallel -j 6 ogr2ogr "$WD/tiled/deter_{1}.shp" "$DETER_SHP" -clipsrcwhere "\"MODE=\'D\' AND PR={1}\"" -clipsrc "$WRS2_SHP" ::: 1066 1067 2066 2067 225063 226062 226063 226064 226068 227067 227068 233067


parallel -j 6 gdalwarp -r near -dstnodata -9999 -crop_to_cutline -cutline $PRODES_SHP -cwhere \"pathrow=\'{2}\'\" {1} "$WD"/mapbiomas_tiled/{1/.}_{2}.tif ::: $(find $WD -maxdepth 1 -type f -name "*.tif") ::: 22563 23367 22664

