#!/bin/bash
###############################################################################
# SPLIT DETER dATA BY LANDSAT SCENE
#------------------------------------------------------------------------------
# Last update 2019-04-12
###############################################################################
BASE_PATH="/home/alber/Documents/data/experiments/prodes_reproduction"
WD="$BASE_PATH"/data/vector/deter
DETER_TAR="$BASE_PATH"/data/vector/deter/deter.tar.gz
DETER_SHP="$WD"/deter_public.shp

mkdir "$WD"/tiled

tar -xzf $DETER_TAR --directory "$WD"

# get the landsat scenes of AOI
parallel -j6 ogr2ogr -skipfailures -where \"PATH_ROW=\'{1}\'\" "$WD"/tiled/{2/.}_{1}.shp {2} ::: 163104 163105 163106 164103 164104 164105 164106 164107 164108 164112 164113 164114 165102 165103 165104 165105 165106 165107 165112 165113 165114 166102 166103 166104 166105 166106 166107 166111 166112 166113 166114 167110 167111 167112 167113 167114 168110 168111 168112 168113 168114 176111 176112 177109 177110 177111 177112 178109 178110 178111 178112 179109 179110 179111 179112 180109 180110 180111 180112 181109 181110 181111 181112 ::: "$DETER_SHP"

# fix topological issues
# parallel  ogr2ogr -sql '"SELECT ST_Buffer(geometry, 0.0), CLASSNAME, QUADRANT, PATH_ROW VIEW_DATE SENSOR SATELLITE AREAUCKM UC AREAMUNKM MUNICIPALI UF FROM {1/.}"' -dialect SQLite "$WD"/buffer/{1/} {1} ::: $(find "$WD"/tiled -maxdepth 1 -type f -name "*.shp")

# cleaning
rm -rf "$WD"/deter_public.*

exit 0

