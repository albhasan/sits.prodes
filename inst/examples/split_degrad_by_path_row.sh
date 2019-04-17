#!/bin/bash
###############################################################################
# SPLIT DEGRAD DATA BY LANDSAT SCENE
#------------------------------------------------------------------------------
# Last update 2019-04-12
###############################################################################
BASE_PATH="/home/alber/Documents/data/experiments/prodes_reproduction"
WD="$BASE_PATH"/data/vector/degrad
PRODES_TAR="$BASE_PATH"/data/vector/prodes/prodes_2017.tar.gz
PRODES_SHP="$WD"/prodes_2017/PDigital2017_AMZ_pol.shp

mkdir "$WD"/degrad_tiled
mkdir "$WD"/degrad_year
mkdir "$WD"/buffer
mkdir "$WD"/tiled

# extract files
tar -xzf $PRODES_TAR --directory "$WD"
find  "$WD" -maxdepth 1 -type f -name "*_shp.zip" -exec unzip -qq {} -d "$WD"/degrad_year \;

# get the landsat scenes of AOI
parallel -j6 ogr2ogr -skipfailures "$WD"/tiled/{1/.}_{2}.shp {1} -clipsrc "$PRODES_SHP" -clipsrcwhere \"pathrow=\'{2}\'\" ::: $(find "$WD"/degrad_year -maxdepth 1 -type f -name "*.shp") ::: 22563 23367 22664

# fix topological issues
parallel ogr2ogr -sql '"SELECT ST_Buffer(geometry,  0.0), linkcolumn, cell_oid, class_name, scene_id, pathrow, uf, areameters, view_date, julday FROM {1/.}"' -dialect SQLite "$WD"/buffer/{1/} {1} ::: $(find "$WD"/tiled -maxdepth 1 -type f -name "*.shp")

# copy polygons
parallel ogr2ogr -f \"ESRI Shapefile\" "$WD"/degrad_tiled/{1/} {1} ::: $(find "$WD"/buffer -maxdepth 1 -type f -name "*.shp")

# cleaning
rm -rf "$WD"/buffer "$WD"/degrad_year "$WD"/prodes_2017 "$WD"/tiled 

exit 0

