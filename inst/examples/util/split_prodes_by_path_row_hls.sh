#!/bin/bash
###############################################################################
# SPLIT PRODES DATA BY LANDSAT SCENE
#------------------------------------------------------------------------------
# Last update 2019-04-12
###############################################################################
BASE_PATH="/home/alber/Documents/data/experiments/prodes_reproduction"
WD="$BASE_PATH"/data/vector/prodes
PRODES_TAR="$BASE_PATH"/data/vector/prodes/prodes_2017.tar.gz
PRODES_SHP="$WD"/prodes_2017/PDigital2017_AMZ_pol.shp

mkdir "$WD"/buffer
mkdir "$WD"/prodes_tiled
mkdir "$WD"/tiled

tar -xzf $PRODES_TAR --directory "$WD"

# get the scenes of AOI
parallel -j6 ogr2ogr -skipfailures -where \"pathrow=\'{1}\'\" "$WD"/tiled/{2/.}_{1}.shp {2} ::: 00166 00167 ::: "$PRODES_SHP"

# fix topological issues
parallel ogr2ogr -sql '"SELECT ST_Buffer(geometry, 0.0), linkcolumn, uf, pathrow, scene_id, mainclass, class_name, dsfnv, julday, view_date, ano, areameters FROM {1/.}"' -dialect SQLite "$WD"/buffer/{1/} {1} ::: $(find "$WD"/tiled -maxdepth 1 -type f -name "*.shp")

# copy polygons
parallel ogr2ogr -f \"ESRI Shapefile\" "$WD"/prodes_tiled/{1/} {1} ::: $(find "$WD"/buffer -maxdepth 1 -type f -name "*.shp")

# rename 
ogr2ogr -f "ESRI Shapefile" "$WD"/prodes_tiled/PDigital2017_AMZ_pol_00166.shp "$WD"/prodes_tiled/PDigital2017_AMZ_pol_001_066.shp 
ogr2ogr -f "ESRI Shapefile" "$WD"/prodes_tiled/PDigital2017_AMZ_pol_00167.shp "$WD"/prodes_tiled/PDigital2017_AMZ_pol_001_067.shp

# cleaning
rm -rf "$WD"/buffer "$WD"/prodes_2017 "$WD"/tiled 
rm "$WD"/prodes_tiled/PDigital2017_AMZ_pol_00166.*
rm "$WD"/prodes_tiled/PDigital2017_AMZ_pol_00167.*

exit 0
 
