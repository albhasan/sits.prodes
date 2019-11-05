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

mkdir "$WD"/tiled

tar -xzf $PRODES_TAR --directory "$WD"

# get the landsat scenes of AOI
parallel -j6 ogr2ogr -skipfailures -where \"pathrow=\'{1}\'\" "$WD"/tiled/{2/.}_{1}.shp {2} ::: 22563 23367 22664 00167 00166 ::: "$PRODES_SHP"

# fix topological issues
parallel ogr2ogr -sql '"SELECT ST_Buffer(geometry, 0.0), linkcolumn, uf, pathrow, scene_id, mainclass, class_name, dsfnv, julday, view_date, ano, areameters FROM {1/.}"' -dialect SQLite "$WD"/buffer/{1/} {1} ::: $(find "$WD"/tiled -maxdepth 1 -type f -name "*.shp")

# rename 
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_225_063.shp "$WD"/tiled/PDigital2017_AMZ_pol_22563.shp 
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_226_064.shp "$WD"/tiled/PDigital2017_AMZ_pol_22664.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_233_067.shp "$WD"/tiled/PDigital2017_AMZ_pol_23367.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_001_066.shp "$WD"/tiled/PDigital2017_AMZ_pol_00166.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_001_067.shp "$WD"/tiled/PDigital2017_AMZ_pol_00167.shp

# cleaning
rm -rf "$WD"/prodes_2017
rm "$WD"/tiled/PDigital2017_AMZ_pol_22563.*
rm "$WD"/tiled/PDigital2017_AMZ_pol_22664.*
rm "$WD"/tiled/PDigital2017_AMZ_pol_23367.*
rm "$WD"/tiled/PDigital2017_AMZ_pol_00166.*
rm "$WD"/tiled/PDigital2017_AMZ_pol_00167.*

exit 0
 
