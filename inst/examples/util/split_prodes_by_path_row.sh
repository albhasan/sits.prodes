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
parallel -j6 ogr2ogr -skipfailures -where \"pathrow=\'{1}\'\" "$WD"/tiled/{2/.}_{1}.shp {2} ::: 001066 001067 002066 002067 225063 226062 226063 226064 226068 227067 227068 233067 ::: "$PRODES_SHP"

# fix topological issues
parallel ogr2ogr -sql '"SELECT ST_Buffer(geometry, 0.0), linkcolumn, uf, pathrow, scene_id, mainclass, class_name, dsfnv, julday, view_date, ano, areameters FROM {1/.}"' -dialect SQLite "$WD"/buffer/{1/} {1} ::: $(find "$WD"/tiled -maxdepth 1 -type f -name "*.shp")

# rename 
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_001_066.shp "$WD"/tiled/PDigital2017_AMZ_pol_001066.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_001_067.shp "$WD"/tiled/PDigital2017_AMZ_pol_001067.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_002_066.shp "$WD"/tiled/PDigital2017_AMZ_pol_002066.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_002_067.shp "$WD"/tiled/PDigital2017_AMZ_pol_002067.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_225_063.shp "$WD"/tiled/PDigital2017_AMZ_pol_225063.shp 
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_226_062.shp "$WD"/tiled/PDigital2017_AMZ_pol_226062.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_226_063.shp "$WD"/tiled/PDigital2017_AMZ_pol_226063.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_226_068.shp "$WD"/tiled/PDigital2017_AMZ_pol_226068.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_227_067.shp "$WD"/tiled/PDigital2017_AMZ_pol_227067.shp
ogr2ogr -f "ESRI Shapefile" "$WD"/tiled/PDigital2017_AMZ_pol_227_068.shp "$WD"/tiled/PDigital2017_AMZ_pol_227068.shp

# cleaning
rm -rf "$WD"/prodes_2017
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_001066.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_001067.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_002066.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_002067.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_225063.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_226062.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_226063.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_226068.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_227067.*
rm -rf "$WD"/tiled/PDigital2017_AMZ_pol_227068.*

exit 0

