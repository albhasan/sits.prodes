#!/bin/bash
# Dissolve PRODES shapefile into Landsat's scenes.

BASE_PATH="/home/alber/Documents/data/experiments/prodes_reproduction"
WD="$BASE_PATH"/data/vector/prodes
PRODES_TAR="$BASE_PATH"/data/vector/prodes/prodes_2017.tar.gz
PRODES_SHP="$WD"/prodes_2017/PDigital2017_AMZ_pol.shp

mkdir "$WD"/scenes
tar -xzf $PRODES_TAR --directory "$WD"

echo "ERROR: Dissolve results are tables and not shapefiles! - use QGIS"
exit 1

parallel -j4 ogr2ogr -f '"ESRI Shapefile"' "$WD"/scenes/prodes_scene_{2}.shp "$PRODES_SHP" -dialect sqlite -sql '"SELECT pathrow,ST_Union(geometry, 0.0) AS geometry FROM '{1/.}' GROUP BY pathrow"' ::: $PRODES_SHP ::: 22563 23367 22664

