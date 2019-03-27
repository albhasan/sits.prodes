#!/bin/bash
###############################################################################
# SPLIT PRODES DATA BY LANDSAT SCENE
#------------------------------------------------------------------------------
# Last update 2018-05-23
###############################################################################
PRODES_SHP='../data/vector/prodes_2017/PDigital2017_AMZ_pol.shp'
OUT_DIR="../data/vector/prodes_tiled"
#ogr2ogr -where "pathrow='21975'" $OUT_DIR/prodes_219_075.shp $PRODES_SHP
ogr2ogr -where "pathrow='22462'" $OUT_DIR/prodes_224_062.shp $PRODES_SHP
ogr2ogr -where "pathrow='22463'" $OUT_DIR/prodes_224_063.shp $PRODES_SHP
ogr2ogr -where "pathrow='22563'" $OUT_DIR/prodes_225_063.shp $PRODES_SHP
ogr2ogr -where "pathrow='23065'" $OUT_DIR/prodes_230_065.shp $PRODES_SHP
ogr2ogr -where "pathrow='23266'" $OUT_DIR/prodes_232_066.shp $PRODES_SHP
ogr2ogr -where "pathrow='23267'" $OUT_DIR/prodes_232_067.shp $PRODES_SHP
ogr2ogr -where "pathrow='23367'" $OUT_DIR/prodes_233_067.shp $PRODES_SHP
#
ogr2ogr -where "pathrow='22562'" $OUT_DIR/prodes_225_062.shp $PRODES_SHP
ogr2ogr -where "pathrow='22564'" $OUT_DIR/prodes_225_064.shp $PRODES_SHP
ogr2ogr -where "pathrow='22664'" $OUT_DIR/prodes_226_064.shp $PRODES_SHP
ogr2ogr -where "pathrow='22668'" $OUT_DIR/prodes_226_068.shp $PRODES_SHP
ogr2ogr -where "pathrow='22761'" $OUT_DIR/prodes_227_061.shp $PRODES_SHP
ogr2ogr -where "pathrow='22762'" $OUT_DIR/prodes_227_062.shp $PRODES_SHP
ogr2ogr -where "pathrow='22967'" $OUT_DIR/prodes_229_067.shp $PRODES_SHP
ogr2ogr -where "pathrow='23166'" $OUT_DIR/prodes_231_066.shp $PRODES_SHP
 
