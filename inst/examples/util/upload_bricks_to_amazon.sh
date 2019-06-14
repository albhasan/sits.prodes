#!/bin/bash

# upload bricks to Amazon s3


if [ "$USER" = "scidb" ]; then
  echo "ERROR: This script must be run as user scidb"
  exit 1
fi


if [ "$HOSTNAME" = "esensing-006" ]; then
  echo "ERROR: This script must be run on esensing-006"
fi

# ssh scidb@150.163.2.206
# cd /home/scidb/shared/rolf

# create buckets
#eval $mc mb s3/landsat-wrs-233-067-interp
#eval $mc mb s3/landsat-wrs-233-067-starfm

# test
#touch test.txt
#eval $mc cp test.txt s3/landsat-wrs-233-067-interp/test.txt
#eval $mc ls test.txt s3/landsat-wrs-233-067-interp
#eval $mc rm test.txt s3/landsat-wrs-233-067-interp/test.txt
#rm test.txt

mc="/home/scidb/shared/rolf/./mc"

eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2013-08-29_ndvi_STACK_BRICK.tif  s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2013-08-29_nir_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2013-08-29_red_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2013-08-29_swir2_STACK_BRICK.tif s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2014-08-29_ndvi_STACK_BRICK.tif  s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2014-08-29_nir_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2014-08-29_red_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2014-08-29_swir2_STACK_BRICK.tif s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2015-08-29_ndvi_STACK_BRICK.tif  s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2015-08-29_nir_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2015-08-29_red_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2015-08-29_swir2_STACK_BRICK.tif s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2016-08-28_ndvi_STACK_BRICK.tif  s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2016-08-28_nir_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2016-08-28_red_STACK_BRICK.tif   s3/landsat-wrs-233-067-interp/
eval $mc cp /home/alber/shared/brick_interp/LC8SR-MOD13Q1-MYD13Q1_233067_2016-08-28_swir2_STACK_BRICK.tif s3/landsat-wrs-233-067-interp/

eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_ndvi_STACK_BRICK.tif          s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_nir_STACK_BRICK.tif           s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_red_STACK_BRICK.tif           s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2014-08-05_swir2_STACK_BRICK.tif         s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_ndvi_STACK_BRICK.tif          s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_nir_STACK_BRICK.tif           s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_red_STACK_BRICK.tif           s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2015-08-08_swir2_STACK_BRICK.tif         s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_ndvi_STACK_BRICK.tif          s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_nir_STACK_BRICK.tif           s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_red_STACK_BRICK.tif           s3/landsat-wrs-233-067-starfm/
eval $mc cp /home/alber/shared/brick/LC8SR-MOD13Q1-STARFM_233067_2016-08-10_swir2_STACK_BRICK.tif         s3/landsat-wrs-233-067-starfm/

