#!/bin/bash
# creates a csv identifying sites potentially influenced by impoundments
# csv file is named "impoundment_sites.csv" and the header is "id"
# the 3rd argument is an optional CSV specifying location id's to run
#   this file has a single column with the header "id"

# usage: $ ./id_impoundment_sites.sh <db name> <db username> <path to output directory> <path to check locations csv>
# example (specify locations): $ ./id_impoundment_sites.sh sheds dan /home/kyle/qaqc /conte/data/qaqc/check_locations.csv 
# example (all locations): $ ./id_impoundment_sites.sh sheds dan /home/kyle/qaqc 
# detail: it will ask for the database password for the user
# detail: log into GNU screen session if not running and saving to same machine to prevent timeout when writing results

set -eu
set -o pipefail

# Set variables
DB=$1
USER=$2
FOLDER=$3
CHECK_FILE=${4-"absent"}
      
# Sites to evaluate depends on the existence of the "CHECK_FILE" input
if [ $CHECK_FILE = "absent" ]; then

  # If no sites are specified, evaluate all  
  echo Identifying all sites influenced by impoundments...
  
  psql -h felek.cns.umass.edu -U $USER -d $DB -c "
  SELECT * INTO TEMPORARY locations_temp FROM public.locations;

  -- Add geometry   
  ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
  UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);
  CREATE INDEX idx_loc_dum_geom ON locations_temp USING GIST(geom);

  -- Select points near impoundment zones
  COPY (
  SELECT id
    FROM locations_temp, impoundment_zones_100m
    WHERE ST_Intersects(ST_Buffer(locations_temp.geom::geography, 10), impoundment_zones_100m.geom)
  ) TO STDOUT WITH CSV HEADER" > $FOLDER/impoundment_sites.csv
  
else
   
  # If no sites are specified, evaluate all 
  echo Identifying specified sites influenced by impoundments...

  psql -h felek.cns.umass.edu -U $USER -d $DB -c "
  -- Read in the specified sites to a temporary table
  CREATE TEMPORARY TABLE check_locations (id  int);

  COPY check_locations
  FROM '$CHECK_FILE'
  WITH CSV HEADER;

  -- Select only the specified locations to evaluate
  SELECT 
     loc.id, loc.latitude, loc.longitude
   INTO TEMPORARY locations_temp
  FROM 
     public.locations loc, 
     check_locations chk 
  WHERE 
     loc.id = chk.id;
  
  -- Add geometry  
  ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
  UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);
  CREATE INDEX idx_loc_dum_geom ON locations_temp USING GIST(geom);

  -- Select points near impoundment zones
  COPY (
  SELECT id
    FROM locations_temp, impoundment_zones_100m
    WHERE ST_Intersects(ST_Buffer(locations_temp.geom::geography, 10), impoundment_zones_100m.geom)
  ) TO STDOUT WITH CSV HEADER " > $FOLDER/impoundment_sites.csv
fi