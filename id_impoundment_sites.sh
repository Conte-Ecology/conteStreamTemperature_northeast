#!/bin/bash
# creates a csv identifying sites potentially influenced by impoundments
# csv file is named "impoundment_sites.csv" and the header is "id"
# the 3rd argument is an optional CSV specifying location id's to run
#   this file has a single column with the header "id"

# usage: $ ./id_impoundment_sites.sh <db name> <path to output directory>
# requires: .pgpass file with hostname, user, and password to connect to database
# example (all locations): $ ./id_impoundment_sites.sh sheds /home/kyle/qaqc 
# detail: log into GNU screen session if not running and saving to same machine to prevent timeout when writing results

set -eu
set -o pipefail

# Set variables
DB=$1
FOLDER=$2

 # If no sites are specified, evaluate all  
  echo Identifying all sites influenced by impoundments...
  
  psql -h felek.cns.umass.edu -d $DB -w -c "
SELECT * INTO TEMPORARY locations_temp FROM public.locations;

-- Add geometry   
ALTER TABLE locations_temp ADD COLUMN geom geometry(POINT,4326);
UPDATE locations_temp SET geom = ST_SetSRID(ST_MakePoint(longitude,latitude),4326);
CREATE INDEX idx_locations_temp_geom ON locations_temp USING GIST(geom);

ALTER TABLE locations_temp ADD COLUMN buffer geometry(POLYGON,4326);
UPDATE locations_temp SET buffer = ST_Buffer(locations_temp.geom::geography, 10)::geometry;
CREATE INDEX idx_locations_temp_buffer ON locations_temp USING GIST(buffer);


-- Select points near impoundment zones
COPY (
SELECT id
  FROM locations_temp, gis.impoundment_zones_100m
  WHERE ST_Intersects(locations_temp.buffer, gis.impoundment_zones_100m.geom)
) TO STDOUT WITH CSV HEADER" > $FOLDER/impoundment_sites.csv
  