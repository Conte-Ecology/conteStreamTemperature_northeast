This is the project folder for the stream temperature work underway at the USGS S.O. Conte Anadromous Fish Research Center in Turners Falls, MA.

The stream temperature model estimates effects of landscape variables (% forest cover, % agriculture, drainage area, etc.) and time varying variables (solar radiation, air temperature, precipitation, etc.) on daily stream water temperature. For each site/year combination, the estimates are limited to the times of the year where air temperature and water temperature are synchronized to avoid issues with ice-cover and phase changes.

## Run the Model

In the future, a bash script or make file should be used to automate the process. For now, however, the process is run manually with the following steps (*have this project open in RStudio to ensure proper file directory structure*):

**1. Pull data from postgres database**

Script: *retrieve_db.R*

Usage: Run code in R


**2. Pull daymet data using sql script**

Script: */code/daymet_query.sql*

Usage: via command line $ psql -d sheds -f code/daymet_query.sql > localData/daymet_results.csv

**3. Calculate syncronized period of the year**

Script: *breakpoints.R*

Usage: Run code in R


## Automated Model Run

**requires permissions to access and write to the sheds database on felek**

1. log into GNU screen via command line on local computer (optional)
2. Connect to osensei via ssh to run the model (optional: assumes running model on osensei)
3. Start persistent screen session
4. Go to `conteStreamTemperature_northeast/` directory (or clone from GitHub if first use)
5. Run `run_model.sh` bash script
6. Enter `sheds` database username when prompted then hit enter
7. Enter `sheds` database password when prompted then hit enter

**Example:**

```
$ screen -S osensei
$ ssh dan@osensei.cns.umass.edu
$ screen -S temperature
$ cd conteStreamTemperature_northeast
$ bash run_model.sh

```

*`$` represent commandline prompt

This script will create a new directory called "modelRun_" followed by the date initiated (e.g. `modelRun_2016-06-27`). Within this directory, a log file will be created (`status_log.txt`) which will give an update each time a new part of the script is started.
