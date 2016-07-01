# Conte Stream Temperature Model for Northeastern Headwater Streams

This is the project folder for the stream temperature work underway at the USGS S.O. Conte Anadromous Fish Research Center in Turners Falls, MA.

The stream temperature model estimates effects of landscape variables (% forest cover, % agriculture, drainage area, etc.) and time varying variables (solar radiation, air temperature, precipitation, etc.) on daily stream water temperature. For each site/year combination, the estimates are limited to the times of the year where air temperature and water temperature are synchronized to avoid issues with ice-cover and phase changes.

More documentation can be found at http://conte-ecology.github.io/conteStreamTemperature_northeast/

The model and R package used in this analysis can be found at: https://github.com/Conte-Ecology/conteStreamTemperature

## Automated Model Run

**requires:**
 * **permissions to access and write to the sheds database on felek**
 * **pgpass file to bypass password to sheds database, instructions at: https://blog.sleeplessbeastie.eu/2014/03/23/how-to-non-interactively-provide-password-for-the-postgresql-interactive-terminal/** (e.g. `ecosheds.org:5432:*:username:password`)

1. log into GNU screen via command line on local computer (optional - see tutorial https://www.rackaid.com/blog/linux-screen-tutorial-and-how-to/)
2. Connect to osensei via ssh to run the model (optional: assumes running model on osensei)
3. Start persistent screen session
4. Go to `conteStreamTemperature_northeast/` directory (or clone from GitHub if first use)
5. Set validate, debug, and MAD options in the `model_config.json` file
6. Run `run_model.sh` bash script

**Model Configuration Details (model_config.json)**

* `validate` - boolean indicating whether to hold out data for validation. Defaults to true.
* `debug` - boolean incdicating whether to turn on debug features. Defaults to true.
* `mad_tf` - boolean indicating whether to use the Median Absolute Deviance in the automated QAQC. Defaults to false. It adds 12+ hours to the model run, increasing non-linearly with increasing data, and the cutoffs are somewhat arbitraty and largely unexplored. It also only removes single days and not full out of water segments.

**Example:**

```
$ screen -S osensei
$ ssh dan@osensei.cns.umass.edu
# $ screen -S temperature
$ cd conteStreamTemperature_northeast
$ screen -d -m -S temperature bash run_model.sh

```

*`$` represents commandline prompt

This script will create a new directory called "modelRun_" followed by the date initiated (e.g. `modelRun_2016-06-27`). Within this directory, a log file will be created (`status_log.txt`) which will give an update each time a new part of the script is started.

## Manual Model Run

**requires:**
 * **permissions to access and write to the sheds database on felek**
 * **pgpass file to bypass password to sheds database, instructions at: https://blog.sleeplessbeastie.eu/2014/03/23/how-to-non-interactively-provide-password-for-the-postgresql-interactive-terminal/** (e.g. `ecosheds.org:5432:*:username:password`)
 
Follow these steps to run the model manually:

`CL` = command line in the `conteStreamTemperature_northeast` directory on osensei or local machine

1. Set the model configuration (`model_config.json`)
2. Set the model run directory in the file "current_model_run.txt". This should be a single line naming the subdirectory (e.g. `modelRun_2016-06-30`)
3. determine what locations are near impoundments (CL: `bash id_impoundment_sites.sh sheds <subdirecotry/>`)
4. determine what locations are potentially tidally influenced (e.g. CL: `bash id_tidal_sites.sh sheds <username> <subdirectory/>`)
5. Fetch data that are reviewed (`retrieve_db.R`)
6. Fetch daymet data (i.e. CL: `psql -f <subdirectory>/code/daymet_query.sql -d sheds -w > <subdirectory>/daymet_results.csv`)
7. Determine breakpoints of synchronized season (`breakpoints.R`)
8. Prepare data for use in model (`prepare_model_data.R`) 
9. Run the statistical model (`run_model.R`)
10. Get mcmc and model diagnostics (`mcmc_diagnostics.R`)
11. Summarize the model (`summarize_iterations.R`)
12. Validate model (`validate_model.R`)
13. Calculate derived metrics for all catchments (`predict_temperature_parallel.R`)


