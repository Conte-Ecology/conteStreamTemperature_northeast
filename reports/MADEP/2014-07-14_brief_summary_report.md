Massachusetts Daily Stream Temperature Modeling
===================

<center><strong>Daniel Hocking, Ben Letcher, Kyle O'Neil</strong></center>

<center><i>Silvio O. Conte Anadromous Fish Research Center, U.S. Geological Survey</i></center>

### Brief Summary Report: 14 July 2014

Our goal is to model daily stream temperature over large geographic areas as a function of air temperature, precipitation, and landscape characteristics. We used an index of air-water temperature synchrony to determine the part of the year where air temperature was directly affecting water temperature (i.e. minimal effects of ice-cover, phase change, and snow melt). We analyzed the synchronized period of each year at each site with a linear mixed effects model implemented in a Bayesian framework to be flexible and scaleable to large data sets. 

The model included air temperature, 1-day lagged air temperature, 2-day lagged air temperature, amount of precipitation that day and in each of the pervious two days (2 lags), drainage area, percent forest cover in the catchment, elevation of the stream reach, surficial coarseness of the catchment (how much sand, gravel, and rocks), percentage of the catchment that is comprised of wetland, area of stream impoundment, snow-water equivalent, latitude, longitude, and a cubic function of day of the year. We used year and each measurement site as random effects to account for correlation not explained by the other predictor variables and to adjust for unequal length time series at different sites and years. We modeled Massachusetts stream temperature data acquired from the MA Department of Environmental Protection, MA Division of Fisheries and Wildlife, and the U.S. Geological Survey.

Our model performed well with a root mean squared error of 0.89 <sup>o</sup>C, suggesting a typical accuracy within 1 <sup>o</sup>C. We found air temperature, forest cover, elevation, impoundments, and day of the year to be the most important predictors of stream temperature. There was also more correlation within sites than within years.

From this model, we will be able to predict daily stream temperature across time and space. This will allow us to calculate additional derived metrics of interest such as the average maximum summer temperature, number of days over a threshold temperature, frequency of thermally impaired days, resiliency to climate warming, etc. We can also compare the predicted stream temperatures for potential management actions (e.g. increased or decreased forest cover). The hierarchical mixed effects approach allows the incorporation of data from short monitoring periods and should provide accurate estimates even for sites that were monitored primarily during relatively extreme events (e.g. a once a decade heat wave). The model accuracy and generality will continue to improve as more data is incorporated. 

We are also investigating further enhancements to the model and incorporation of additional predictor variables. These include riparian forest cover, riparian impervious surfaces, distance to the nearest upstream dam, and various interactions among predictor variables.

### Figures

Examples of observed (blue) and predicted (red) stream temperature compared with air temperature (black).

#### Well-predicted

Our model predicts daily stream temperature extremely well for most sites as seen below:

<br>
<img src="/Users/Dan/Documents/Research/Stream_Climate_Change/temperatureProject/reports/MADEP/figures/MADEP_W0568_T1.png" height="375px" width="600px" />

#### Poorly Predicted

Our model predictions fail on a small number of sites as seen below. These are likely due to releases from impoundments where we do not have sufficient data to model the release timing and effects.

<br>
<img src="/Users/Dan/Documents/Research/Stream_Climate_Change/temperatureProject/reports/MADEP/figures/MADEP_W0454_T1.png" height="375px" width="600px" />

### Summary of Derived Metrics

From these models we can predict daily stream temperature for any stream reach where there is air temperature, precipitation, and landscape data. We are currently using Daymet [http://daymet.ornl.gov/](http://daymet.ornl.gov/) as our source of daily climate data from 1980-2013. Below are examples of predictions (red) for some sites where some stream temperature data was collected (blue).

<img src="/Users/Dan/Documents/Research/Stream_Climate_Change/temperatureProject/reports/MADEP/figures/MADEP_W2157_T1.png"/ height="400px", width="600px">

<img src="/Users/Dan/Documents/Research/Stream_Climate_Change/temperatureProject/reports/MADEP/figures/MAUSGS_WB_JIMMY.png"/ height="400px", width="600px">


From these predicted values across years, we calculate derived stream metrics including mean maximum stream temperature over years at a given site, max maximum stream temperature over years, mean number of days per year that the temperature is predicted to exceed 18 <sup>o</sup>C, the number of years from 1980 - 2013 that predicted maximum stream temperature exceeded 18 <sup>o</sup>C, the frequency of years that the max temperature exceeds 18 <sup>o</sup>C, and the average resistance to maximum air temperature change (likely indicator of groundwater inputs and resistance to climate change). We also calculate the average root mean squared error for each site and flag those with high values indicating that the predicted values do not match the observed values as well. These can then be checked for the influence of impoundments not accounted for in the model. Below is an example of derived metrics for some sites. A more complete list for currently processed data can be found in the accompanying excel file.

<img src="/Users/Dan/Documents/Research/Stream_Climate_Change/temperatureProject/reports/MADEP/figures/Table_Head.png"/>

Additionally, below are some figures depicting the derived metric *Resistance to Peak Air Temperature* (meanResist) for one site. The dark shaded area between the two lines is the representation of the resistance. For streams without significant groundwater inputs, during the hottest time of the year the area should be small (the red stream temperature line will closely follow the black air temperature line). In streams with more groundwater input, there will be a buffering (resistance) to changes in air temperature so this area will be larger. 

<img src="/Users/Dan/Documents/Research/Stream_Climate_Change/temperatureProject/reports/MADEP/figures/WB_Resistance.png"/ height="400px", width="600px">

Finally, we can calculate the effects of changing parameters in the model on stream temperature and these derived metrics. For example, we can calculate what the stream temperatures would be if forest cover was increased (or decreased) by 20%. We don't present those results here but they will be easy to add in the future.
