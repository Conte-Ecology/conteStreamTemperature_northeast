# Northeast Daily Stream Temperature
Daniel J Hocking  
3/25/2016  



## Abstract

## Objectives

Our objective was to develop novel statistical model of daily stream temperature that incorporates features of stochastic models and extends the Letcher et al. (2016) framework to large geographic areas. The model needed to (1) handle time series data of widely varying duration from many sites accounting for autocorrelation at specific locations within watersheds, (2) while also including information about catchment, landscape, and meteorological conditions for explanatory and predictive purposes. (3) For a daily temperature model we needed to include an autoregressive function to account for temporal autocorrelation in the time series, a challenge with other statistical models at fine temporal resolution. (4) We also wanted to account for uncertainty at various levels and propogate this through to predictions. 

We use the model to predict daily stream temperature across the northeastern United States over a 34-year time record.

## Approach 

Statistical models of stream temperature often rely on the close relationship between air temperature and water temperature. However, this relationship breaks down during the winter in temperature zones, particularly as streams freeze, thereby changing their thermal and properties. Many researchers and managers are interested in the non-winter effects of temperature. The winter period, when phase change and ice cover alter the air-water relationship, differs in both time (annually) and space. We developed an index of air-water synchrony ($Index_{sync}$) so we can model the portion of the year that it not affected by freezing properties. The index is the difference between air and observed water temperatures divided by the water temperature plus 0.000001 to avoid division by zero. 

We calculate the $Index_{sync}$ for each day of the year at each reach for each year with observed data. We then calculate the 99.9% confidence interval of $Index_{sync}$ for days between the 125 and 275 days of the year (05 May and 02 October). Then moving from the middle of the year (day 180) to the beginning of the year, we searched for the first time when 10 consecutive days were not within the 99.9% CI. This was selected as the spring breakpoint. Similarly moving from the middle to the end of the year, the first event with fewer than 16 consecutive days within the 99.9% CI was assigned as the autumn breakpoint. Independent breakpoints were estimated for each reach-year combination. More details regarding the identification of the synchronized period can be found in Letcher et al. (2016). The portion of the year between the spring and autumn breakpoints was used for modeling the non-winter, approximately ice-free stream temperatures.

We used a generalized linear mixed model to account for correlation in space (stream reach nested within HUC8). This allowed us to incorporate short time series as well as long time series from different reaches and disjunct time series from the same reaches without risk of pseudoreplication (Hurlbert 1984). By limited stream drainage area to <200 $km^2$ and only modeling the synchronized period of the year, we were able to use a linear model, avoiding the non-linearities that occur at very high temperatures due to evaporative cooling and near 0 C due to phase change (Mohseni 1999).

## Model Structure

We assumed stream temperature measurements were normally distributed following,

$$ t_{h,r,y,d} \sim \mathcal{N}(\mu_{h,r,y,d}, \sigma) $$

where $t_{h,r,y,d}$ is the observed stream water temperature at the reach ($r$) within the sub-basin identified by the 8-digit Hydrologic Unit Code (HUC8; $h$) for each day ($d$) in each year ($y$). We describe the normal distribution based on the mean ($mu_{h,r,y,d}$) and standard deviation ($\sigma$) and assign a vague prior of $\sigma = 100$. The mean temperature is modeled to follow a linear trend

$$ \omega_{h,r,y,d} = X_0 B_0 + X_{h,r} B_{h,r} + X_{h} B_{h} + X_{y} B_{y} $$

but the expected mean temperature ($\mu_{h,r,y,d}$) was also adjusted based on the residual error from the previous day

$$ \mu_{h,r,y,d} = \begin{cases}
    \omega_{h,r,y,d} + \delta(t_{h,r,y,d-1} - \omega_{h,r,y,d-1}) & \quad  \text{for $t_{h,r,y,d-1}$ is real} \\
    \omega_{h,r,y,d} & \quad  \text{for $t_{h,r,y,d-1}$ is not real}
  \end{cases}
 $$

where $\delta$ is an autoregressive [AR(1)] coefficient and $\omega_{h,r,y,d}$ is the expected temperature before accounting for temporal autocorrelation in the error structure.

$X_{0}$ is the $n \times K_0$ matrix of predictor values. $B_0$ is the vector of $K_0$ coefficients, where $K_0$ is the number of fixed effects parameters including the overall intercept. 

$B_{h,r}$ is the $R \times K_{R}$ matrix of regression coefficients where $R$ is the number of unique reaches and $K_{R}$ is the number of regression coefficients that vary randomly by reach within HUC8. The effects of daily air temperature and mean air temperature over the previous 7 days varied randomly with reach and HUC8 (Table 1). 

$X_{h}$ is the matrix of parameters that vary by HUC8. We allowed for correlation among the effects of these HUC8 coefficients as described by Gelman and Hill [-@Gelman2007].

$B_{h}$ is the $H \times K_{H}$ matrix of coefficients where $H$ is the number of HUC8 groups and $K_H$ is the number of parameters that vary by HUC8 including a constant term. 

Similarly, we allowed the some effects of some parameters ($X_{y}$) to vary randomly by year with potential correlation among the coefficients. The intercept, day of the year ($day$), $day^2$, and $day^3$ all varied randomly with year such that $K_{y} = 4$. 

## Stream Temperature Data (Dependent Data)

Add summary of agencies, sites, and amounts of data

## Predictor Variables

| Variable | Description | Source | Processing | GitHub Repository |
|:--------:| --------------------------- | --------------- | ------------------------- | ----------------- |
| Total Drainage Area | The total contributing drainage area from the entire upstream network | [The SHEDS Data project](http://conte-ecology.github.io/shedsData/) | The individual polygon areas are summed for all of the catchments in the contributing network| [NHDHRDV2](https://github.com/Conte-Ecology/shedsData/tree/master/NHDHRDV2) |
| Riparian Forest Cover | The percentage of the upstream 200ft riparian buffer area that is covered by trees taller than 5 meters | [The National LandCover Database (NLCD)](http://www.mrlc.gov/nlcd06_data.php) | All of the NLCD forest type classifications are combined and attributed to each riparian buffer polygon  using GIS tools. All upstream polygon values are then aggregated.| [nlcdLandCover](https://github.com/Conte-Ecology/shedsData/tree/master/basinCharacteristics/rasterPrep/nlcdLandCover) |
| Daily Precipition | The daily precipitation record for the individual local catchment | [Daymet Daily Surface Weather and Climatological Summaries](https://daymet.ornl.gov/) | Daily precipitation records are spatially assigned to each catchment based on overlapping grid cells using the [zonalDaymet](https://github.com/Conte-Ecology/zonalDaymet) R package| [daymet](https://github.com/Conte-Ecology/shedsData/tree/master/daymet) |
| Upstream Impounded Area| The total area in the contributing drainage basin that is covered by wetlands, lakes, or ponds that intersect the stream network | [U.S. Fish & Wildlife Service (FWS) National Wetlands Inventory](http://www.fws.gov/wetlands/Data/Data-Download.html)| All freshwater surface water bodies are attributed to each catchment using GIS tools. All upstream polygon values are then aggregated.| [fwsWetlands](https://github.com/Conte-Ecology/shedsData/tree/master/basinCharacteristics/rasterPrep/fwsWetlands) |
| Percent Agriculture | The percentage of the contributing drainage area that is covered by agricultural land (e.g. cultivated crops, orchards, and pasture) including fallow land. | [The National LandCover Database](http://www.mrlc.gov/nlcd06_data.php)| All of the NLCD agricutlural classifications are combined and attributed to each catchment polygon using GIS tools. All upstream polygon values are then aggregated.| [nlcdLandCover](https://github.com/Conte-Ecology/shedsData/tree/master/basinCharacteristics/rasterPrep/nlcdLandCover) |
| Percent High Intensity Developed | The percentage of the contributing drainage area covered by places where people work or live in high numbers (typically defined as areas  covered by more than 80% impervious surface) | [The National LandCover Database](http://www.mrlc.gov/nlcd06_data.php)| The NLCD high intensity developed classification is attributed to each catchment polygon using GIS tools. All upstream polygon values are then aggregated. | [nlcdLandCover](https://github.com/Conte-Ecology/shedsData/tree/master/basinCharacteristics/rasterPrep/nlcdLandCover) |


## General Results

### Coefficients Table


|                       Parameter |   Mean |    SD |   LCRI |    UCRI |
|--------------------------------:|-------:|------:|-------:|--------:|
|                       Intercept | 17.051 | 0.123 |  16.81 | 17.2980 |
|                            AirT |  2.029 | 0.020 |   1.99 |  2.0694 |
|                      7-day AirT |  1.551 | 0.025 |   1.50 |  1.5997 |
|                    2-day Precip |  0.051 | 0.002 |   0.05 |  0.0541 |
|                   30-day Precip |  0.001 | 0.006 |  -0.01 |  0.0130 |
|                   Drainage Area |  0.371 | 0.058 |   0.26 |  0.4849 |
|                  Impounded Area |  0.181 | 0.067 |   0.05 |  0.3092 |
|           Riparian Forest Cover | -0.249 | 0.050 |  -0.35 | -0.1504 |
|                High Development | -0.002 | 0.032 |  -0.06 |  0.0615 |
|                     Agriculture | -0.139 | 0.047 |  -0.23 | -0.0461 |
|             AirT x 2-day Precip |  0.019 | 0.002 |   0.02 |  0.0228 |
|            AirT x 30-day Precip | -0.033 | 0.003 |  -0.04 | -0.0261 |
|                 AirT x Drainage |  0.027 | 0.019 |  -0.01 |  0.0662 |
|           AirT x Impounded Area | -0.047 | 0.023 |  -0.09 | -0.0003 |
|                   AirT x Forest | -0.016 | 0.017 |  -0.05 |  0.0181 |
|         AirT x High Development | -0.001 | 0.011 |  -0.02 |  0.0212 |
|             AirT x Agriculture, | -0.017 | 0.016 |  -0.05 |  0.0147 |
|         2-day Precip x Drainage | -0.039 | 0.002 |  -0.04 | -0.0353 |
|        30-day Precip x Drainage | -0.044 | 0.006 |  -0.06 | -0.0336 |
|  AirT x 2-day Precip x Drainage | -0.015 | 0.002 |  -0.02 | -0.0118 |
| AirT x 30-day Precip x Drainage | -0.005 | 0.003 |  -0.01 |  0.0017 |
|                             AR1 |  0.813 | 0.002 |   0.81 |  0.8159 |



|   Group |       Coef |   SD |   Variance |
|--------:|-----------:|-----:|-----------:|
|    Site |  Intercept | 1.09 |      1.186 |
|         |       AirT | 0.34 |      0.113 |
|         | 7-day AirT | 0.36 |      0.132 |
|    HUC8 |  Intercept | 0.64 |      0.412 |
|         |       AirT | 0.24 |      0.059 |
|         | 7-day AirT | 0.28 |      0.080 |
|    Year |  Intercept | 0.27 |      0.076 |



|       &nbsp; |   Intercept |   AirT |   7-day AirT |
|-------------:|------------:|-------:|-------------:|
|    Intercept |             |        |              |
|         AirT |       0.553 |        |              |
|   7-day AirT |       0.306 |  0.126 |              |


## Model Fit and Predictions
