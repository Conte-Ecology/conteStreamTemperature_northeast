# Priorities for Temperature Model Development

## Daniel J. Hocking
### 19 September 2014
<hr>

### Riparian forest cover and impervious surface

### Network structure

### Site nested within HUC

### Additional spatial pattern: kriging or random fields 

### Autoregressive

* I'm not sure how to code it in JAGS or TMB to take temperature for the previous day at a given site. 
* Maybe a while statement if data is sorted by site then date.
* This would replace day of the year

### Add interactions

* airTemp * drainage

### Residual correlation structure

### Alt. temp/precip lag

* 5-, 7-, 10-day total precip 
* 5-, 7-, or 10-day average air temp