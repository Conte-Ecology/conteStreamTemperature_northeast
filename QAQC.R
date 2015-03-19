

#----------Add column denoting frequency of observations------------------------

#' @param data time series data pull from the postgres database (df_values table)

obs_freq <- function(data) {
  obs_per_day <- data %>%
    dplyr::group_by(series_id, date) %>%
    dplyr::summarise(obs_per_day = n())
  
  median_obs <- obs_per_day %>%
    dplyr::group_by(series_id) %>%
    dplyr::summarise(median_freq = median(obs_per_day, na.rm = T), min_n90 = median_freq*0.9)
  
  data <- data %>%
    left_join(obs_per_day, by = c("series_id", "date")) %>%
    left_join(median_obs, by = c("series_id"))
  
  return(data)
}

#----------Uniformity of records within a day-----------------------------------

# check to make sure datetime was done correctly when guessing 12-hr vs 24-hr formats

# flag inconsistent time intervals within a day at a given location or series (deployment?)
flag_interval <- function(data) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- ungroup(data)
  data <- group_by(data, series_id, date)
  data$row <- 1:nrow(data)
  data$time_prev <- c(NA_real_, data[2:nrow(data)-1, ]$datetime)
  data$series_prev <- c(NA_character_, data[1:nrow(data)-1, ]$series_id)
  data_series <- data %>%
    group_by(series_id, date) %>%
    mutate(series_start = ifelse(is.na(series_prev), 1, ifelse(series_id == series_prev, NA_real_, 1)),
           time_prev = ifelse(is.na(series_start), time_prev, NA_real_),
           d_time = ifelse(median_freq > 1, datetime - time_prev, NA_real_)) %>%
    summarise(flag_interval = ifelse(max(d_time, na.rm = T) != min(d_time, na.rm = T), TRUE, FALSE))
  
  data <- left_join(data, data_series, by = c("series_id", "date"))
  
  return(data)
}

#---------Check for duplicates: not necessary while things are being averaged----

#n_occur <- data.frame(table(temperatureData$site_date))
#n_occur[n_occur$Freq > 1, ]
#temperatureData[temperatureData$site_date %in% n_occur$Var1[n_occur$Freq > 1], ]

#temperatureData[temperatureData$site_date == "736_2007-08-01", ]



#----------Flag incomplete days--------------

# Flag days with less than 90% of median number of observations for that series

flag_incomplete <- function(data) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
data <- data %>%
  dplyr::mutate(flag_incomplete = ifelse(obs_per_day <= min_n90, TRUE, FALSE))
return(data)
}

#----------Check for out of water (out of water vs. dry stream?)---------------- 
# Rate of change in temperature per hour
flag_hourly_rise <- function(data, deg = 5) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- ungroup(data)
  data$row <- 1:nrow(data)
  data$temp_prev <- c(NA_real_, data[2:nrow(data)-1, ]$temp)
  data$series_prev <- c(NA_character_, data[1:nrow(data)-1, ]$series_id)
  data <- data %>%
    #group_by(series_id) %>%
    mutate(series_start = ifelse(is.na(series_prev), 1, ifelse(series_id == series_prev, NA_real_, 1)),
           temp_prev = ifelse(is.na(series_start), temp_prev, NA_real_),
           d_temp = ifelse(median_freq == 24, temp - temp_prev, NA_real_),
           flag_hourly_rise = ifelse(abs(d_temp) < deg | is.na(d_temp), FALSE, TRUE))
  
return(data)
}


# Rate of change in mean temperature per day
flag_daily_rise <- function(data, deg) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- ungroup(data)
  data$row <- 1:nrow(data)
  data$temp_prev <- c(NA_real_, data[2:nrow(data)-1, ]$temp)
  data$series_prev <- c(NA_character_, data[1:nrow(data)-1, ]$series_id)
  data <- data %>%
    #group_by(series_id) %>%
    mutate(series_start = ifelse(is.na(series_prev), 1, ifelse(series_id == series_prev, NA_real_, 1)),
           temp_prev = ifelse(is.na(series_start), temp_prev, NA_real_),
           d_temp = ifelse(median_freq == 1, temp - temp_prev, NA_real_),
           flag_daily_rise = ifelse(abs(d_temp) < deg | is.na(d_temp), FALSE, TRUE))
  
  return(data)
}

#----------Check for excessively cold events------------------------------------

# Convert -1 - 0 C readings to 0
convert_neg <- function(data) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- data %>%
    mutate(temp = ifelse(temp < 0 & temp >= -1 & median_freq > 1, 0, temp))
  
  return(data)
}

# Flagging observations < -1 C makes sense.  

flag_cold_obs <- function(data) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- data %>%
    mutate(flag_cold_obs = ifelse(temp < -1 & median_freq > 1, TRUE, FALSE))
  
  return(data)
}

flag_cold_days <- function(data) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- data %>%
    mutate(flag_cold_days = ifelse(temp < -1 & median_freq == 1, TRUE, FALSE))
  
  return(data)
}

#---------Check for excessively hot readings------------------------------------
# Flagging observations > 30 C might not work for the database, as it isn't all small trout streams.

flag_hot_obs <- function(data, threshold = 35) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- data %>%
    mutate(flag_hot_obs = ifelse(temp > threshold & median_freq > 1, TRUE, FALSE))
  
  return(data)
}

flag_hot_days <- function(data, threshold = 35) {
  if(!("median_freq" %in% colnames(data, threshold))) {
    data <- obs_freq(data)
  }
  data <- data %>%
    mutate(flag_hot_days = ifelse(temp > threshold & median_freq == 1, TRUE, FALSE))
  
  return(data)
}

#---------Flagging extremes-----------------------------------------------------
# Flagging upper and lower 5th percentiles probably wont work for the database, as those are best reverified by whoever collected/uploaded the data.

flag_extreme_obs <- function(data, qlo = 0.001, qhi = 0.999) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- data %>%
    mutate(flag_extremes = ifelse(temp > quantile(temp, c(qhi), na.rm = T) | temp < quantile(temp, c(qlo) & median_freq > 1, na.rm = T), TRUE, FALSE))
  
  return(data)
}


flag_extreme_days <- function(data, qlo = 0.001, qhi = 0.999) {
  if(!("median_freq" %in% colnames(data))) {
    data <- obs_freq(data)
  }
  data <- data %>%
    mutate(flag_extremes = ifelse(temp > quantile(temp, c(qhi), na.rm = T) | temp < quantile(temp, c(qlo) & median_freq == 1, na.rm = T), TRUE, FALSE))
  
  return(data)
}







