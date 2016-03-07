flag_daily_rise <- function (data, deg = 5) 
{
  data <- ungroup(data)
  data$row <- 1:nrow(data)
  data$temp_prev <- c(NA_real_, data[2:nrow(data) - 1, ]$temp)
  if(length(data$series_id)==0) {
    data$series_id = data$featureid
  }
  data$series_prev <- c(NA_character_, data[1:nrow(data) - 
                                              1, ]$series_id)
  data <- data %>% mutate(series_start = ifelse(is.na(series_prev), 
                                                1, ifelse(series_id == series_prev, NA_real_, 1)), temp_prev = ifelse(is.na(series_start), 
                                                                                                                      temp_prev, NA_real_), d_temp = temp - temp_prev, flag_daily_rise = ifelse(abs(d_temp) < 
                                                                                                                                                                                                  deg | is.na(d_temp), FALSE, TRUE))
  return(data)
}



MAD.roller <- function(vals, window){
library(RcppRoll)
if(!is.numeric(window) | length(window) != 1) {
stop("window must be a single numeric value")
}
#vals <- data[ , var]
# Croux and Rousseeuw, 1992
if(window >= 10) {
b <- 1/qnorm(3/4)
}
if(window == 9) {
b <- 1.107
}
if(window == 8) {
b <- 1.129
}
if(window == 7) {
b <- 1.140
}
if(window == 6) {
b <- 1.200
}
if(window == 5) {
b <- 1.206
}
if(window == 4) {
b <- 1.363
}
if(window < 4) {
stop("window must be a numeric value >= 4")
}
warning('MAD.roller function has not been robustly tested w/ NAs')
u.i <- is.finite(vals)
left.fill <- median(head(vals[u.i], ceiling(window/2)))
right.fill <- median(tail(vals[u.i], ceiling(window/2)))
medians <- roll_median(vals[u.i], n=window, fill=c(left.fill, 0, right.fill))
abs.med.diff <- abs(vals[u.i]-medians)
left.fill <- median(head(abs.med.diff, ceiling(window/2)))
right.fill <- median(tail(abs.med.diff, ceiling(window/2)))
abs.med <- roll_median(abs.med.diff, n=window, fill=c(left.fill, 0, right.fill))
MAD <- abs.med*b
MAD.normalized = rep(NA,length(vals))
MAD.normalized[u.i] <- abs.med.diff/MAD # division by zero
MAD.normalized[is.na(MAD.normalized)] <- 0
#data$MAD.normalized <- MAD.normalized
#return(data)
return(MAD.normalized)
}


MAD.windowed <- function(vals, windows){
stopifnot(length(vals) == length(windows))
if (length(unique(windows)) == 1){
w = unique(windows)
x = vals
return(MAD.roller(x, w))
} else {
. <- '_dplyr_var'
mad <- group_by_(data.frame(x=vals,w=windows), 'w') %>% 
  mutate_(mad='sensorQC:::MAD.values(x)') %>% 
  .$mad
return(mad)
}
}


# flag if n number of points in a row are within 0.1 C (calc number of ~identical temps in a row)
roll_consistant <- function() {
  
}


# visualize time series with remaining sub-daily problems
dir.create(file.path(data_dir, "diagnostics"))
for(i in 1:length(bad_series_id)) {
  bad_series <- as.data.frame(dplyr::filter(df_values, series_id == bad_series_id[i]))
  bad_hourly <- bad_series %>%
    dplyr::filter(abs(d_temp) > 4) %>%
    as.data.frame(stringsAsFactors = FALSE)
  bad_MAD <- bad_series %>%
    dplyr::filter(MAD_normalized > 10) %>%
    as.data.frame(stringsAsFactors = FALSE)
  bad_MAD_30 <- bad_series %>%
    dplyr::filter(MAD_normalized_30 > 10) %>%
    as.data.frame(stringsAsFactors = FALSE)
  g <- ggplot(bad_series, aes(datetime, temp)) + geom_line() + geom_point() + ylim(c(0, 30)) + ggtitle(paste0("series_id: ", bad_series_id[i]))
  if(nrow(bad_hourly) > 0) {
    g <- g + geom_point(data = bad_hourly, aes(datetime, temp), colour = "yellow", size = 5, alpha = 0.8)
  }
  if(nrow(bad_MAD) > 0) {
    g <- g + geom_point(data = bad_MAD, aes(datetime, temp), colour = "red", size = 5, alpha = 0.5)
  }
  if(nrow(bad_MAD_30) > 0) {
    g <- g + geom_point(data = bad_MAD_30, aes(datetime, temp), colour = "green", size = 5, alpha = 0.5)
  }
  ggsave(file = file.path(data_dir, "diagnostics", paste0("series_", bad_series_id[i], ".png")), plot = g)
}

# MAD doesn't seem effective near the ends of any time series

# challenge of how to exclude all points during out of water event (in field or office) without manually doing it for each time series!

