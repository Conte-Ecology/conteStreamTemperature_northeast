# Daily amplitude changes or extremes or matching to air
# Sync of air and water temperatures




# send SQL commands to the database
tbl(my_db, sql("SELECT * FROM hflights LIMIT 100"))



foo <- left_join(df_values, df_locations)
West <- filter(foo, grepl("Finney", location_name))
unique(West$location_name)

West <- West %>%
  mutate(year = year(datetime),
         month = month(datetime),
         dom = day(datetime))

bar <- filter(West, year == 2013)# & month >= 1 & month <=6)

ggplot(bar, aes(datetime, value)) + geom_line() #+ ylim(12, 20)

#----------Uniformity of records within a day-----------------------------------

# check to make sure datetime was done correctly when guessing 12-hr vs 24-hr formats



#---------Check for duplicates: not necessary while things are being averaged----

#n_occur <- data.frame(table(temperatureData$site_date))
#n_occur[n_occur$Freq > 1, ]
#temperatureData[temperatureData$site_date %in% n_occur$Var1[n_occur$Freq > 1], ]

#temperatureData[temperatureData$site_date == "736_2007-08-01", ]

#----------Check for out of water (out of water vs. dry stream?)---------------- 
#Rate of change > 3 C per hour

flag_3C_hour <- function(data) {
  data <- data %>%
    group_by(series_id, date) %>%
    
}

#daily mean change > 3 C between successive days has picked up several dewatering events in my data.  It has missed a couple dewatering events that occurred during the fall.

#----------Check for excessively cold events------------------------------------

# Convert -1 - 0 C readings to 0
Flagging observations < -1 C makes sense.  

#---------Check for excessively hot readings------------------------------------
Flagging observations > 30 C might not work for the database, as it isn't all small trout streams.

#---------Flagging extremes-----------------------------------------------------
Flagging upper and lower 5th percentiles probably wont work for the database, as those are best reverified by whoever collected/uploaded the data.

