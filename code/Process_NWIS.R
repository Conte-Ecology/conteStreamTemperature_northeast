# process PA data from NWIS

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Read data downloaded from NWIS Water Quality site
# pa <- read_tsv(file = "dataIn/NWIS_PA.txt")
# sites <- read_csv("dataIn/NWIS_PA_Sites.csv")

# Get List of Instantaneous Data Sites from NWIS Current Data list: http://waterdata.usgs.gov/pa/nwis/current?submitted_form=introduction
sites <- read_tsv("dataIn/NWIS_PA_Inst_Sites.txt")

# Clean (rename and remove NA)
pa <- pa %>%
  dplyr::select(site_number = site_no, date = sample_dt, time = sample_tm, value = result_va) %>%
  dplyr::filter(!is.na(site_number),
                !is.na(date)) %>%
  dplyr::arrange(site_number, date)

sites <- sites %>%
  dplyr::filter(!is.na(site_number))

# check that one lat lon record per site 
dim(sites)[1] == length(unique(sites$site_number))

# make series id and exclude any data not part of a series at least 4 days long
pa$year <- year(pa$date)

pa$site_prev <- c(NA, as.integer(pa[1:(nrow(pa)-1), ]$site_number))
pa$date_prev <- c(pa[1, ]$date, pa[1:(nrow(pa)-1), ]$date)
pa[1, ]$date_prev <- NA

pa_series <- pa %>%
  dplyr::arrange(site_number, date) %>%
  #filter(year >= 1980) %>%
  dplyr::mutate(same_day = ifelse(site_number == site_prev & date == date_prev, TRUE, FALSE),
         next_day = ifelse(site_number == site_prev & date == date_prev + days(1), TRUE, FALSE),
         start_series = ifelse(same_day == TRUE | next_day == TRUE, FALSE, TRUE))

pa_series$start_next <- c(pa_series$start_series[2:nrow(pa)], NA)

pa_series <- pa_series %>%
  dplyr::arrange(site_number, date) %>%
  dplyr::filter((start_series + start_next) < 2,
                year > 2007) %>%
  dplyr::arrange(site_number, date)

head(pa_series, 20)

ggplot(pa_series, aes(date, value, group = site_number, colour = site_number)) + geom_line() + ylim(0, 30)





# Join location and temp data
foo <- left_join(pa, sites)

dim(pa)[1] == dim(foo)[1]




# write cleaned site list
write.csv(sites, "dataIn/NWIS_PA_Sites.csv", row.names = FALSE)

write.csv(pa, file = "dataIn/NWIS_PA_Temp.csv", row.names = FALSE)





