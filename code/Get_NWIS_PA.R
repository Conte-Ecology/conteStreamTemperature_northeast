# process PA data from NWIS

# clear environment
rm(list=ls())
gc()

# load packages
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dataRetrieval)

# Get List of Instantaneous Data Sites from NWIS Current Data list: http://waterdata.usgs.gov/pa/nwis/current?submitted_form=introduction
df_sites <- read_tsv("dataIn/NWIS_PA_Inst_Sites.txt", col_types = list(col_character(), col_character(), col_numeric(), col_numeric(), col_character(), col_character(), col_numeric()))

# get site list
sites <- unique(df_sites$site_no)
sites <- sites[!is.na(sites)]

#pull temp (00010) data from NWIS with dataRetrieval package for each site - one at a time so doesn't time out
df_temp <- dataRetrieval::readNWISuv(siteNumbers = sites[1], parameterCd = "00010")
for(i in 2:length(sites)) {
  x <- dataRetrieval::readNWISuv(siteNumbers = sites[i], parameterCd = "00010")
  df_temp <- bind_rows(df_temp, x)
}

dim(temp)

# df_temp <- dataRetrieval::readNWISuv(siteNumbers = "01432805", parameterCd = "00010")

# check time series plot
filter(df_temp, site_no=="01432805") %>%
  filter(as.Date(dateTime) >= as.Date("2009-12-01"),
         as.Date(dateTime) <= as.Date("2009-12-31")) %>%
  mutate(duplicated = duplicated(dateTime)) %>%
  ggplot(aes(dateTime, X_00010_00011, color=duplicated)) +
  geom_point() +
  ggtitle("NWIS_PA_Inst_Temp")

# Clean (rename and remove NA)
df_temp <- df_temp %>%
  dplyr::select(site_no, dateTime, tz = tz_cd, value = X_00010_00011) %>%
  dplyr::filter(!is.na(site_no),
                !is.na(dateTime)) %>%
  dplyr::arrange(site_no, dateTime)

df_sites <- df_sites %>%
  dplyr::filter(!is.na(site_no))

summary(df_temp)
str(df_temp)

# check the number of distinct site-dateTime combos and other duplicates
df_temp2 <- df_temp %>%
  dplyr::distinct(site_no, dateTime)

dim(df_temp2) == dim(df_temp)

length(unique(df_sites$site_no)) == length(unique(df_temp$site_no))

# check that one lat lon record per site 
dim(df_sites)[1] == length(unique(df_sites$site_no))

# check that the sites will match up when joined/queried
all(unique(df_temp$site_no) %in% df_sites$site_no)

# add new site numbers in case the current are too long and that's causing the problem with reading and writing
df_sites <- df_sites %>%
  dplyr::mutate(site_new = seq(1:nrow(df_sites)))

df_sites_sm <- df_sites %>%
  dplyr::select(site_no, site_new) # reduce so can just add site_new to df_temp without lots of extra columns since data is large

df_temp <- dplyr::left_join(df_temp, df_sites_sm)

# write cleaned site list
write.csv(df_sites, "dataIn/NWIS_PA_Inst_Sites.csv", row.names = FALSE)

write.csv(df_temp, file = "dataIn/NWIS_PA_Inst_Temp.csv", row.names = FALSE)

write_csv(df_temp, "dataIn/NWIS_PA_Inst_Temp2.csv")

df_temp_dist <- df_temp %>%
  distinct() %>%
  dim()

df_temp %>%
  distinct(site_no, dateTime) %>%
  dim()

df_temp %>%
  distinct(site_new, dateTime) %>%
  dim()

foo <- read_csv("dataIn/NWIS_PA_Inst_Temp.csv", col_types = list(col_character(), col_datetime(), col_character(), col_numeric(), col_integer())) # messes up import of dateTime unless types specified

# bar <- read.table("dataIn/NWIS_PA_Inst_Temp.txt", stringsAsFactors = FALSE, header = TRUE)

# fu <- read_delim("dataIn/NWIS_PA_Inst_Temp.txt", delim = " ", col_types = list(col_character(), col_datetime(), col_character(), col_numeric()))

foo_dist <- foo %>%
  distinct() %>%
  dim() # should be the same size as df_temp_dist

foo_dist[1] == df_temp_dist[1]

# check time series plot
filter(foo, site_no=="01432805") %>%
  filter(as.Date(dateTime) >= as.Date("2009-12-01"),
         as.Date(dateTime) <= as.Date("2009-12-31")) %>%
  mutate(duplicated = duplicated(dateTime)) %>%
  ggplot(aes(dateTime, value, color=duplicated)) +
  geom_point() +
  ggtitle("NWIS_PA_Inst_Temp")

# bar_dist <- bar %>%
#   distinct() %>%
#   dim() # smaller than df_temp_dist when saved with readr and imported with base



