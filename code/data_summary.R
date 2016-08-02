library(pander)
library(ggplot2)
library(dplyr)
library(RPostgreSQL)
library(lubridate)
library(conteStreamTemperature)
library(texreg)
library(stargazer)

data_dir <- as.character(read.table("current_model_run.txt", stringsAsFactors = FALSE)[1,1])

df_series <- read.csv(file = file.path(data_dir, "series_used.csv"), stringsAsFactors = FALSE)
series_used <- unique(df_series$series_id)

df_values <- readRDS(file = file.path(data_dir, "df_values.RData"))

library(RPostgreSQL)
db <- src_postgres(dbname='sheds', host='felek.cns.umass.edu', port='5432', user=options('SHEDS_USERNAME'), password=options('SHEDS_PASSWORD'))

tbl_states <- tbl(db, 'catchment_state')

df_states <- dplyr::collect(tbl_states, n = Inf) %>%
  dplyr::rename(state = stusps) %>%
  dplyr::mutate(featureid = as.integer(featureid))
# str(df_states)

# dbDisconnect(db$con)

agency_summary <- df_values %>%
  dplyr::filter(series_id %in% series_used) %>%
  dplyr::mutate(featureid = as.integer(featureid)) %>%
  dplyr::left_join(df_states) %>%
  dplyr::group_by(state, agency_name) %>%
  dplyr::mutate(year = year(date)) %>%
  dplyr::summarise(n_records = n(),
                   n_years = length(unique(year)),
                   n_locations = length(unique(location_id)),
                   n_streams = length(unique(featureid))) %>%
  dplyr::filter(!(state %in% c(NA, "NA", "NC", "TN")))
# print(as.data.frame(state_summary, stringsAsFactors = FALSE))

state_summary <- df_values %>%
  dplyr::ungroup() %>%
  dplyr::filter(series_id %in% series_used) %>%
  dplyr::mutate(featureid = as.integer(featureid)) %>%
  dplyr::left_join(df_states) %>%
  dplyr::group_by(state) %>%
  dplyr::mutate(year = year(date)) %>%
  dplyr::summarise(n_records = n(),
                   n_years = length(unique(year)),
                   n_locations = length(unique(location_id)),
                   n_streams = length(unique(featureid))) %>%
  dplyr::filter(!(state %in% c(NA, "NA", "NC", "TN")))

totals <- df_values %>%
  dplyr::filter(series_id %in% series_used) %>%
  dplyr::mutate(featureid = as.integer(featureid)) %>%
  dplyr::left_join(df_states) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(year = year(date)) %>%
  dplyr::filter(!(state %in% c(NA, "NA", "NC", "TN"))) %>%
  dplyr::summarise(n_records = n(),
                   n_years = length(unique(year)),
                   n_locations = length(unique(location_id)),
                   n_streams = length(unique(featureid)))
df_total <- data.frame("Totals: ", totals, stringsAsFactors = FALSE)
names(df_total) <- names(state_summary)

# agency_summary <- dplyr::bind_rows(agency_summary, df_total)

saveRDS(agency_summary, paste0(data_dir, "/agency_summary.Rds"))
saveRDS(state_summary, paste0(data_dir, "/state_summary.Rds"))
saveRDS(df_total, paste0(data_dir, "/data_totals.Rds"))

closeAllConnections()