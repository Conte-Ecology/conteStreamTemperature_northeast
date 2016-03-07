
load(file = paste0(output_file4, "/data_dir.RData"))

df_south <- df_values %>%
  dplyr::filter(latitude < 40.7127)

if(!file.exists(file.path(getwd(), data_dir, "plots"))) dir.create(file.path(getwd(), data_dir, "plots"))

series <- unique(df_south$series_id)
for(i in series) {
  ggplot(df_south[which(df_south$series_id == i), ], aes(datetime, temp)) + geom_line(color = "blue")
  ggsave(paste0(data_dir, "/plots/series_", i, ".png"))
}

df_series <- data.frame(series_id = series, stringsAsFactors = FALSE)
write.csv(df_series, file = paste0(data_dir, "/series_id.csv"), row.names = FALSE)
