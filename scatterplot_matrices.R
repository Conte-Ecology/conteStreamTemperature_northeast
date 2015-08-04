library(dplyr)

load("localData/tempDataSync.RData")

source("C:/Users/dhocking/Dropbox/Functions/scatterplot_matrix.R")

pair_vector <- c("airTemp", "temp5p", "temp7p", "prcp", "prcp30", "srad", "dOY2", "dayl", "forest")

Pairs <- tempDataSyncS %>%
  dplyr::select(one_of(pair_vector)) %>%
  dplyr::sample_n(1000) # too big to plot otherwise

pairs(Pairs, upper.panel = panel.smooth, lower.panel = panel.cor, diag.panel = panel.hist)
