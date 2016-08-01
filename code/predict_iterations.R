#------------------- predictions by iteration--------------
# by iteration
data <- tempDataSyncValidS

#pb <- txtProgressBar(style = 3)
metrics.list <- list()
start <- Sys.time()
for(i in 1:n.iter) {
  fixed.ef <- as.numeric(B.0[i, ]) # separate out the iteration or do for mean/median
  # add specific random effects to the dataframe
  df <- data %>%
    dplyr::select(-sitef) %>%
    left_join(rand_ids$df_site) %>%
    left_join(rand_ids$df_huc) %>%
    left_join(rand_ids$df_year)
  
  
  
  df <- left_join(df, B.site.list[[i]], by = "sitef")
  df <- left_join(df, B.huc.list[[i]], by = "hucf") # problem with validation data, need to use the mean when huc don't match
  df <- left_join(df, B.year.list[[i]], by = "yearf")
  
  
  for (j in 2:length(names(B.site.list[[i]]))) {
    df[, names(B.site.list[[i]][j])][is.na(df[, names(B.site.list[[i]][j])])] <- colMeans(B.site.list[[i]][j])
  }
  for (j in 2:length(names(B.huc.list[[i]]))) {
    df[, names(B.huc.list[[i]][j])][is.na(df[, names(B.huc.list[[i]][j])])] <- colMeans(B.huc.list[[i]][j])
  }
  for (j in 2:length(names(B.year.list[[i]]))) {
    df[, names(B.year.list[[i]][j])][is.na(df[, names(B.year.list[[i]][j])])] <- colMeans(B.year.list[[i]][j])
  }
  #     dplyr::filter(dOY >= finalSpringBP & dOY <= finalFallBP | is.na(finalSpringBP) | is.na(finalFallBP & finalSpringBP != "Inf" & finalFallBP != "Inf")) %>%
  #dplyr::filter(dOY >= mean.spring.bp & dOY <= mean.fall.bp) %>%
  # df <- dplyr::filter(df, AreaSqKM < 200) only works on the original scale
  
  
  
  df$fixed.ef <- as.vector(fixed.ef %*% t(as.matrix(as.data.frame(unclass(select(ungroup(data), one_of(cov.list$fixed.ef)))))))
  df$site.ef <- rowSums(as.matrix(select(df, one_of(cov.list$site.ef))) * as.matrix(select(df, starts_with("B.site"))))
  df$huc.ef <- rowSums(as.matrix(select(df, one_of(cov.list$huc.ef))) * as.matrix(select(df, starts_with("B.huc"))))
  df$year.ef <- rowSums(as.matrix(select(df, one_of(cov.list$year.ef))) * as.matrix(select(df, starts_with("B.year"))))
  
  df$trend <- rowSums(as.matrix(dplyr::select(df, one_of(c("fixed.ef", "site.ef", "huc.ef", "year.ef")))))
  
  # Add B.ar1 to predictions
  #df <- group_by(df, sitef)
  df <- mutate(df, prev.temp = c(NA, df$temp[(2:(nrow(df))) -1]),
               prev.trend = c(NA, df$trend[(2:nrow(df)) - 1]),
               prev.err = prev.temp - prev.trend,
               tempPredicted = trend,
               prev.temp = ifelse(newDeploy == 1, NA, prev.temp),
               prev.err = ifelse(newDeploy == 1, NA, prev.err))
  B.ar1.sub <- data.frame(sitef = rand_ids$df_site$sitef)
  B.ar1.sub$B.ar1 <- as.numeric(B.ar1[ ,i]) # iteration i
  df <- left_join(df, B.ar1.sub, by = c("sitef"))
  df <- df %>%
    dplyr::mutate(B.ar1 = ifelse(is.na(B.ar1), mean(B.ar1.sub$B.ar1, na.rm = T), B.ar1)) %>%
    dplyr::arrange(featureid, date)
  
  df[which(!is.na(df$prev.err)), ]$tempPredicted <- df[which(!is.na(df$prev.err)), ]$trend + df[which(!is.na(df$prev.err)), ]$B.ar1 * df[which(!is.na(df$prev.err)), ]$prev.err
  
  #df <- dplyr::select(df, featureid, date, year, temp, trend, tempPredicted)
  
  metrics.list[[i]] <- deriveMetrics(df)
  
  cat(paste0("Iteration ", i, " of ", n.iter, ": ", Sys.time() - start, "\n"), file = paste0(data_dir, "/derived_metrics_uncertainty_loop.txt"), append = T)
  
  #names(df) <- c("featureid", "date", paste0("trend_", i), paste0("tempPredicted_", i))
  #df_preds <- dplyr::left_join(df_preds, df)
  #setTxtProgressBar(pb, i)
}


metrics.array <- array(unlist(metrics.list), dim = c(nrow(metrics.list[[1]]), ncol(metrics.list[[1]]), n.iter))
metrics.mean <- apply(metrics.array, 1:2, mean, na.rm = T)
metrics.sd <- apply(metrics.array, 1:2, sd, na.rm = T)
metrics.LCRI <- apply(metrics.array, 1:2, FUN = function(x) quantile(x, probs = c(0.025), na.rm = T))
metrics.UCRI <- apply(metrics.array, 1:2, FUN = function(x) quantile(x, probs = c(0.975), na.rm = T))
colnames(metrics.mean) <- names(metrics.list[[1]])
colnames(metrics.sd) <- names(metrics.list[[1]])
colnames(metrics.LCRI) <- names(metrics.list[[1]])
colnames(metrics.UCRI) <- names(metrics.list[[1]])
end <- Sys.time()
end - start



