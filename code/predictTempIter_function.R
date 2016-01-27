predictTempIter <- function (data, coef.list, cov.list, rand_ids, Random_AR1 = FALSE) {
  if (!exists("data$sitef")) {
    data <- data %>% left_join(rand_ids$df_site)
  }
  if (!exists("data$hucf")) {
    data <- data %>% left_join(rand_ids$df_huc)
  }
  if (!exists("data$yearf")) {
    data <- data %>% left_join(rand_ids$df_year)
  }
  fixed.ef <- as.numeric(coef.list$B.fixed$mean)
  data <- left_join(data, coef.list$B.site)
  data <- left_join(data, coef.list$B.huc)
  data <- left_join(data, coef.list$B.year)
  
  
  
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
    
  
  
  
  
  for (j in 2:length(names(coef.list$B.site))) {
    data[, names(coef.list$B.site[j])][is.na(data[,names(coef.list$B.site[j])])] <-
      colMeans(coef.list$B.site[j])
  }
  for (j in 2:length(names(coef.list$B.huc))) {
    data[, names(coef.list$B.huc[j])][is.na(data[,
                                                 names(coef.list$B.huc[j])])] <-
      colMeans(coef.list$B.huc[j])
  }
  for (j in 2:length(names(coef.list$B.year))) {
    data[, names(coef.list$B.year[j])][is.na(data[,
                                                  names(coef.list$B.year[j])])] <-
      colMeans(coef.list$B.year[j])
  }
  data$fixed.ef <-
    as.vector(fixed.ef %*% t(as.matrix(as.data.frame(unclass(
      select(ungroup(data),
             one_of(cov.list$fixed.ef))
    )))))
  data$site.ef <- rowSums(as.matrix(select(
    data,
    one_of(cov.list$site.ef)
  )) * as.matrix(select(data,
                        starts_with("B.site"))))
  data$huc.ef <- rowSums(as.matrix(select(
    data,
    one_of(cov.list$huc.ef)
  )) * as.matrix(select(data,
                        starts_with("B.huc"))))
  data$year.ef <- rowSums(as.matrix(select(
    data,
    one_of(cov.list$year.ef)
  )) * as.matrix(select(data,
                        starts_with("B.year"))))
  data$trend <-
    data$fixed.ef + data$site.ef +
    data$huc.ef + data$year.ef
  data <- mutate(
    data, prev.temp = c(NA,
                        data$temp[(2:(nrow(data))) - 1]), prev.trend = c(NA,
                                                                         data$trend[(2:nrow(data)) - 1]), prev.err = prev.temp -
      prev.trend, tempPredicted = trend, prev.temp = ifelse(newDeploy ==
                                                              1, NA, prev.temp), prev.err = ifelse(newDeploy == 1,
                                                                                                   NA, prev.err)
  )
  if (Random_AR1) {
    B.ar1.sub <- coef.list$B.ar1 %>% dplyr::select(sitef,
                                                   mean) %>% dplyr::rename(B.ar1 = mean)
    data <- left_join(data, B.ar1.sub,
                      by = c("sitef"))
    data <-
      data %>% dplyr::mutate(B.ar1 = ifelse(is.na(B.ar1),
                                            mean(B.ar1.sub$B.ar1, na.rm = T), B.ar1)) %>% dplyr::arrange(featureid,
                                                                                                         date)
  }
  else {
    data$B.ar1 <- coef.list$B.ar1$mean
    data <- data %>% dplyr::arrange(featureid,
                                    date)
  }
  data[which(!is.na(data$prev.err)),]$tempPredicted <-
    data[which(!is.na(data$prev.err)),]$trend + data[which(!is.na(data$prev.err)),]$B.ar1 * data[which(!is.na(data$prev.err)),]$prev.err
  data <- dplyr::arrange(data, rowNum)
  data <-
    data.frame(unclass(data), stringsAsFactors = FALSE)
  return(data)
}