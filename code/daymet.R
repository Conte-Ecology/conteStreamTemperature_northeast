library(RPostgreSQL)
library(ggplot2)

get_daymet_featureids <- function(con, featureids) {
  # con: database connection returned from RPostgreSQL::dbConnect
  # featureids: numeric or character vector of featureids
  
  # returns data frame of daymet data with columns:
  #   [featureid, date, tmax, tmin, prcp, dayl, srad, vp, swe]
  featureids_string <- paste0("{", paste0(featureids, collapse=","), "}")

  sql <- paste0("select * from get_daymet_featureids('", featureids_string, "');")
  dbGetQuery(con, sql)
}

con <- dbConnect(dbDriver("PostgreSQL"),
                 dbname="daymet")
x0 <- get_daymet_featureids(con)                  # throws error: missing featureids
x0 <- get_daymet_featureids(con, featureids = "") # returns empty dataframe
x1 <- get_daymet_featureids(con, featureids = c(201407698))
x2 <- get_daymet_featureids(con, featureids = c(201407698, 201407699))
x3 <- get_daymet_featureids(con, featureids = c("201407698", "201407699"))
x5 <- get_daymet_featureids(con, featureids = c(201407698, 201407699, 201407700, 201407701, 201407702))
dbDisconnect(con)

ggplot(x2, aes(date, tmin)) +
  geom_line() +
  facet_wrap(~featureid)
