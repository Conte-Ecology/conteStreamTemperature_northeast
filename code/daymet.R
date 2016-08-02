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


n <- sample(c(5, 10, 20, 50, 100, 250, 500, 750, 1000), size = 20, replace = TRUE)
times <- rep(NA, length.out = length(n))
for(i in 1:length(n)) {
  Start <- Sys.time()
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "sheds_new", host = "osensei.cns.umass.edu",
                   user = options("SHEDS_USERNAME"), password = options("SHEDS_PASSWORD"))
  catches <- catchmentid[1:n[i]]
  foo <- get_daymet_featureids(con, featureids = catches)
  dbDisconnect(con)
  End <- Sys.time()
  times[i] <- timediff(End - Start, units = "mins")
  rm(foo)
}

df_times <- data.frame(n, times, stringsAsFactors = FALSE)
# df_times <- df_times %>%
#   dplyr::mutate(new_time = ifelse(n > 250, times*60, times))
ggplot(df_times, aes(n, times)) + geom_point() + geom_smooth()

# ggplot(df_times, aes(n, new_time)) + geom_point() + geom_smooth()
