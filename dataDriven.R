library(tidyverse)
library(raster)
library(lubridate)
## Read in occurrence data
occ<-read_csv('/home/pgalante/Projects/Git/WorkingCode/All_new_records_by_year.csv')
## Global Wc to crop 
e1<-stack(list.files(path = '/home/pgalante/Projects/layers/wc2_5', pattern = '\\.tif$', full.names = T))
## Set Datescale: can be "year", "month", or "day"
dateScale<-"year"
#########################################################################################################
##  Name rasters using associated dates (in this example simulated dates)
dates <- list(sample(seq(as.Date('2005-01-01'), as.Date('2016-01-01'), by="day"), 19))
dates <- arrange(uniqueDates, years)$years
test <- lapply(dates, function(x) paste("Y", x, sep=''))
e1<-e1[[1:8]]
names(e1) <- lapply(dates, function(x) paste("Y", x, sep=''))

# ##################  Metadata assignment? Might be better to name rasters as dates  ######################
# ## assign metadata as yyyy/mm/dd for raster stack (this follows as.Date convention)
# dates<-list(sample(seq(as.Date('2005/01/01'), as.Date('2016/01/01'), by="day"), 19))
# # Figure out a way for lapply to work here?
# for (i in 1:nlayers(e1)){
#   metadata(e1[[i]])<-as.list(dates[[1]][[i]])
# }


###############################################################################
### Extract occs into sub-tables based on dates (here, using only year)  ######
##############  Eventually, find online RS data for tutorial  #################
##  First parse out the dates into the appropriate dateScale (year, month, date)
colnames(occ)<-c("long", "lat", "date")
reDate <- function(DatedOccs){
  if(dateScale == "year"){
    return(occ %>% dplyr::select(long,lat,date) %>%
             mutate(date = as.Date(paste0(date, "-01-01"))) %>%
             mutate(years = year(date)))
  }
  if(dateScale == "month"){
    return(occ %>% dplyr::select(long,lat,date) %>%
             mutate(date = parse_date_time(date, "ym")) %>%
             mutate(months = month(date)) %>%
             mutate(years = year(date)))
  }
  if(dateScale == "day"){
    return(occ %>% dplyr::select(long,lat,date) %>%
             mutate(date = ymd(date)) %>%
             mutate(years = year(date)) %>% 
             mutate(months = month(date)) %>%
             mutate(days = day(date)))
  }
}
occ1 <- reDate(occ)
##  Next, for each appropriate dateScale, get unique dates
uniDates<-function(occ1, dateScale){
  require(lubridate)
  
  if (dateScale == "year"){
    return(arrange(unique(dplyr::select(occ1, years)), years))
  } 
  if (dateScale == "month"){
    return(arrange(unique(dplyr::select(occ1, years, months)), years))
  } 
  if (dateScale == "day"){
    return(arrange(unique(dplyr::select(occ1, years, months, days)), years))
  }
}
uniqueDates <- uniDates(occ1, dateScale)
##  Last, for each unique date, create different tibbles
parseDate <- function(dateScale, occ1, uniqueDates){
  t1 <- NULL
  if (dateScale == "year"){
    for (i in 1:nrow(uniqueDates)){
      t1[[i]] = filter(occ1, years == as.list(uniqueDates)$years[[i]])
    }}
  #### NEED TO FIGURE OUT YEAR MONTH DAY COMBINATIONS
  if (dateScale == "month"){
    t1 <- NULL
    for (i in 1:nrow(uniqueDates)){
      t1[[i]] = filter(occ1, months == as.list(uniqueDates)$years[[i]])
    }}
  
  if (dateScale == "day"){
    t1 <- NULL
    for (i in 1:nrow(uniqueDates)){
      t1[[i]] = filter(occ1, days == as.list(uniqueDates)$years[[i]])
    }}
  return(t1)
}

occ3<-parseDate(dateScale, occ1, uniqueDates)

###############################################################################
### Subset env/RS data into sub-stacks based on dates (here, using only year)  
# Using example dates (years only)
ydates<-lapply(uniqueDates, function(x) paste("Y", x, sep=''))
e1
###############################################################################
### For each occs sub-table, extract values from corresponding env/RS sub-stack
valExtract <- function(occ3, e1){
  occ4 <- lapply(occ3, function(x) x %>% dplyr::select(long, lat))
  vals <- mapply(raster::extract, unstack(e1), occ4)
  lowerBound <- min(unlist(vals))
  upperBound <- max(unlist(vals))
  return(as.data.frame(cbind(lowerBound, upperBound)))
}
Bounds <-valExtract(occ3, e1)

###############################################################################
############### Use bounds to set mask limits  ################################
## Load current env layer to use as mask since this is most recent data 
currentEnv<-e1[[8]]
currentEnv[currentEnv > Bounds$lowerBound] <- NA
currentEnv[currentEnv < Bounds$upperBound] <- NA

dataMask <- function(rasterMask, Bounds){
  rasterMask[rasterMask > Bounds$lowerBound] <- NA
  rasterMask[rasterMask < Bounds$upperBound] <- NA
  return(rasterMask)
}
dataMask(currentEnv, Bounds)


