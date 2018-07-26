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
##################  Metadata assignment? Might be better to name rasters as dates  ######################
## assign metadata as yyyy/mm/dd for raster stack (this follows as.Date convention)
dates<-list(sample(seq(as.Date('2005/01/01'), as.Date('2016/01/01'), by="day"), 19))
# Figure out a way for lapply to work here?
for (i in 1:nlayers(e1)){
  metadata(e1[[i]])<-as.list(dates[[1]][[i]])
}


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
    return(unique(dplyr::select(occ1, years)))
  } 
  if (dateScale == "month"){
    return(unique(dplyr::select(occ1, years, months)))
  } 
  if (dateScale == "day"){
    return(unique(dplyr::select(occ1, years, months, days)))
  }
}
uniqueDates<- uniDates(occ1, dateScale)
##  Last, for each unique date, create different tibbles
parseDate <- function(dateScale, occ1, uniqueDates){
  t1<-NULL
  if (dateScale == "year"){
    for (i in 1:length(unique(occ1$years))){
      t1[[i]] = filter(occ1, years == as.list(unique(occ1$years))[[i]])
    }}
  #### NEED TO FIGURE OUT YEAR MONTH DAY COMBINATIONS
  if (dateScale == "month"){
    t1<-NULL
    for (i in 1:length(unique(occ1$years))){
      t1[[i]] = filter(occ1, months == as.list(unique(occ1$months))[[i]])
    }}
  
  if (dateScale == "day"){
    t1<-NULL
    for (i in 1:length(unique(occ1$days))){
      t1[[i]] = filter(occ1, days == as.list(unique(occ1$days))[[i]])
    }}
  return(t1)
}

occ3<-parseDate(dateScale, occ1, uniqueDates)

###############################################################################
### Subset env/RS data into sub-stacks based on dates (here, using only year)  

###############################################################################
### For each occs sub-table, extract values from corresponding env/RS sub-stack

###############################################################################
### Reappend extracted values and get min/max  ################################






## Capture min / max values of occurrences for each layer
# Get years
ys<-NULL
for (i in 1:nlayers(e1)){
  ys[[i]]<-year(as.Date(metadata(e1[[i]])[[1]]))
}
# Extract values for each year?
subOccs <- function(dateScale, dtab1, uniDates){
  t1<-NULL
  if (dateScale == "year"){
    for (i in 1:length(unique(dtab2$years))){
      t1[[i]] = filter(dtab2, years == as.list(unique(dtab2$years))[[i]])
    }}
  #### NEED TO FIGURE OUT YEAR MONTH DAY COMBINATIONS
  if (dateScale == "month"){
    t1<-NULL
    for (i in 1:length(unique(dtab2$years))){
      t1[[i]] = filter(dtab2, months == as.list(unique(dtab2$months))[[i]])
    }}
  
  if (dateScale == "day"){
    t1<-NULL
    for (i in 1:length(unique(dtab2$days))){
      t1[[i]] = filter(dtab2, days == as.list(unique(dtab2$days))[[i]])
    }}
  return(t1)
}

test<-subOccs(dateScale = "year", dtab1 = occ, uniDates = uniDates)




