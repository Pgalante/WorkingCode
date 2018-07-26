library(tidyverse)
library(raster)
library(lubridate)
##############Global Wc to crop 
env<-stack(list.files(path = '/home/pgalante/Projects/rasterData/wc2-5', pattern = '\\.tif$', full.names = T))
## Give fake dates  -- for now using 2 years because thats what I have occ data for
years<-c(rep("2008", 10), rep("2009", 10))
for (i in 1:19){
  names(datedRasterStack[[i]])<- paste(names(datedRasterStack[[i]]), years[[i]], sep='')
}

#### Load and recognize Dates from files in R
dtab1<-read_csv('/home/pgalante/Projects/Git/WorkingCode/TestData.csv')
colnames(dtab1)<-c('x','y','date')
reDate <- function(dtab1){
  dtab1 %>% dplyr::select(x,y,date) %>%
  mutate(date = mdy_hm(date)) %>%
  mutate(years = year(date)) %>% 
  mutate(months = month(date)) %>%
  mutate(days = day(date))
}
dtab2<-reDate(dtab1)

## Crop global Wc to smaller area 
e<-extent(dtab1)
datedRasterStack<-crop(datedRasterStack, e)            

###########
## Pair occs with corresponding env 
# datedRasterStack must some how have the dates associated with the layers. 
#####  LUBRIDATE WAY  ######
#######  STILL NEED TO FIGURE OUT HOW TO DEFINE UNIQUE MONTH/YEAR/DAY COMBINATIONS 
library(lubridate)
dateScale<-"year"
occDates<-function(dtab1, dateScale){
  require(lubridate)
  
  if (dateScale == "year"){
    return(unique(dplyr::select(dtab2, years)))
  } 
  if (dateScale == "month"){
    return(unique(dplyr::select(dtab2, years, months)))
  } 
  if (dateScale == "day"){
    return(unique(dplyr::select(dtab2, years, months, days)))
  }
}
uniDates<-occDates(dtab1, dateScale = "year")
class(uniDates)

# #################### OLD WAY  ########################
# dateFormat<-"%m/%d/%Y %H:%M"
# RasDatesYear <- function(occDates, dateFormat, dateScale){
#   require(raster)
#   require(dplyr)
#   if (dateScale == "year"){
#   # Get unique years
#   return(unique(strptime(as.character(occDates$date), dateFormat)$year + 1900))
#   }
#   if (dateScale == "monthYear"){
#     return(unique(strptime(as.character(occDates$date), dateFormat)$mon + 1))
#   }
#   if (dateScale == "dayMonthYear"){
#     return(unique(strptime(as.character(occDates$date), dateFormat)$mday))
#   }
# }
# 
# test<-RasDatesYear(occDates, dateFormat, dateScale = "year")
# ##################  OLD WAY  #############################################


# Make a list of rasterStacks for each unique year
rasNames <- lapply(uniDates[[1]], function(x) datedRasterStack[[grep(x, names(datedRasterStack))]])
# Create different tibbles for each unique dateScale
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

test<-subOccs(dateScale = "year", dtab1 = dtab1, uniDates = uniDates)

# Match occs to dates and extract values





