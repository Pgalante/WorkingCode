library(tidyverse)
##############Global Wc to crop 
datedRasterStack<-stack(list.files(path = '/home/pjg/worldClim/wc2-5', pattern = '\\.tif$', full.names = T))
## Give fake dates  -- for now using 2 years because thats what I have occ data for
years<-c(rep("2008", 10), rep("2009", 10))
for (i in 1:19){
  names(datedRasterStack[[i]])<- paste(names(datedRasterStack[[i]]), years[[i]], sep='')
}

#### Load and recognize Dates from files in R
dtab1<-read_csv('/home/pjg/Git/WorkingCode/TestData.csv')
colnames(dtab1)<-c('x','y','date')
## Crop global Wc to smaller area 
e<-extent(dtab1)
datedRasterStack<-crop(datedRasterStack, e)            


###########
## Pair occs with corresponding env 
# datedRasterStack must some how have the dates associated with the layers. 
#####  LUBRIDATE WAY  ######
#######  STILL NEED TO FIGURE OUT HOW TO DEFINE UNIQUE MONTH/YEAR/DAY COMBINATIONS 
library(lubridate)
dateScale<-"month"
occDates<-function(dtab1, dateScale){
  require(lubridate)
  d<-lapply(dtab1$date, mdy_hm)
  d.df <- tbl_df(cbind(lapply(d, year), lapply(d, month), lapply(d, day)))
  colnames(d.df) <- c("year", "month", "day")
  if (dateScale == "year"){
    return(unique(select(d.df, year)))
  } 
  if (dateScale == "month"){
    return(unique(select(d.df, year, month)))
  } 
  if (dateScale == "day"){
    return(unique(select(d.df, year, month, day)))
  }
}
test<-occDates(dtab1, dateScale = "year")

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
rasNames <- lapply(test[[1]], function(x) datedRasterStack[[grep(x, names(datedRasterStack))]])
# Create different tibbles for each unique dateScale
subOccs <- function(dateScale, dtab1, test){
  if (dateScale == "year"){
    dtab2 <- dtab1 %>% mutate(date = mdy_hm(date)) %>%
      lapply(distinct(year(date)),  print(x))
            

    
    
     d<-add_column(dtab1, unlist(lapply(lapply(dtab1$date, mdy_hm), year))
    colnames(d)<-c("lon", "lat", "utc", "dates")
    return(
d[d$dates      
  }
}


subOccs <- function(dateScale, occDates, test){
  if (dateScale == "year"){
    return(lapply(test, function (x) occDates[strptime(occDates$date, dateFormat)$year+1900 == test,]))
  }
}
test2 <- subOccs(dateScale, occDates, test)



# Match occs to dates and extract values
datedValues<-function(test2, rasNames){

mapply(raster::extract, rasNames, as.matrix(test2)[,1:2])

raster::extract(rasNames[[1]], as.data.frame(cbind(as.matrix(test2[[1]]$lon), matrix(test2[[1]]$lat))))

plot(rasNames[[1]][[1]])
points(as.data.frame(cbind(as.matrix(test2[[1]]$lon), matrix(test2[[1]]$lat))))
