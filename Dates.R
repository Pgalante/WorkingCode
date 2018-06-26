#### Load and recognize Dates from files in R
dtab1<-read.csv('/home/pjg/Git/WorkingCode/TestData.csv')
occDates<-as.data.frame(cbind(dtab1$lon1, dtab1$lat1, as.character(dtab1$utc)))
#d1<-strptime(as.character(dtab1$utc), "%m/%d/%Y %H:%M")

## Pair occs with corresponding env 
# datedRasterStack must some how have the dates associated with the layers. 
RasDates<-function(occDates, datedRasterStack){
  # match layers with appropriate env data
  
}

timeMaskr<-function(rasterStack, occDates){
  ## Extract values and set mask layer
  
}