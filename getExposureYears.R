# load("C:/Users/pc/Dropbox/rangeHorizonsData/Andreas_data/Overshoot/rawResults/Marine/MarineReptiles/MarineReptiles_CanESM5.rda")
# 
# x <- as.data.frame(fastDoCall(rbind, rawResults))
# 
# rm("rawResults"); gc()
# 
# cell.id <- sample(x$WorldID, 1)
# cell.id <- 26232
# 
# data <- x[x$WorldID == cell.id,]
# data <- na.omit(data)
# data <- data[,-c(1,2)]
# data[,280:294]

getExposureYears <- function(data){
  
  require(dplyr)
  
  # identify species origin

  tmp <- rle(as.numeric(data)) # remove first argument, which indicates if the species is native (1) or not (0)
  tmp <- data.frame(time=tmp$lengths, values=tmp$values, year = cumsum(tmp$lengths)+2006) # year is the first year after the change in suitability
  
  # sum(tmp[tmp$values==1,]$time)
  
  exposureTimes <- tmp[tmp$time>=5,]
  exposureTimes
  
# if there is only one event in exposureTimes, and this event is 1
# this means that the species was never exposed. Hence, function returns a NA
  
  if(length(unique(exposureTimes$values))==1){
    if(unique(exposureTimes$values) == 1) return(NA)
    }
  
  # if there is only one event in exposureTimes, and the duration of the event is less than 5 years, this means that the species was never exposed, regardless of the event (0 or 1). Hence, function returns a NA
  
  if(length(unique(exposureTimes$values))==0) return(NA)
 
  
# # if the origin is the same as the first event (e.g. 0 and 0) 
# # the function excludes the first line
#   while(origin == exposureTimes$values[1]){
#     exposureTimes <- exposureTimes[-1,]  
#   }

  exposureTimes$year <- exposureTimes$year - exposureTimes$time
  
  results <- exposureTimes
  

  

  # if(results$values[1]==1) results <- results[-1,]
  
  if(nrow(exposureTimes) ==  1)  return(results) else 
    
    for(i in 1:(nrow(exposureTimes)-1)){
      
      if(exposureTimes$values[i]==exposureTimes$values[i+1]) results[i+1,] <- NA
      
    }
  
  # if the first rows of the results has a value of 1, it means that the
  # event is unexposure. But the species already occurs in that cell, so the first
  # event must be an exposure. # therefore, I am excluding this first event
  
  if(results$values[1]==1) results <- results[-1,]  
  
  return(na.omit(results))
}

