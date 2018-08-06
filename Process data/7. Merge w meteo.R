#merge cc data/flux data with meteo data in a big dataframe

#Input
# - cc data/flux data
# - meteo data


#Function
merge.with.meteo.data <- function(cc.flux.data, meteo.data){
  
  if(nrow(cc.flux.data) %% nrow(meteo.data) !=0){ #remainder of euclidean division
    stop("Error 101 in merge function")
  }
  
  if(!identical(unique(as.numeric(cc.flux.data$DY)),unique(as.numeric(meteo.data$jour)))){ #same number of days?
    stop("Error 102 in merge function")
  }
  
  if(!identical(unique(as.numeric(cc.flux.data$Year)),unique(as.numeric(meteo.data$annee)))){ #same years?
    stop("Error 103 in merge function")
  }
  
  n <- nrow(cc.flux.data) / nrow(meteo.data)
  #duplicate n times number of observations of meteorological data
  meteo.data <- meteo.data[rep(1:nrow(meteo.data), each = n),]
  
  #list of dataframes
  list.of.dataframes <- vector("list", length(unique(cc.flux.data$DY)))
  #counter
  i <- 1
  for(year in unique(cc.flux.data$Year)){
    for(day in unique(cc.flux.data$DY)){
      #filter by day and year
      meteo.day <- filter(meteo.data, annee == year & jour == day)
      cc.flux.day <- filter(cc.flux.data, Year == year &DY ==day)
      #insert new data in list of dataframes
      list.of.dataframes[[i]] <- cbind(meteo.day, cc.flux.day)
      #increment counter
      i <-  i+1
    }
  }
  #put everything in a big dataframe
  df <- rbind.fill(list.of.dataframes)
  #outputs dataframe
  df
}
