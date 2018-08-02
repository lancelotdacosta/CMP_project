#setwd("~/CMP project/ABR08")

load("/Users/lancelotdacosta/Desktop/Data/Cell Count Data/Cellcounts_ABR_2007to2009.RData")

cc.data <- filter(ABR.raw.data, Year == 2008)


#clean_cell_count_data <- function(cc.data){} #cc.data is cell count data. Can be of many years and many sites
  

 #function 1 #needs a cell count dataframe

  if(length(cc.data$Site) == 1 & length(cc.data$Year) ==1){
  cc.data <- clean(cc.data)
  
  }else{
    
    list_of_dataframes <- vector("list", length(unique(cc.data$Site))*length(unique(cc.dataYear)))
    i <- 1 #some counter
      
    for(site in unique(cc.data$Site)){
      
      cc.site <- filter(cc.data, Site == site)
      
      for(year in unique(cc.data$Year)){
          
        list_of_dataframes[[i]] <- clean(filter(cc.site, Year == year))
        i <-  i +1
      }
    }
  cc.data <- rbind.fill(list_of_dataframes)
  }
  cc.data #outputs the cell count data cleaned

  
  
#clean() #inputs a dataframe
#Removes all the NAs from the first to the last sample
  
  
  
  


  

