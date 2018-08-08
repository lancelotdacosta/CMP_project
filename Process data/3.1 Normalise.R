#Normalise

#Input: interpolated cell count data


normalise <- function(cc.data){
  
  if(sum(c("CZ", "EZ", "WZ", "MZ") %in% names(cc.data)) != 4){
    stop("Error in normalise function: data does not contain either CZ, EZ, WZ or MZ")
  }
  if("DY" %in% names(cc.data) == FALSE){
    stop("Error in normalise function: data does not contain DY (day of year)")
  }
  
  list.of.dataframes <- vector("list", length(unique(cc.data$Tree))*length(unique(cc.data$RF)))
  i <- 1
  
  for(tree in unique(cc.data$Tree)){
    cc.tree <- filter(cc.data, Tree == tree)
    for(rf in unique(cc.tree$RF)){
      cc.tree.rf <- filter(cc.tree, RF == rf)
      #critical day: first day such that EZ count is always zero after that
      cc.before.critical.day <- filter(cc.tree.rf, EZ == 0)
      critical_day <- max(cc.before.critical.day$DY)
      
      while((critical_day-1) %in% cc.before.critical.day$DY){
        critical_day <- critical_day - 1
      }
      
      if(critical_day < max(cc.tree.rf$DY)){
        
        cc.critical.day <- filter(cc.tree.rf, DY == critical_day)
        WZ_critical_day <- cc.critical.day$WZ[1]
        MZ_critical_day <- cc.critical.day$MZ[1]
        
        for(day in (critical_day+1):max(cc.tree.rf$DY)){
          
          if(day %in% cc.tree.rf$DY == FALSE){
            warning("Error in normalise function")
            stop(paste(c("Tree ", as.character(tree), ", radial file ", as.character(rf),
                   " is missing data for day ", as.character(day)),collapse = ""))
          }
          
          index <- which(cc.tree.rf$DY==day)
          cc.tree.rf$MZ[index] <- MZ_critical_day + (WZ_critical_day - cc.tree.rf$WZ[index])
        }
      }else{
        warning(paste("Critical day is ", as.character(day)))
      }
      
      list.of.dataframes[[i]] <- cc.tree.rf
      i <- i+1
      
    }
  }
  
  cc.data <- rbind.fill(Filter(Negate(is.null), list.of.dataframes))
  cc.data
}
