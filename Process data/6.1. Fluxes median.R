#6. Take the median of the fluxes

#Input
# - flux data


#Function
flux.median.cc.data <- function(flux.data){
  #a list to store data
  list.of.dataframes <- vector("list", length(unique(flux.data$Tree))*length(unique(flux.data$DY)))
  #a counter
  i <- 1
  
  #loops on trees and days
  for(tree in unique(flux.data$Tree)) {
    for(day in unique(flux.data$DY)) {
      
      #filters data by tree and day
      flux.tree.day <- filter(flux.data, Tree == tree, DY == day)
      
      #take different fluxes and precision
      flux1 <- flux.tree.day[, "toCZ"]
      flux2 <- flux.tree.day[, "CZtoEZ"]
      flux3 <- flux.tree.day[, "EZtoWZ"]
      flux4 <- flux.tree.day[, "WZtoMZ"]
      precision <- flux.tree.day[, "PR"]
      
      #new data
      new.data <- flux.tree.day[1,]
      
      #change flux data by the medians
      new.data[, "toCZ"] <- median(flux1)
      new.data[, "CZtoEZ"] <- median(flux2)
      new.data[, "EZtoWZ"] <- median(flux3)
      new.data[, "WZtoMZ"] <- median(flux4)
      new.data[, "PR"] <- median(precision)
      
      #insert in list
      list.of.dataframes[[i]] <- new.data
      #increment counter
      i <- i +1
    }
  }
  
  #insert in flux.data
  flux.data <- rbind.fill(list.of.dataframes)
  
  #output flux.data
  flux.data
}
