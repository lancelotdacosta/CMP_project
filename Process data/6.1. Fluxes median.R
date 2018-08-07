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
      
      #new data
      new.data <- flux.tree.day[1,]
      
      #change flux data by the medians
      new.data[, "CZ"] <- median(flux.tree.day[, "CZ"])
      new.data[, "EZ"] <- median(flux.tree.day[, "EZ"])
      new.data[, "WZ"] <- median(flux.tree.day[, "WZ"])
      new.data[, "MZ"] <- median(flux.tree.day[, "MZ"])
      
      new.data[, "toCZ"] <- median(flux.tree.day[, "toCZ"])
      new.data[, "CZtoEZ"] <- median(flux.tree.day[, "CZtoEZ"])
      new.data[, "EZtoWZ"] <- median(flux.tree.day[, "EZtoWZ"])
      new.data[, "WZtoMZ"] <- median(flux.tree.day[, "WZtoMZ"])
      new.data[, "PR"] <- median(flux.tree.day[, "PR"])
      
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
