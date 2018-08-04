#5. Changes cell count data by fluxes

#input
#cell count data
cc <- cc.data
cc.data <-  cc

#Function

#list to store data
list_of_dataframes <- vector("list", length(unique(cc.data$Tree))*length(unique(cc.data$RF))) #we again assume that all trees are always sampled with the same amount of radial files
#a counter
i <- 1
#loops on trees and radial files
for(tree in unique(cc.data$Tree)) {
  for(rf in unique(cc.data$RF)) {
    #filters by tree and radial file
    flux_tree_rf <- filter(cc.data, Tree == tree, RF ==rf) 
    
    #We copy cell count data. We use which() instead of select() so that the output is a vector not a dataframe
    CZ <- flux_tree_rf[, "CZ"]
    EZ <- flux_tree_rf[, "EZ"]
    WZ <- flux_tree_rf[, "WZ"]
    MZ <- flux_tree_rf[, "MZ")]
    
    #computes the change of XZ at each day of the year and stores it in Delta_XZ
    CZ_copy <- CZ
    EZ_copy <- EZ
    WZ_copy <- WZ
    MZ_copy <- MZ
    
    CZ <- CZ[2:length(CZ)] #removes the first element of the vector
    EZ <- EZ[2:length(EZ)]
    WZ <- WZ[2:length(WZ)]
    MZ <- MZ[2:length(MZ)]
    
    CZ <- c(CZ, CZ[length(CZ)]) #adds a copy of the last element at the end of the vector
    EZ <- c(EZ, EZ[length(EZ)])
    WZ <- c(WZ, WZ[length(WZ)])
    MZ <- c(MZ, MZ[length(MZ)])
    
    CZ_copy <- CZ_copy[1:(length(CZ_copy)-1)] #removes the last element of the vector
    EZ_copy <- EZ_copy[1:(length(EZ_copy)-1)]
    WZ_copy <- WZ_copy[1:(length(WZ_copy)-1)]
    MZ_copy <- MZ_copy[1:(length(MZ_copy)-1)]
    
    CZ_copy <- c(CZ_copy[1], CZ_copy) #adds a copy of the first element at the beginning of the vector
    EZ_copy <- c(EZ_copy[1], EZ_copy)
    WZ_copy <- c(WZ_copy[1], WZ_copy)
    MZ_copy <- c(MZ_copy[1], MZ_copy)
    
    #this is the change in XZ cells at each day of the year
    Delta_CZ <- (CZ - CZ_copy)/2
    Delta_EZ <- (EZ - EZ_copy)/2
    Delta_WZ <- (WZ - WZ_copy)/2
    Delta_MZ <- (MZ - MZ_copy)/2
    
    flux_tree_rf <- mutate(flux_tree_rf, #changes the data frame to be the dataframe of fluxes with Tree tree and RF rf
                           CZ = Delta_CZ + Delta_EZ +Delta_WZ +Delta_MZ, #the fluxes follow these mathematical equations
                           EZ = Delta_EZ +Delta_WZ +Delta_MZ,
                           WZ = Delta_WZ + Delta_MZ,
                           MZ = Delta_MZ)
    #put in list
    list_of_dataframes[[i]] <- flux_tree_rf
    #increment counter
    i <- i +1
  }
}

#puts everything in flux.data
flux.data <- rbind.fill(list_of_dataframes)

#changes names of the columns
names(flux.data)[names(flux.data)=="CZ"] <- "toCZ"
names(flux.data)[names(flux.data)=="EZ"] <- "CZtoEZ"
names(flux.data)[names(flux.data)=="WZ"] <- "EZtoWZ"
names(flux.data)[names(flux.data)=="MZ"] <- "WZtoMZ"

#return flux.data
flux.data