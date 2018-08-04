#receive cell count data and linear interpolate to get daily cell count data throughtout the year

#Input
#cell count data
cc <- cc.data
cc.data <-  cc

#number of days in the year
days_in_year <- 366

#Function

#1) Add observations for each tree at each day

#We assume that all trees in cc.data have the same number of RF
#We assume the cell count data contains no NAs
#This holds if we applied the clean function before
#Algorithm: for each day of the year and for each tree, do we have cell count data?
#If yes do nothing. If not add the relevant observations with interpolated cell count data

#trees
Trees <-  unique(cc.data$Tree)
#radial files
RFs <- unique(cc.data$RF)
#initialise a list to store data
list.of.dataframes <-  vector("list", 1 + days_in_year*length(Trees))
#first element of list
list.of.dataframes[[1]] <-  cc.data
#initailise a counter
i <- 2
#loop on days of the year and trees
for(day in 1:days_in_year){
  for(tree in Trees){
    #do we have cell count data for that day and that tree?
    if(nrow(filter(cc.data, DY == day & Tree == tree))==0){
      #If not:
      #subsets data by tree
      cc.tree <- filter(cc.data, Tree == tree)
      #create new dataframe of missing cell count data that will be added to cc.data at the end
      new.data <- cc.data[rep(1, each= length(RFs)),]
      #we need to change the tree number, species, sample number (to zero indicating that this is not part of the original dataset)
      #the day of year, RF, PR
      #precision (PR) is set to NA
      tree_species <- cc.tree$Species[1]
      new.data <- mutate(new.data, Species = tree_species, Tree = tree, Sample = 0,
                         DY = day, RF = RFs, PR=NA)
      #we linear interpolate the cell count data
      #list of days this tree was sampled
      days_this_tree_was_sampled <- unique(cc.tree$DY)
      #first and last day of sampling
      day_first_sample <- min(days_this_tree_was_sampled)
      day_last_sample <- max(days_this_tree_was_sampled)
      #cells to interpolate
      cells <-  c("CZ", "EZ", "WZ", "MZ")
      #if day is before first day of sampling we copy cell count data from the first sample
      if(day < day_first_sample){
        #cell count data of first sample
        new.cc.data <- filter(cc.tree, DY == day_first_sample)[,cells]
        #for later in QC_flqg
        day_prev_sample <- day_first_sample
        day_next_sample <- day_first_sample
      }
      #if day is after last day of sampling we copy cell count data from the last sample
      if(day > day_last_sample){
        #cell count data of last sample
        new.cc.data <- filter(cc.tree, DY == day_last_sample)[,cells]
        #for later in QC_flag
        day_prev_sample <- day_first_sample
        day_next_sample <- day_first_sample
      }
      if(day == day_last_sample | day == day_first_sample){
        #this shouldn't happen but just in case
        stop("Error 101 in linear interpolate function")
      }
      if(day < day_last_sample & day > day_first_sample){
        #if this happens need to linear interpolate from previous and next sample
        #day of previous available sample for that tree
        day_prev_sample <- max(filter(cc.tree, DY < day)$DY)
        #day of next available sample for that tree
        day_next_sample <- min(filter(cc.tree, DY > day)$DY)
        #cell count data previous sample
        prev.cc.data <- filter(cc.tree, DY == day_prev_sample)[, cells]
        #cell count data next sample
        next.cc.data <- filter(cc.tree, DY == day_next_sample)[, cells]
        #linear interpolated cell count data
        new.cc.data <- (prev.cc.data*(day - day_prev_sample) +next.cc.data*(day_next_sample - day))/(day_next_sample - day_prev_sample)
      }
      #insert cell count data into new.data
      new.data <- mutate(new.data, CZ = new.cc.data[,"CZ"], EZ = new.cc.data[,"EZ"], WZ = new.cc.data[,"WZ"], MZ = new.cc.data[,"MZ"])

      #checks if QC_flag is a variable and sets it as the mean of the QC_flags we interpolated it on +1
      if("QC_flag" %in% names(new.data)){
        #the mean of the QC_flqgs +1
        new_QC_flag <- (filter(cc.tree, DY == day_next_sample)$QC_flag + filter(cc.tree, DY == day_prev_sample)$QC_flag)/2 +1
        #changes the QC_flag
        new.data <- mutate(new.data, QC_flag = new_QC_flag)
      }
      #inserts new data in the list of dataframes
      list.of.dataframes[[i]] <- new.data
      #increments counter
      i <-  i+1
    }
  }
}

#puts all data back in cell count data
cc.data <- rbind.fill(Filter(Negate(is.null), list.of.dataframes))
#orders cell count data by day for aesthetic reasons
cc.data <- cc.data[order(cc.data$DY),]

#Returns cell count data
cc.data

