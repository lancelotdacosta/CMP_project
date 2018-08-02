#2) Receives cell count data for 1 year & 1 site and returns clean cell count data with QC flag

#Input
#cell count data
cc <- cc.data
cc.data <- cc


#Function

#Make sure all trees have the same number of radial files in each sample, add rows for the missing ones

#list to store data
list.of.dataframes <- vector("list", length(unique(cc.data$Tree))*length(unique(cc.data$Sample)))
#a counter
i <-  1

for(tree in unique(cc.data$Tree)){
  #filter by tree
  cc.tree <- filter(cc.data, Tree == tree)
  for(sample in unique(cc.tree$Sample)){
    #filter by tree and sample
    cc.tree.sample <- filter(cc.tree, Sample == sample)
    #add a dataframe with tree sample and the number of radial files for that sample
    list.of.dataframes[[i]] <- data.frame(Tree = tree, Sample = sample, number_of_RF = length(unique(cc.tree.sample$RF)))
    #increment the counter
    i <-  i+1
  }
}
#missing_RF is a dataframe that contains all the Tree samples that are missing radial file measurements
missing_RF <- rbind.fill(Filter(Negate(is.null), list.of.dataframes))
missing_RF <- filter(missing_RF, number_of_RF < length(unique(cc.data$RF)))

#now we add observations to cell count data for the missing values if there are some missing
if(nrow(missing_RF)>0){
  #initialise the list again
  list.of.dataframes <- vector("list", 1 + nrow(missing_RF))
  #insert the first element of the list
  list.of.dataframes[[1]] <-  cc.data
  #and the counter
  i <- 2
  #
  cells_and_precision <- c("CZ", "EZ", "WZ", "MZ","PR")
  #
  for(obs in 1:nrow(missing_RF)){
    #prepares the missing meausrements that will be added to cc.data
    missing_measurements <- filter(cc.data, Tree == as.character(missing_RF$Tree[obs]) & Sample == missing_RF$Sample[obs])[1,]
    #makes sure we add the right amount of measurements
    missing_measurements <- missing_measurements[rep(1,each=length(unique(cc.data$RF))-missing_RF$number_of_RF[obs]),]
    #changes the relevant parameters in these measurements
    missing_measurements <- mutate(missing_measurements, RF = (missing_RF$number_of_RF[obs]+1):length(unique(cc.data$RF)))
    missing_measurements[,cells_and_precision] = NA
    #inserts in list of data frames
    list.of.dataframes[[i]] <- missing_measurements
    #increments the counter
    i <-  i+1
  }
}
#adds these observations to cell count data
cc.data <- rbind.fill(list.of.dataframes)



#Remove NA cell count values at each sample when not all values are NA

#Initialise a list where we will store data
list.of.dataframes <- vector("list", length(unique(cc.data$Tree))*length(unique(cc.data$Sample)))

#Initialise a list where we will store missing data
all.NA.list <- vector("list", length(unique(cc.data$Tree))*length(unique(cc.data$Sample))*4)

#Initialise two counters
i <- 1
j <- 1

#Initialise a vector of cells
cells <- c("CZ", "EZ", "WZ", "MZ")


for(tree in unique(cc.data$Tree)){
  #filters cell count data by tree
  cc.tree <- filter(cc.data, Tree == tree)
  #counts number of observations in this dataframe
  n <- nrow(cc.tree)

  #controls that not all cell count values for a certain tree are missing
  for(cell in cells){
    if(sum(is.na(cc.tree[,cell]))==n){
      warning(paste(c("Tree", as.character(tree), "has only NA", cell, "cell count values"), collapse = " "))
      stop(paste(c("Remove tree", as.character(tree), "from cell count data and restart"), collapse = " "))
    }
  }
  
  
  for(sample in unique(cc.tree$Sample)){
    #filters cell count data by tree and sample
    #adds a QC flag variable that controls quality of the data
    cc.tree.sample <- cc.tree %>% filter(Sample == sample) %>% mutate(QC_flag = 0)
    #counts number of observations in this dataframe
    m <- nrow(cc.tree.sample)
    
    for(cell in cells){
      #counts number of NAs for each cell
      number_of_NAs <- sum(is.na(cc.tree.sample[,cell]))
      
      if( number_of_NAs <m & number_of_NAs >0  ){
        #increments QC flag to indicate that data is going to be modified at these observations
        cc.tree.sample$QC_flag[is.na(cc.tree.sample[,cell])] <- cc.tree.sample$QC_flag[is.na(cc.tree.sample[,cell])] +1
        #replace missing cell count values by the mean of the non-missing ones
        cc.tree.sample[is.na(cc.tree.sample[,cell]),cell] <- mean(cc.tree.sample[,cell], na.rm = TRUE)
      }
      
      #if all cell count values for a certain cell type/tree/sample are missing
      if(number_of_NAs == m){
        #records where theseare in a dataframe and stores it in a list
        all.NA.list[[j]] <- data.frame(Tree = tree, Sample = sample, Cell_type = cell) 
        #increment the corresponding counter
        j <- j+1
      }
    }
    #store the (modified) cell count tree/sample data in the list
    list.of.dataframes[[i]] <- cc.tree.sample
    #increment the corresponding counter
    i <-  i+1
  }
}
#store missing data information in all.NA to fix later
all.NA <- rbind.fill(Filter(Negate(is.null), all.NA.list))
#store cell count data in cc.data
cc.data <- rbind.fill(list.of.dataframes)

#we replace the rest of the missing data (which we can find in all.NA) by linear interpolation

#loop on all the observations which we need to modify
for(row in 1:nrow(all.NA)){
 
  #which samples are available for the tree all.NA$Tree[row]
  samples_per_tree <- unique(filter(cc.data, Tree == as.character(all.NA$Tree[row]))$Sample)
  
  #the next available observation to interpolate on
  next_obs <- all.NA[row,]
  
  #set a condition that is true once we have found that next observation
  found_next_obs <- FALSE
  while(found_next_obs == FALSE){
    #increments sample by 1
    next_obs <- mutate(next_obs, Sample = Sample +1)
    
    #checks if next_obs is inside the cell-count data
    if(next_obs$Sample[1] %in% samples_per_tree){
      
      #if so, checks if next_obs is NOT inside all.NA
      if(nrow(match_df(next_obs, all.NA))==0){
        #we found the next observation
        found_next_obs <-  TRUE
      }
    }else{
      #check if next_obs is outside the sampling range of cell count data
      if(next_obs$Sample[1] > max(samples_per_tree)){
        #if so, set sample to NA
        next_obs <- mutate(next_obs, Sample = NA)
        found_next_obs <-  TRUE
      }
    }
  }
  
  #the previous available observation to interpolate on
  prev_obs <- all.NA[row,]
  
  #set a condition that is true once we have found that previous observation
  found_prev_obs <- FALSE
  while(found_prev_obs == FALSE){
    #increments sample by 1
    prev_obs <- mutate(prev_obs, Sample = Sample -1)
    
    #checks if next_obs is inside the cell-count data
    if(prev_obs$Sample[1] %in% samples_per_tree){
      
      #if so, checks if next_obs is NOT inside all.NA
      if(nrow(match_df(prev_obs, all.NA))==0){
        #we found the next observation
        found_prev_obs <-  TRUE
      }
    }else{
      #check if next_obs is outside the sampling range of cell count data
      if(next_obs$Sample[1] < min(samples_per_tree)){
        #if so, set sample to NA
        prev_obs <- mutate(prev_obs, Sample = NA)
        found_prev_obs <-  TRUE
      }
    }
  }

  #now that we have previous and next available sample we can interpolate
  
  #if both samples are available we do linear interpolation
  #if only one sample is available we copy that
  #if the two samples are unavailable we return an error
 
   s <- sum(is.na(c(next_obs$Sample[1], prev_obs$Sample[1])))
  
  #tests if both samples are available
  if(!is.na(next_obs$Sample[1]) & !is.na(prev_obs$Sample[1])){
    
    #day of the sample we need to modify
    day_of_sample <- filter(cc.data, Sample == all.NA$Sample[row])$DY[1]
    
    #day and cell count of the next available sample
    cc.next.sample <- filter(cc.data, Tree == as.character(next_obs$Tree[1]) & Sample == next_obs$Sample[1])
    day_next_sample <- cc.next.sample$DY[1]
    cc_next_sample_cell <-  select(cc.next.sample, as.character(next_obs$Cell_type[1]))
    
    #day and cell count of the previous available sample
    cc.prev.sample <- filter(cc.data, Tree == as.character(prev_obs$Tree[1]) & Sample == prev_obs$Sample[1])
    day_prev_sample <- cc.prev.sample$DY[1]
    cc_prev_sample_cell <-  select(cc.prev.sample, as.character(prev_obs$Cell_type[1]))
    
    #loop on radial files
    for(rf in unique(cc.data$RF)){
      
      #observation number of Tree/Sample/RF
      obs_no <- which(cc.data$Tree == as.character(all.NA$Tree[row]) &
                        cc.data$Sample == all.NA$Sample[row] &
                        cc.data$RF == rf)
      
      #linear interpolation of the element (Tree/Sample/RF,Cell)
      cc.data[obs_no, as.character(all.NA$Cell_type[row])] <- 
        (cc_prev_sample_cell[rf,1]*(day_of_sample - day_prev_sample) + cc_next_sample_cell[rf,1]*(day_of_sample - day_prev_sample))/(day_next_sample - day_prev_sample)
      
      #increment the QC flag to indicate that the data has been modified
      cc.data[obs_no, "QC_flag"] <- cc.data[obs_no, "QC_flag"] +1
    }
  }else{
    #Need to add a control that both samples are available
    stop("error of type 303")
  }
}

#Return clean cell count data
cc.data
