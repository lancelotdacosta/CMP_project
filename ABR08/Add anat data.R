#1) Receives cell count data for a certain site & year + anatomical data
#and adds anatomical data as another sample

#packages
library(plyr) #for rbind.fill
library(dplyr)

#setup
#load cell count data
load("/Users/lancelotdacosta/Desktop/Data/Cell Count Data/Cellcounts_ABR_2007to2009.RData")
cc.data <- filter(ABR.raw.data, Year == 2008)
#load anatomical data
anat.data <- read.csv(file = "/Users/lancelotdacosta/Desktop/Data/Anatomical data/TDG_ABR2008_Raw.csv", sep = ";")
#parameter (7 by default) number of days after last sample at which to add anatomical data
sampling_period <- 7 #days



#function

#Checks that anatomical data has the variables we need. If not returns error message
if(sum(c('Year', 'Tree', 'PathName', 'CellRank') %in% names(anat.data)) !=4) {
  stop("Anatomical data does not contain variable 'Year' or 'Tree' or 'PathName' or 'CellRank'. Please add them and perform operation again.")
  
}

#Selects the variables we need from the anatomical data
anat.data.select <- select(anat.data, Year, Tree, PathName, CellRank)

#Creates a list that we will use to store data
list.of.dataframes <- vector("list", length(unique(cc.data$Tree))+1)

list.of.dataframes[[1]] <- cc.data

#starts a counter
i <-  2

for(tree in unique(cc.data$Tree)){
  
  #Checks that we have anatomical data for that tree
  if(tree %in% unique(anat.data.select$Tree)){
    
    #filters anatomical data by tree
    anat.data.tree <- filter(anat.data.select, Tree == tree)
    
    #record mean number of mature cells in anatomical data for that tree
    vector <- seq(0,0,length.out= length(unique(anat.data.tree$PathName)))
    j <- 1 # a counter
    for(p_name in unique(anat.data.tree$PathName)){
      #find the number of mature cells in that PathName and insert it in the vector
      vector[j] <- max(filter(anat.data.tree, PathName == p_name)$CellRank)
      j <- j +1 #increase counter to next position
    }
    mature_cells_tree <- mean(vector, na.rm = TRUE)
    
    #create a new cell count data sample consisting of anatomical data and store it in list.of.dataframes
    #Note: we assume that there is the same number of RF in each sample in each tree
    new.sample <- cc.data %>% filter(Tree == tree)
    new.sample <- new.sample %>% filter(Sample == max(new.sample$Sample))
    
    new.sample <- new.sample %>% mutate(Sample = Sample +1, DY = DY + sampling_period, EZ =0, WZ =0, MZ = mature_cells_tree, PR = 0) #Precision is set to zero for now
    
    list.of.dataframes[[i]] <-  new.sample #put sample in the list
    i <-  i+1 #increase counter
    
  }else{
    #If we don't have anatomical data for that tree, we return a message.
    message(paste("Anatomical data not available for tree", as.character(tree)))
  }
  
}

#Remove NULL values corresponding to trees missing in anatomical data
list.of.dataframes <- Filter(Negate(is.null), list.of.dataframes)

#Update cell count data with the new samples from anatomical data
cc.data <- rbind.fill(list.of.dataframes)

#Return updated cell count data
cc.data