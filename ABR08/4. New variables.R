#4. add day length, tree size, age and diameter

#Packages
library(readxl)
library(purrr)
library(suncalc)

#Input
#cell count data
cc <- cc.data
cc.data <- cc
#cell count excel file
path <- "/Users/lancelotdacosta/Desktop/Summer project/data/Cellular_resolution_France_Vosgues copy/Donon Data/Cell Count Data/ABR2008 Cell Count - 2014-12-03.xlsx"
#at which year were the tree ages estimated: default 2010 for ABR
year_estimated <-  2010


#Function

#1) we add age, diameter & height
#2) we add day length
#3) we add altitude
#as new variables to our cell count data

#1)
#extracts raw cell count data 
cc.data.raw <- path %>%
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

#extracts tree description of site
Tree_description <- cc.data.raw$`Tree description`
#selects the variables we will insert in the dataframe
Tree_age_dimensions <- select(Tree_description, Tree, Age, Diameter, Height)

#starts a list where we store data
list_of_dataframes <- vector("list", length(unique(cc.data$Tree)))
#starts a counter
i <- 1

for(tree in unique(cc.data$Tree)){
  #filters to get the interesting variables for the tree
  temp1 <- filter(Tree_age_dimensions, Tree ==tree)
  #add the new variables to temp2
  temp2 <- cc.data %>%
    filter(Tree == tree) %>%
    mutate(Age = temp1$Age[1] - (year_estimated - unique(cc.data$Year)[1]), #removes something to the ages as they were estimated in 2010 -> date sensitive!!!!!!
           Diameter= temp1$Diameter[1],
           Height= temp1$Height[1])
  #store in list
  list_of_dataframes[[i]] <- temp2
  #increment counter
  i <- i+1
}
#add to cell count data
cc.data <- rbind.fill(list_of_dataframes)

#2)

#gets latitude and longitude of the site
latitude <- as.numeric(filter(cc.data.raw$`General description`, Investigators == "Latitude")[1,2])
longitude <- as.numeric(filter(cc.data.raw$`General description`, Investigators == "Longitude")[1,2])

#list to store data
list_of_dataframes <- vector("list", length(unique(cc.data$annee))*length(unique(cc.data$jour)))
# a counter
i <- 1

#works with data from different years
for(year in unique(cc.data$Year)){
  
  #last day of the previous year
  start_date <- paste(c(as.character(year-1), "-12-31"), collapse= "")
  
  #loop on days
  for(day in unique(cc.data$DY)){
    
    #filter by year and day
    temp <- filter(cc.data, Year == year, DY == day)
    
    #Date of the day
    Date_of_day <- as.Date(day, origin = start_date)
    
    #Get length of the day
    sunrise_sunset <- getSunlightTimes(date = Date_of_day, lat = latitude, lon= longitude, keep = c("sunrise", "sunset"))
    Length_of_day <- as.numeric(difftime(sunrise_sunset$sunset, sunrise_sunset$sunrise), units="hours")
    
    #Add length of day variables to temp
    temp <- mutate(temp, daylength = Length_of_day)
    
    #Add temp to our list of dataframes
    list_of_dataframes[[i]] <- temp 
    #increment counter
    i <- i+1
  }
}

#merge our dataframes
cc.data <- rbind.fill(list_of_dataframes)


#3)
#Add a variable for altitude
cc.data <- mutate(cc.data, 
             Altitude = as.numeric(filter(cc.data.raw$`General description`, Investigators == "Altitude (m a.s.l.)")[1,2]))

#return updated cell count data
cc.data
