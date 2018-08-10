#Libraries
library(plyr) #for rbind.fill
library(dplyr)
library(readxl)
library(purrr)
library(suncalc)

test <- Donon.data

names <- c("Abreschviller_AE.txt", "Grandfontaine.txt", "Walscheid.txt")
Donon.data <- Donon.cc.data.clean
list.of.dataframes <- vector("list", 9)
i <- 1
j <- 1
for(site in unique(Donon.data$Site)){
  for(year in unique(Donon.data$Year)){
    cc.data <- filter(Donon.data, Site == site & Year == year)
    meteo.data <- read.delim(paste(c("/Users/lancelotdacosta/Desktop/Data/Meteorological data/DonneesMeteoJournalieres_", names[j]), collapse = ""))
    meteo.data <- filter(meteo.data, annee == year)
    cc.data <- merge.with.meteo.data(cc.data, meteo.data)
    message(i)
    list.of.dataframes[[i]] <- cc.data
    i <- i+1
  }
  j <- j+1
}
Donon.data <- rbind.fill(list.of.dataframes)

save(Donon.data, file = "Donon.fluxes.wmeteo.current.RData")


#rm
rm(list = setdiff(ls(), lsf.str()))