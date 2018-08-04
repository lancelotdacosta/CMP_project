#Libraries
library(plyr) #for rbind.fill
library(dplyr)
library(readxl)
library(purrr)
library(suncalc)

#Inputs
#1)
#cell count data for one site & one year
#anat data
#number of days after last sample at which to add anatomical data
#2)
#cc data
#3)
#cc data
#4)
#cc.data
#path to cell count excel file
#5)
#cc data
#6)
#flux data
#7)
#cc/flux data
#meteo data


#cell count data for one site & one year
load("/Users/lancelotdacosta/Desktop/Data/Cell Count Data/Cellcounts_ABR_2007to2009.RData")
cc.data <- filter(ABR.raw.data, Year == 2007)
#anatomical data
anat.data <- read.csv(file = "/Users/lancelotdacosta/Desktop/Data/Anatomical data/TDG_ABR2007_Raw.csv", sep = ";")
#path to cell count excel file
path <- "/Users/lancelotdacosta/Desktop/Summer project/data/Cellular_resolution_France_Vosgues copy/Donon Data/Cell Count Data/ABR2007 Cell Count - 2014-12-18.xlsx"
#meteo data
meteo.data <- read.delim("/Users/lancelotdacosta/Desktop/Data/Meteorological data/DonneesMeteoJournalieres_Abreschviller_AE.txt")
meteo.data <- filter(meteo.data, annee == 2007)

#
cc.data <- add.anat.data(cc.data, anat.data)
cc.data <- clean.cc.data(cc.data)
cc.data <- linear.interpolate.cc.data(cc.data)
cc.data <- add.new.variables(cc.data, path)
cc.data <- fluxes.cc.data(cc.data)
cc.data <- flux.mean.cc.data(cc.data)
cc.data <- merge.with.meteo.data(cc.data, meteo.data)
ABR07 <- cc.data

save(ABR07, file = "ABR07.RData")
