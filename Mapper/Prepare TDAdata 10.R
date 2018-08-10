merge.with.method.meteo <- function(Donon.data, days = 1, method = "mean"){
  
names <- c("Abreschviller_AE.txt", "Grandfontaine.txt", "Walscheid.txt")
locations <- c("loc_Abreschviller_430m.RData", "loc_Grandfontaine_650m.RData", "loc_Walscheid_370m.RData")
locations_path <- "/Users/lancelotdacosta/Desktop/Data/New Meteorological data/"
load(paste(c(locations_path, locations[1]),collapse = ""))
load(paste(c(locations_path, locations[2]),collapse = ""))
load(paste(c(locations_path, locations[3]),collapse = ""))
annemarie.data <- list(loc_Abreschviller_430m, loc_Grandfontaine_650m, loc_Walscheid_370m)

list.of.dataframes <- vector("list", 9)
i <- 1
j <- 1

for(site in unique(Donon.data$Site)){
  
  meteo.data <- read.delim(paste(c("/Users/lancelotdacosta/Desktop/Data/Meteorological data/DonneesMeteoJournalieres_", names[j]), collapse = ""))
  meteo.data <- cbind(meteo.data, data.frame(SWpotential = annemarie.data[[j]]$SWpotential))

  method.meteo.data <- method.meteorology.data(meteo.data, days =days, method = method)
  
  for(year in unique(Donon.data$Year)){
    cc.data <- filter(Donon.data, Site == site & Year == year)
    meteo.year <- filter(method.meteo.data, annee == year)
    cc.data <- merge.with.meteo.data(cc.data, meteo.year)
    message(i)
    list.of.dataframes[[i]] <- cc.data
    i <- i+1
  }
  j <- j+1
}

TDAdata <- rbind.fill(list.of.dataframes)

TDAdata
}


method.meteorology.data <- function(data, days, method = "mean"){
  
  #data$tmax <- as.numeric(as.character(data$tmax))
  
  avgdata <- data
  f <- get(method)
  
  variables_to_method <- c("vent", "pluie", "tsec", "hum", "rgl", "tmin", "tmax", "SWpotential")
  
  for(r in 2:(nrow(avgdata))){
    
    if(r <= days) {
      for(c in variables_to_method) {
        avgdata[r,c] = f(data[1:r,c])
      }
    } else{
      for(c in variables_to_method) {
        avgdata[r,c] = f(data[(r-days+1):r,c])}
    }
  }
  avgdata
}



#moment of truth
load("~/Desktop/CMP_project/Process data/Donon.fluxes.current.RData")
days <- 18
method <- "mean"

TDAdata <- merge.with.method.meteo(Donon.data, days = days, method = method)

save(TDAdata, file = paste(c("TDA", as.character(days), method, ".RData"), collapse =""))


###
TDAdata$PR <- NULL

TDAdata[which(!complete.cases(TDAdata))[1:5],]
TDAdata$tmax <- NULL
