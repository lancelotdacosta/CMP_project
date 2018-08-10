library(dplyr)
load("TDA18mean.RData")

Mapper <- function(data_frame,
                   data_name = "mapper data",
                   filter_values,
                   lens_name = "mapper lens",
                   num_intervals = 10,
                   percent_overlap = 50,
                   num_bins_when_clustering = 10){
  
  library(TDAmapper)
  library(fastcluster)
  
  dist_mx <- dist(data_frame)
  
  mapper_object <- mapper1D(distance_matrix = dist_mx,
                            filter_values = filter_values,
                            num_intervals = num_intervals,
                            percent_overlap = percent_overlap,
                            num_bins_when_clustering = num_bins_when_clustering)
  
  message(paste(c("Mapper with",lens_name, "."),collapse = " "))
  mapper_object
  
  Mapper_output <- list(mapper_object,           #1
                        data_frame,              #2
                        data_name,        #3
                        filter_values,           #4
                        lens_name,        #5
                        num_intervals,           #6
                        percent_overlap,         #7
                        num_bins_when_clustering)#8
  
  Mapper_output
}

TDA <- select(TDAdata, vent, pluie, tsec, hum, rgl, tmin, tmax, SWpotential, Age, Diameter, Height, daylength, Altitude, toCZ ,CZtoEZ,     
EZtoWZ, WZtoMZ )

Mapper_output <- Mapper(data_frame = TDA, data_name = "Fluxes + Meteo mean 18 days", filter_values = TDA$tsec, lens_name = "tsec")

save(Mapper_output, file = "Mapper18mean.RData")