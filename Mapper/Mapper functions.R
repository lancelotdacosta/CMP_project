Mapper <- function(distance_matrix,
                   data_description = "",
                   filter_values,
                   lens_description = "",
                   num_intervals = 10,
                   percent_overlap = 50,
                   num_bins_when_clustering = 10){
  
  library(TDAmapper)
  library(fastcluster)
  mapper_object <- mapper1D(distance_matrix = distance_matrix,
                            filter_values = filter_values,
                            num_intervals = num_intervals,
                            percent_overlap = percent_overlap,
                            num_bins_when_clustering = num_bins_when_clustering)
 
  message(paste(c("Mapper with",lens_description, ":"),collapse = " "))
  mapper_object
  
  output <- list(mapper_object,
                 data_description,
                 filter_values,
                 lens_description,
                 num_intervals,
                 percent_overlap,
                 num_bins_when_clustering)
  
  output
}

Mapperplot <- function(Mapper_output){
  library(igraph)
  Mapper.graph <- graph.adjacency(Mapper_output[[1]]$adjacency, mode="undirected")
  
  #prepration of plot subtitle
  i <- paste("Intervals: ", as.character(intervals))
  j <- paste("overlap: ", as.character(overlap))
  k <- paste("bins w clustering: ", as.character(bins))
  
  plot(Mapper.graph, layout = layout.auto(Mapper.graph), sub = paste(c(i,j,k), collapse = ", "))
  

  
  #plot of graph
  plot(g1, layout = layout.auto(g1), sub = paste(c(i,j,k), collapse = ", "))
  
  #plot title and text
  mtext(paste("Data: ", label[1])) #Data
  title(paste("Lens: ", label[2])) #Lens
  
}