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

Mapperplot <- function(Mapper_output){
  library(igraph)
  
  mapper_object <- Mapper_output[[1]]
  #data_frame <- Mapper_output[[2]]
  data_name <- Mapper_output[[3]]   
  #filter_values <- Mapper_output[[4]]   
  lens_name <-   Mapper_output[[5]]   
  num_intervals <- Mapper_output[[6]]   
  percent_overlap <- Mapper_output[[7]]   
  num_bins_when_clustering <- Mapper_output[[8]]  
  
  
  Mapper.graph <- graph.adjacency(mapper_object$adjacency, mode="undirected")
  
  #prepration of plot subtitle
  intervals <- paste("Intervals: ", as.character(num_intervals))
  overlap <- paste(c("overlap: ", as.character(percent_overlap), "%"), collapse = "")
  bins <- paste("bins w clustering: ", as.character(num_bins_when_clustering))
  
  plot(Mapper.graph, layout = layout.auto(Mapper.graph), sub = paste(c(intervals,overlap,bins), collapse = ", "))
  
  #plot title and text
  title(paste("Data: ", data_name)) #Data description
  mtext(paste("Lens: ", lens_name)) #Lens description
}



Mappercolor <- function(Mapper_output, color_lens = NULL, color_lens_name = NULL, method = "mean", bw = FALSE, size = TRUE){
  library(igraph)
  
  mapper_object <- Mapper_output[[1]]
  data_frame <- Mapper_output[[2]]
  data_name <- Mapper_output[[3]]   
  filter_values <- Mapper_output[[4]]   
  lens_name <-   Mapper_output[[5]]   
  num_intervals <- Mapper_output[[6]]   
  percent_overlap <- Mapper_output[[7]]   
  num_bins_when_clustering <- Mapper_output[[8]]  
  
  #Mapper graph with the vertices colored in function of color_lens and vertex size proportional to the number of points inside:
  Mapper.graph <- graph.adjacency(mapper_object$adjacency, mode="undirected")
  
  #color_lens
  if(is.null(color_lens) | is.null(color_lens_name)){
    message("Color lens replaced by filter function")
    color_lens <- filter_values
    message("Color lens description replaced by filter function description")
    color_lens_name <- lens_name
  }
  
  #Method (e.g mean) value of color_lens in each vertex:
  f <- get(method)
  
  method.vertex <- rep(0, mapper_object$num_vertices)
  for (i in 1:mapper_object$num_vertices){
    points.in.vertex <- mapper_object$points_in_vertex[[i]]
    method.vertex[i] <- f((color_lens[points.in.vertex]))
  }
  
  #Vertex size
  if(size == TRUE){
  
    vertex.size <- rep(0,mapper_object$num_vertices)
    points.in.vertex <- rep(0,mapper_object$num_vertices)
    
    for (i in 1:mapper_object$num_vertices){
      points.in.vertex[i] <- mapper_object$points_in_vertex[[i]]
    }
    
    for (i in 1:mapper_object$num_vertices){
      vertex.size[i] <- (points.in.vertex[i]-min(points.in.vertex))/(max(points.in.vertex)-min(points.in.vertex))/mapper_object$num_vertices*200
    }
      
    
    V(Mapper.graph)$size <- vertex.size
  }
  
  #Plot with color to each vertex
  intervals <- paste("Intervals: ", as.character(num_intervals))
  overlap <- paste(c("overlap: ", as.character(percent_overlap), "%"), collapse = "")
  bins <- paste("bins w clustering: ", as.character(num_bins_when_clustering))
  subtitle <-  paste(c(intervals,overlap,bins), collapse = ", ")
  
  
  if(bw == FALSE){
  
    vector_of_colors <- rainbow(floor(length(method.vertex)*4/3)+1)
    vector_of_colors <- vector_of_colors[1:length(method.vertex)]
    V(Mapper.graph)$color <- vector_of_colors[order(-method.vertex)]
    
    plot(Mapper.graph, sub=subtitle)
    legend(x=-2, y=-1,
           c(paste("high", color_lens_name), paste("medium", color_lens_name), paste("low", color_lens_name)),
           pch=21, col="#777777", pt.bg=rainbow(3), pt.cex=2, cex=.8, bty="n", ncol=1)
 
   }else{
     
     method.vertex.grey <- grey(1-(method.vertex - min(method.vertex))/(max(method.vertex) - min(method.vertex) ))
     V(Mapper.graph)$color <- method.vertex.grey

     plot(Mapper.graph, sub=subtitle)
     legend(x=-2, y=-1, c(paste("high", color_lens_name), paste("medium", color_lens_name), paste("low", color_lens_name)),pch=21,
            col="#777777", pt.bg=grey(c(0,0.5,1)), pt.cex=2, cex=.8, bty="n", ncol=1)
   }
  
  title(paste("Data: ", data_name)) #Data description
  mtext(paste("Lens: ", lens_name)) #Lens description
}

Mapperinteractive