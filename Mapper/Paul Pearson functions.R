mapperVertices <- function(m, pt_labels) {
  
  # Hovering over vertices gives the point labels:
  # convert the list of vectors of point indices to a list of vectors of labels
  labels_in_vertex <- lapply( m$points_in_vertex, FUN=function(v){ pt_labels[v] } )
  nodename <- sapply( sapply(labels_in_vertex, as.character), paste0, collapse=", ")
  nodename <- paste0("V", 1:m$num_vertices, ": ", nodename )
  
  # Hovering over vertices gives the point indices:
  # list the points in each vertex
  # nodename <- sapply( sapply(m$points_in_vertex, as.character), paste0, collapse=", ")
  # concatenate the vertex number with the labels for the points in each vertex
  #nodename <- paste0("V", 1:m$num_vertices, ": ", nodename )
  
  nodegroup <- m$level_of_vertex
  nodesize <- sapply(m$points_in_vertex, length)
  
  return(data.frame( Nodename=nodename, 
                     Nodegroup=nodegroup, 
                     Nodesize=nodesize ))
  
}

mapperEdges <- function(m) {
  linksource <- c()
  linktarget <- c()
  linkvalue <- c()
  k <- 1
  for (i in 2:m$num_vertices) {
    for (j in 1:(i-1)) {
      if (m$adjacency[i,j] == 1) {
        linksource[k] <- i-1
        linktarget[k] <- j-1
        linkvalue[k] <- 2
        k <- k+1
      }
    }
  }
  return( data.frame( Linksource=linksource,
                      Linktarget=linktarget, 
                      Linkvalue=linkvalue ) )
  
}
