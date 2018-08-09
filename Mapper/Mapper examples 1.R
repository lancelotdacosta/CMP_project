library(networkD3)
library(TDAmapper)
library(igraph)

First.Example.data = data.frame( x=1:1000, y=sin(1:1000) )
plot(First.Example.data$x,First.Example.data$y)

First.Example.dist = dist(First.Example.data)

First.Example.mapper <- mapper1D(distance_matrix = First.Example.dist,
                                 filter_values = First.Example.data$y,
                                 num_intervals = 20,
                                 percent_overlap = 50,
                                 num_bins_when_clustering = 2)

First.Example.graph <- graph.adjacency(First.Example.mapper$adjacency, mode="undirected")

plot(First.Example.graph, layout = layout.auto(First.Example.graph) )


#Mean value of First.Example.data$x in each vertex:

y.mean.vertex <- rep(0,First.Example.mapper$num_vertices)
for (i in 1:First.Example.mapper$num_vertices){
  points.in.vertex <- First.Example.mapper$points_in_vertex[[i]]
  y.mean.vertex[i] <-mean((First.Example.data$x[points.in.vertex]))
}

#Vertex size:

vertex.size <- rep(0,First.Example.mapper$num_vertices)
for (i in 1:First.Example.mapper$num_vertices){
  points.in.vertex <- First.Example.mapper$points_in_vertex[[i]]
  vertex.size[i] <- length((First.Example.mapper$points_in_vertex[[i]]))
}

#Mapper graph with the vertices colored in function of First.Example.data$y and vertex size proportional to the number of points inside:

y.mean.vertex.grey <- grey(1-(y.mean.vertex - min(y.mean.vertex))/(max(y.mean.vertex) - min(y.mean.vertex) ))
V(First.Example.graph)$color <- y.mean.vertex.grey
V(First.Example.graph)$size <- vertex.size

plot(First.Example.graph,main ="Mapper Graph")
legend(x=-2, y=-1, c("y small","y medium", "large y"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)


#Interactive
library(networkD3)
MapperNodes <- mapperVertices(First.Example.mapper, 1:2000 )
MapperLinks <- mapperEdges(First.Example.mapper)

forceNetwork(Nodes = MapperNodes, Links = MapperLinks, 
             Source = "Linksource", Target = "Linktarget",
             Value = "Linkvalue", NodeID = "Nodename",
             Group = "Nodegroup", opacity = 1, 
             linkDistance = -1, charge = -400) 

